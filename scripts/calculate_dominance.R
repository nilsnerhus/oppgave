#' @title Calculate Topic Dominance Across Categories
#' @description Calculates dominance metrics for all category/subcategory combinations
#'   defined in the category map.
#'
#' @param model STM model result from fit_model()
#' @param topics Named topics result from name_topics()
#' @param n Number of top topics to consider (default: 3)
#' @param normalize Whether to normalize dominance values (default: TRUE)
#'
#' @return A list containing dominance metrics for various categories
calculate_dominance <- function(model, topics, n = 3, normalize = TRUE) {
  ## --- Setup & Initialization -------------------------------------------------
  log_message("Starting dominance calculation", "calculate_dominance")
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    category_stats = list()
  )
  
  ## --- Extract needed components ----------------------------------------------
  theta <- model$data$model$theta
  meta <- model$data$aligned_meta
  category_map <- model$data$category_map
  topics_table <- topics$data$topics_table
  
  # Initialize results dataframe with the new column
  results <- data.frame(
    level_type = character(),
    category = character(),
    subcategory = character(),
    documents = integer(),
    raw_dominance = numeric(),
    normalized_dominance = numeric(),
    variance = numeric(),
    top_topics = character(),
    top_topic_ids = character(),  # Added this column
    stringsAsFactors = FALSE
  )
  
  ## --- Global metrics ---------------------------------------------------------
  log_message("Calculating global dominance metrics", "calculate_dominance")
  all_docs <- 1:nrow(theta)
  global_result <- find_dominance(theta, all_docs, n, normalize)
  
  # Check global result
  if (is.null(global_result)) {
    warning_msg <- "Global dominance calculation failed"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "calculate_dominance", "WARNING")
  } else {
    # Get top topic names
    top_indices <- global_result$corpus_level$top_indices
    top_topics <- sapply(top_indices, function(idx) {
      topic_row <- which(topics_table$topic_id == idx)
      if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
    })
    top_topics_str <- paste(top_topics, collapse = ", ")
    
    # Add the topic IDs as a separate field
    top_topic_ids_str <- paste(top_indices, collapse = ",")
    
    # Add to results
    results <- rbind(results,
                     data.frame(
                       level_type = "document",
                       category = "Overall",
                       subcategory = "Overall",
                       documents = length(all_docs),
                       raw_dominance = global_result$doc_level$raw,
                       normalized_dominance = global_result$doc_level$normalized,
                       variance = global_result$doc_level$variance,
                       top_topics = top_topics_str,
                       top_topic_ids = top_topic_ids_str,  # New field
                       stringsAsFactors = FALSE
                     ),
                     data.frame(
                       level_type = "corpus",
                       category = "Overall",
                       subcategory = "Overall",
                       documents = length(all_docs),
                       raw_dominance = global_result$corpus_level$raw,
                       normalized_dominance = global_result$corpus_level$normalized,
                       variance = global_result$corpus_level$variance,
                       top_topics = top_topics_str,
                       top_topic_ids = top_topic_ids_str,  # New field
                       stringsAsFactors = FALSE
                     ))
  }
  
  ## --- Process categories -----------------------------------------------------
  if (!is.null(category_map) && length(category_map) > 0) {
    log_message(paste("Processing", length(names(category_map)), "categories"), "calculate_dominance")
    
    # Track category statistics
    categories_processed <- 0
    subcategories_processed <- 0
    
    # Go through each category (e.g., Income, Region)
    for (category_name in names(category_map)) {
      category_columns <- category_map[[category_name]]
      
      # Skip empty columns
      if (length(category_columns) == 0) next
      
      categories_processed <- categories_processed + 1
      
      # Process each column
      for (col_name in category_columns) {
        # Skip if column doesn't exist
        if (!col_name %in% names(meta)) {
          warning_msg <- paste("Column", col_name, "not found in metadata")
          diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
          log_message(warning_msg, "calculate_dominance", "WARNING")
          next
        }
        
        if (is.logical(meta[[col_name]])) {
          ## --- Binary column processing ---------------------------------------
          doc_indices <- which(meta[[col_name]] == TRUE)
          
          # Skip if too few documents
          if (length(doc_indices) < 3) {
            next
          }
          
          # Format subcategory name
          if (grepl("^is_", col_name)) {
            subcategory <- toupper(gsub("^is_", "", col_name))
          } else {
            subcategory <- col_name
          }
          
          # Calculate dominance
          result <- find_dominance(theta, doc_indices, n, normalize)
          
          if (is.null(result)) {
            warning_msg <- paste("Dominance calculation failed for", subcategory)
            diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
            log_message(warning_msg, "calculate_dominance", "WARNING")
            next
          }
          
          # Get top topics
          top_indices <- result$corpus_level$top_indices
          top_topics <- sapply(top_indices, function(idx) {
            topic_row <- which(topics_table$topic_id == idx)
            if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
          })
          top_topics_str <- paste(top_topics, collapse = ", ")
          
          # Add the topic IDs as a separate field
          top_topic_ids_str <- paste(top_indices, collapse = ",")
          
          # Add to results
          results <- rbind(results,
                           data.frame(
                             level_type = "document",
                             category = category_name,
                             subcategory = subcategory,
                             documents = length(doc_indices),
                             raw_dominance = result$doc_level$raw,
                             normalized_dominance = result$doc_level$normalized,
                             variance = result$doc_level$variance,
                             top_topics = top_topics_str,
                             top_topic_ids = top_topic_ids_str,  # New field
                             stringsAsFactors = FALSE
                           ),
                           data.frame(
                             level_type = "corpus",
                             category = category_name,
                             subcategory = subcategory,
                             documents = length(doc_indices),
                             raw_dominance = result$corpus_level$raw,
                             normalized_dominance = result$corpus_level$normalized,
                             variance = result$corpus_level$variance,
                             top_topics = top_topics_str,
                             top_topic_ids = top_topic_ids_str,  # New field
                             stringsAsFactors = FALSE
                           ))
          
          subcategories_processed <- subcategories_processed + 1
          
        } else {
          ## --- Categorical column processing ---------------------------------
          # Get unique values
          unique_values <- unique(meta[[col_name]])
          unique_values <- unique_values[!is.na(unique_values)]
          
          # Process each value
          for (value in unique_values) {
            doc_indices <- which(meta[[col_name]] == value)
            
            # Skip if too few documents
            if (length(doc_indices) < 3) {
              next
            }
            
            # Calculate dominance
            result <- find_dominance(theta, doc_indices, n, normalize)
            
            if (is.null(result)) {
              warning_msg <- paste("Dominance calculation failed for", value)
              diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
              log_message(warning_msg, "calculate_dominance", "WARNING")
              next
            }
            
            # Get top topics
            top_indices <- result$corpus_level$top_indices
            top_topics <- sapply(top_indices, function(idx) {
              topic_row <- which(topics_table$topic_id == idx)
              if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
            })
            top_topics_str <- paste(top_topics, collapse = ", ")
            
            # Add the topic IDs as a separate field
            top_topic_ids_str <- paste(top_indices, collapse = ",")
            
            # Add to results
            results <- rbind(results,
                             data.frame(
                               level_type = "document",
                               category = category_name,
                               subcategory = as.character(value),
                               documents = length(doc_indices),
                               raw_dominance = result$doc_level$raw,
                               normalized_dominance = result$doc_level$normalized,
                               variance = result$doc_level$variance,
                               top_topics = top_topics_str,
                               top_topic_ids = top_topic_ids_str,  # New field
                               stringsAsFactors = FALSE
                             ),
                             data.frame(
                               level_type = "corpus",
                               category = category_name,
                               subcategory = as.character(value),
                               documents = length(doc_indices),
                               raw_dominance = result$corpus_level$raw,
                               normalized_dominance = result$corpus_level$normalized,
                               variance = result$corpus_level$variance,
                               top_topics = top_topics_str,
                               top_topic_ids = top_topic_ids_str,  # New field
                               stringsAsFactors = FALSE
                             ))
            
            subcategories_processed <- subcategories_processed + 1
          }
        }
      }
    }
    
    # Update diagnostics with stats
    diagnostics$category_stats <- list(
      categories_processed = categories_processed,
      subcategories_processed = subcategories_processed
    )
  }
  
  ## --- Finalize and return results -------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_message(paste("Dominance calculation complete,", nrow(results)/2, "entries processed"), 
              "calculate_dominance")
  
  # Return results
  return(create_result(
    data = results,
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      n_value = n,
      normalize = normalize,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}