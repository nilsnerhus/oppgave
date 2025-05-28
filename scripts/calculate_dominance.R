#' @title Calculate Topic Dominance Across Categories
#' @description Calculates dominance metrics for all category/subcategory combinations
#'   defined in the category map. Uses rarefaction to standardize comparisons across
#'   groups of different sizes.
#'
#' @param model STM model result from fit_model()
#' @param topics Named topics result from name_topics()
#' @param n Number of top topics to consider (default: 3)
#' @param normalize Whether to apply rarefaction normalization (default: TRUE)
#' @param min_group_size Minimum group size to include in analysis (default: 8)
#'
#' @return A list containing dominance metrics for various categories
calculate_dominance <- function(model, topics, n = 3, normalize = TRUE, min_group_size = 8) {
  ## --- Setup & Initialization -------------------------------------------------
  log_message("Starting dominance calculation", "calculate_dominance")
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    category_stats = list(),
    excluded_groups = list()
  )
  
  ## --- Extract needed components ----------------------------------------------
  theta <- model$data$model$theta
  meta <- model$data$aligned_meta
  category_map <- model$data$category_map
  topics_table <- topics$data$topics_table
  
  ## --- Determine minimum group size for rarefaction if normalizing ------------
  if (normalize) {
    log_message("Determining minimum group size for rarefaction", "calculate_dominance")
    
    # Get all group sizes (only those meeting minimum threshold)
    group_sizes <- c()
    
    for (category_name in names(category_map)) {
      category_columns <- category_map[[category_name]]
      
      for (col_name in category_columns) {
        if (col_name %in% names(meta)) {
          if (is.logical(meta[[col_name]])) {
            size <- sum(meta[[col_name]] == TRUE, na.rm = TRUE)
            if (size >= min_group_size) {
              group_sizes <- c(group_sizes, size)
            }
          } else {
            unique_values <- unique(meta[[col_name]])
            unique_values <- unique_values[!is.na(unique_values)]
            for (value in unique_values) {
              size <- sum(meta[[col_name]] == value, na.rm = TRUE)
              if (size >= min_group_size) {
                group_sizes <- c(group_sizes, size)
              }
            }
          }
        }
      }
    }
    
    # Set minimum as rarefaction target
    if (length(group_sizes) > 0) {
      rarefaction_target <- min(group_sizes)
      log_message(paste("Using rarefaction target of", rarefaction_target, "documents"), "calculate_dominance")
      
      # Set as environment variable for find_dominance
      Sys.setenv(RAREFACTION_TARGET = rarefaction_target)
    } else {
      log_message("No groups meet minimum size requirement", "calculate_dominance", "WARNING")
      normalize <- FALSE
    }
  }
  
  # Initialize results dataframe with confidence interval columns
  results <- data.frame(
    level_type = character(),
    category = character(),
    subcategory = character(),
    documents = integer(),
    raw_dominance = numeric(),
    normalized_dominance = numeric(),
    variance = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    top_topics = character(),
    top_topic_ids = character(),
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
                       ci_lower = global_result$doc_level$ci_lower,
                       ci_upper = global_result$doc_level$ci_upper,
                       top_topics = top_topics_str,
                       top_topic_ids = top_topic_ids_str,
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
                       ci_lower = global_result$corpus_level$ci_lower,
                       ci_upper = global_result$corpus_level$ci_upper,
                       top_topics = top_topics_str,
                       top_topic_ids = top_topic_ids_str,
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
          
          # Format subcategory name
          if (grepl("^is_", col_name)) {
            subcategory <- toupper(gsub("^is_", "", col_name))
          } else {
            subcategory <- col_name
          }
          
          # Skip if too few documents
          if (length(doc_indices) < min_group_size) {
            excluded_msg <- paste(subcategory, "(n=", length(doc_indices), ")")
            diagnostics$excluded_groups[[category_name]] <- c(diagnostics$excluded_groups[[category_name]], excluded_msg)
            log_message(paste("Excluding", excluded_msg, "- below minimum group size"), "calculate_dominance")
            next
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
                             ci_lower = result$doc_level$ci_lower,
                             ci_upper = result$doc_level$ci_upper,
                             top_topics = top_topics_str,
                             top_topic_ids = top_topic_ids_str,
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
                             ci_lower = result$corpus_level$ci_lower,
                             ci_upper = result$corpus_level$ci_upper,
                             top_topics = top_topics_str,
                             top_topic_ids = top_topic_ids_str,
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
            if (length(doc_indices) < min_group_size) {
              excluded_msg <- paste(value, "(n=", length(doc_indices), ")")
              diagnostics$excluded_groups[[category_name]] <- c(diagnostics$excluded_groups[[category_name]], excluded_msg)
              log_message(paste("Excluding", excluded_msg, "- below minimum group size"), "calculate_dominance")
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
                               ci_lower = result$doc_level$ci_lower,
                               ci_upper = result$doc_level$ci_upper,
                               top_topics = top_topics_str,
                               top_topic_ids = top_topic_ids_str,
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
                               ci_lower = result$corpus_level$ci_lower,
                               ci_upper = result$corpus_level$ci_upper,
                               top_topics = top_topics_str,
                               top_topic_ids = top_topic_ids_str,
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
      subcategories_processed = subcategories_processed,
      min_group_size = min_group_size,
      rarefaction_target = if(exists("rarefaction_target")) rarefaction_target else NA
    )
  }
  
  ## --- Report excluded groups -------------------------------------------------
  if (length(diagnostics$excluded_groups) > 0) {
    log_message("Groups excluded from analysis due to small sample size:", "calculate_dominance")
    for (cat in names(diagnostics$excluded_groups)) {
      log_message(paste("  ", cat, ":", paste(diagnostics$excluded_groups[[cat]], collapse = ", ")), 
                  "calculate_dominance")
    }
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
      min_group_size = min_group_size,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}