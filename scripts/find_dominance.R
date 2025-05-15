#' @title Calculate dominance across all documents and dimensions with top topics
#' @description Calculates discourse dominance at the document level for all documents,
#'   identifies the top topics for each group, and prepares data for reporting.
#'   
#' @param model The output from fit_model() containing topic data
#' @param n Value for top-n topics to consider (default: 3)
#' @param topic_names Optional data frame with topic_id, topic_name, and short_name
#' @param output_path Path to save results (default: "data/dominance_analysis.rds")
#'
#' @return A list containing dominance values and top topics by dimension
find_all_dominance <- function(
    model,
    n = 3,
    topic_names = NULL,
    output_path = "data/dominance_analysis.rds"
) {
  ## --- Initialization ------------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character()
  )
  
  ## --- Input validation and data extraction --------------------------------
  log_message("Validating inputs", "find_all_dominance")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model)) {
    stop("model must be the output from fit_model() with a 'data' component")
  }
  
  # Extract topic_data from model
  if (!"topic_data" %in% names(model$data)) {
    stop("model$data must contain a 'topic_data' component")
  }
  
  # Get the topic data in long format
  topic_data <- model$data$topic_data
  
  # Get topic proportions in wide format
  topic_props <- model$data$topic_proportions
  
  # Validate structure
  required_cols <- c("Topic", "Proportion", "doc_id")
  missing_cols <- setdiff(required_cols, names(topic_data))
  if (length(missing_cols) > 0) {
    stop("Topic data missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate n value
  if (!is.numeric(n) || n < 1) {
    stop("n must be a positive integer")
  }
  
  ## --- Get unique document IDs ---------------------------------------------
  log_message("Identifying unique documents", "find_all_dominance")
  
  # Get list of unique document IDs
  unique_docs <- unique(topic_data$doc_id)
  doc_count <- length(unique_docs)
  
  log_message(paste("Found", doc_count, "unique documents to process"), "find_all_dominance")
  
  ## --- Extract document metadata -------------------------------------------
  log_message("Extracting document metadata", "find_all_dominance")
  
  # Get non-topic columns from topic_data
  meta_cols <- setdiff(names(topic_data), c("Topic", "Proportion"))
  
  # Get one row per document with all metadata
  doc_metadata <- topic_data %>%
    dplyr::select(dplyr::all_of(meta_cols)) %>%
    dplyr::distinct(doc_id, .keep_all = TRUE)
  
  ## --- Calculate dominance values ------------------------------------------
  log_message(paste("Calculating dominance with n =", n), "find_all_dominance")
  
  # Initialize data frame for results
  results <- data.frame(
    doc_id = unique_docs,
    dominance = numeric(doc_count),
    stringsAsFactors = FALSE
  )
  names(results)[2] <- paste0("dominance_n", n)
  
  # Process each document
  for (i in 1:doc_count) {
    doc_id <- unique_docs[i]
    
    # Log progress periodically
    if (i %% 10 == 0 || i == 1 || i == doc_count) {
      log_message(paste("Document", i, "of", doc_count), "find_all_dominance")
    }
    
    # Calculate dominance for this document
    doc_result <- find_dominance(
      data = topic_data,
      value_col = "Proportion",
      doc_id_col = "doc_id",
      n = n,
      filter_col = "doc_id",
      filter_value = doc_id
    )
    
    # Store the result
    results[i, paste0("dominance_n", n)] <- doc_result$raw_dominance
  }
  
  # Join with metadata
  full_results <- dplyr::left_join(results, doc_metadata, by = "doc_id")
  
  ## --- Identify top topics by category -------------------------------------
  log_message("Identifying top topics by category", "find_all_dominance")
  
  # Define category map
  category_map <- list(
    Income = "wb_income_level", 
    Region = "region", 
    Geography = c("is_sids", "is_lldc", "is_ldc")
  )
  
  # Initialize list for top topics
  top_topics <- list()
  
  # Helper function to identify top topics for a group
  identify_top_topics <- function(data, n_topics = n) {
    # Get topic columns
    topic_cols <- grep("^Topic_", names(data), value = TRUE)
    
    # Calculate average proportion for each topic
    topic_means <- colMeans(data[, topic_cols, drop = FALSE], na.rm = TRUE)
    
    # Sort and get top n
    top_indices <- order(topic_means, decreasing = TRUE)[1:min(n_topics, length(topic_means))]
    top_topic_ids <- as.numeric(gsub("Topic_", "", topic_cols[top_indices]))
    top_proportions <- topic_means[top_indices]
    
    # Create result data frame
    result <- data.frame(
      topic_id = top_topic_ids,
      proportion = top_proportions,
      stringsAsFactors = FALSE
    )
    
    # Add names if provided
    if (!is.null(topic_names)) {
      result <- merge(result, topic_names[, c("topic_id", "topic_name", "short_name")], by = "topic_id")
    }
    
    # Sort by proportion
    result <- result[order(result$proportion, decreasing = TRUE), ]
    
    return(result)
  }
  
  # Get overall top topics
  top_topics$Overall <- identify_top_topics(topic_props)
  
  # Process each category
  for (category_name in names(category_map)) {
    dimensions <- category_map[[category_name]]
    
    # Skip empty categories
    if (length(dimensions) == 0) next
    
    # Add category entry
    top_topics[[category_name]] <- list()
    
    # Process differently based on category type
    if (category_name %in% c("Income", "Region")) {
      dim <- dimensions[1]
      
      # Skip if dimension not in data
      if (!dim %in% names(topic_props)) {
        warning(paste("Dimension", dim, "not found in topic data"))
        next
      }
      
      # Get unique values
      values <- unique(topic_props[[dim]])
      values <- values[!is.na(values)]
      
      # Process each value
      for (val in values) {
        # Filter data
        group_data <- topic_props[topic_props[[dim]] == val, ]
        
        # Skip if too few documents
        if (nrow(group_data) < 3) next
        
        # Get top topics for this group
        top_topics[[category_name]][[as.character(val)]] <- identify_top_topics(group_data)
      }
      
    } else if (category_name == "Geography") {
      # For binary dimensions (multiple columns)
      for (dim in dimensions) {
        # Skip if dimension not in data
        if (!dim %in% names(topic_props)) {
          warning(paste("Dimension", dim, "not found in topic data"))
          next
        }
        
        # Format dimension name
        dim_name <- switch(dim,
                           "is_sids" = "SIDS",
                           "is_lldc" = "LLDC",
                           "is_ldc" = "LDC",
                           gsub("is_", "", dim))
        
        # Only process TRUE values
        # Ensure binary format
        if (!is.logical(topic_props[[dim]])) {
          if (is.character(topic_props[[dim]]) || is.factor(topic_props[[dim]])) {
            topic_props[[dim]] <- tolower(as.character(topic_props[[dim]])) %in% c("true", "yes", "1", "t")
          } else if (is.numeric(topic_props[[dim]])) {
            topic_props[[dim]] <- topic_props[[dim]] > 0
          }
        }
        
        # Filter data
        group_data <- topic_props[topic_props[[dim]] == TRUE, ]
        
        # Skip if too few documents
        if (nrow(group_data) < 3) next
        
        # Get top topics for this group
        top_topics[[category_name]][[dim_name]] <- identify_top_topics(group_data)
      }
    }
  }
  
  ## --- Finalize and return results -----------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    total_documents = doc_count,
    n_value = n,
    model_metadata = model$metadata
  )
  
  # Create final result
  final_result <- list(
    data = full_results,
    top_topics = top_topics,
    metadata = metadata,
    diagnostics = diagnostics
  )
  
  log_message(paste("Document dominance analysis complete for", doc_count, 
                    "documents with n =", n), 
              "find_all_dominance")
  
  # Save to output path if provided
  if (!is.null(output_path)) {
    saveRDS(final_result, output_path)
    log_message(paste("Results saved to", output_path), "find_all_dominance")
  }
  
  return(final_result)
}