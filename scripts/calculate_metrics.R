# Calculate dominance metrics for country categories
calculate_metrics <- function(model, topics, dfm, n = 3, min_group_size = 2) {
  
  start_time <- Sys.time()
  
  ## --- Extract components -----------------------------------------------------
  theta <- model$data$topic_proportions
  meta <- model$data$aligned_meta
  stm_meta <- dfm$data$meta
  stm_model <- model$data$model
  category_map <- model$data$category_map
  topics_table <- topics$data
  
  log_message("Processing category groupings for dominance analysis", "calculate_metrics")
  
  ## --- Initialize results -----------------------------------------------------
  results <- data.frame(
    category = character(), 
    subcategory = character(),
    documents = integer(),
    top_topics = character(),
    dominance = numeric(),
    significant = logical(),
    effect_size = numeric(),
    std_error = numeric(),
    stringsAsFactors = FALSE
  )
  
  ## --- Process each category --------------------------------------------------
  for (category_name in names(category_map)) {
    col_name <- category_map[[category_name]]
    
    if (!col_name %in% names(meta)) {
      log_message(paste("Column", col_name, "not found in metadata"), "calculate_metrics", "WARNING")
      next
    }
    
    # Get unique values 
    unique_values <- unique(meta[[col_name]])
    unique_values <- unique_values[!is.na(unique_values)]
    
    # Track values for category averages
    category_dominance <- c()
    category_effects <- c()
    category_docs <- c()
    
    ## --- Process individual subcategories -----------------------------------
    for (value in unique_values) {
      doc_indices <- which(meta[[col_name]] == value)
      
      # Skip groups with too few documents
      if (length(doc_indices) < min_group_size) {
        log_message(paste("Skipping", category_name, "->", value, "(only", length(doc_indices), "documents, minimum:", min_group_size, ")"), "calculate_metrics")
        next
      }
      
      if (length(doc_indices) > 0) {
        # Calculate dominance
        dominance_result <- find_dominance(theta, doc_indices, n)
        
        if (!is.null(dominance_result)) {
          top_indices <- dominance_result$corpus_level$top_indices
          top_topics <- get_topic_names(top_indices, topics_table)
          
          # Handle empty top_topics gracefully
          top_topics_text <- if(length(top_topics) > 0) paste(top_topics, collapse = ", ") else "No topics"
          
          # Test statistical significance
          significance_result <- find_variance(
            stm_model = stm_model,
            stm_meta = stm_meta,
            col_name = col_name,
            col_value = value,
            top_topics = top_indices
          )
          
          # Add individual result
          results <- rbind(results, data.frame(
            category = category_name,
            subcategory = as.character(value),
            documents = length(doc_indices),
            top_topics = top_topics_text,
            dominance = dominance_result$corpus_level$normalized,
            significant = if(!is.null(significance_result$significant)) as.logical(unname(significance_result$significant)) else FALSE,
            effect_size = if(!is.null(significance_result$effect_size)) as.numeric(unname(significance_result$effect_size)) else NA_real_,
            std_error = if(!is.null(significance_result$std_error)) as.numeric(unname(significance_result$std_error)) else NA_real_,
            stringsAsFactors = FALSE
          ))
          
          # Store values for category averaging
          category_dominance <- c(category_dominance, dominance_result$corpus_level$normalized)
          category_effects <- c(category_effects, if(!is.null(significance_result$effect_size)) significance_result$effect_size else NA_real_)
          category_docs <- c(category_docs, length(doc_indices))
        }
      }
    }
    
    ## --- Add category summary -----------------------------------------------
    if (length(category_dominance) > 0) {
      results <- rbind(results, data.frame(
        category = category_name,
        subcategory = "Overall",
        documents = sum(category_docs),
        top_topics = "Average across subcategories",
        dominance = mean(category_dominance),
        significant = NA,
        effect_size = mean(category_effects, na.rm = TRUE),
        std_error = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  ## --- Return result ----------------------------------------------------------
  if (nrow(results) == 0) {
    log_message("No results generated - check category_map", "calculate_metrics", "WARNING")
  }
  
  log_message(paste("Completed metrics calculation:", nrow(results), "results"), "calculate_metrics")
  
  return(create_result(
    data = results,
    metadata = list(
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      n_value = n,
      min_group_size = min_group_size
    )
  ))
}

## --- Helper function --------------------------------------------------------
get_topic_names <- function(topic_ids, topics_table) {
  sapply(topic_ids, function(idx) {
    topic_row <- which(topics_table$topic_id == idx)
    if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
  })
}