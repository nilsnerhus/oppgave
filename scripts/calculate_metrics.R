# Calculate dominance metrics for country categories
calculate_metrics <- function(model, topics, dfm, n = 3) {
  
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
    test_type = character(),
    documents = integer(),
    dominance = numeric(),
    top_topics = character(),
    effect_size = numeric(),
    std_error = numeric(),
    significant = logical(),
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
      
      if (length(doc_indices) > 0) {
        # Calculate dominance
        dominance_result <- find_dominance(theta, doc_indices, n)
        top_indices <- dominance_result$corpus_level$top_indices
        top_topics <- get_topic_names(top_indices, topics_table)
        
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
          test_type = "Individual",
          documents = length(doc_indices),
          dominance = dominance_result$corpus_level$normalized,
          top_topics = paste(top_topics, collapse = ", "),
          effect_size = significance_result$effect_size,
          std_error = significance_result$std_error,
          significant = significance_result$significant,
          stringsAsFactors = FALSE
        ))
        
        # Store values for category averaging
        category_dominance <- c(category_dominance, dominance_result$corpus_level$normalized)
        category_effects <- c(category_effects, significance_result$effect_size)
        category_docs <- c(category_docs, length(doc_indices))
      }
    }
    
    ## --- Add category summary -----------------------------------------------
    if (length(category_dominance) > 0) {
      results <- rbind(results, data.frame(
        category = category_name,
        subcategory = "Overall",
        test_type = "Average",
        documents = sum(category_docs),
        dominance = mean(category_dominance),
        top_topics = "Average across subcategories",
        effect_size = mean(category_effects, na.rm = TRUE),
        std_error = NA_real_,
        significant = NA,
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
      n_value = n
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