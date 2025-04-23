# =========================================================================
# NAP Topic Modeling Function
# =========================================================================
# Purpose: Process topic model results for dissonance analysis

# Load required packages
library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(tidytext)    # Text processing
library(digest)      # Cache

topic_model <- function(optimal_result, 
                        final_k = NULL,
                        parallel = TRUE,
                        include_model = TRUE,
                        use_cache = TRUE,
                        cache_dir = "results/cache/topic_model"
                        ) {
  
  if (use_cache) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Get the model k-value
    k <- ifelse(is.null(final_k), optimal_result$best_k, final_k)
    
    # Get data hash from optimal result if available
    if (!is.null(attr(optimal_result, "data_hash"))) {
      data_hash <- attr(optimal_result, "data_hash")
    } else {
      # Generate hash from model data
      data_hash <- digest(optimal_result$stm_data, algo = "md5")
    }
    
    # Create parameter hash
    param_hash <- digest(list(
      k = k,
      include_model = include_model
    ), algo = "md5")
    
    # Create cache path
    cache_id <- paste0(data_hash, "_", param_hash)
    cache_path <- file.path(cache_dir, paste0("topic_model_", cache_id, ".rds"))
    
    if (file.exists(cache_path)) {
      cat("Loading cached topic model results for k =", k, "...\n")
      return(readRDS(cache_path))
    }
  }
  
  # Validate input
  if (!is.list(optimal_result) || !all(c("stm_data", "doc_meta", "best_k", "best_model") %in% names(optimal_result))) {
    stop("Invalid input: Expected output from updated optimal_topics() function")
  }
  
  # Extract data from optimization results
  stm_data <- optimal_result$stm_data
  doc_meta <- optimal_result$doc_meta
  
  # Use provided k or best k from optimization
  k <- ifelse(is.null(final_k), optimal_result$best_k, final_k)
  
  # Use the pre-computed model if k matches best_k, otherwise warn
  if (is.null(final_k) || final_k == optimal_result$best_k) {
    cat("Using pre-computed model with k =", optimal_result$best_k, "\n")
    final_model <- optimal_result$best_model
  } else {
    warning("Requested k differs from optimal k. Pre-computed model will not be used.")
    cat(paste0("Fitting new model with k = ", final_k, "...\n"))
    final_model <- stm(
      documents = stm_data$documents,
      vocab = stm_data$vocab,
      K = final_k,
      max.em.its = 100,
      init.type = "Spectral",
      seed = 1234,
      verbose = TRUE,
      cores = if(parallel) parallel::detectCores() - 1 else 1  # Correct parameter with value
      )
  }
  
  # Extract topic proportions (theta matrix)
  topic_props <- as.data.frame(final_model$theta)
  
  # Add document identifiers
  topic_props$doc_id <- rownames(topic_props)
  
  # Ensure doc_id is in the same format in both dataframes for joining
  if (is.numeric(doc_meta$doc_id) && !is.numeric(topic_props$doc_id)) {
    topic_props$doc_id <- as.numeric(topic_props$doc_id)
  } else if (is.character(doc_meta$doc_id) && !is.character(topic_props$doc_id)) {
    topic_props$doc_id <- as.character(topic_props$doc_id)
  }
  
  # Rename topic columns with simple numeric identifiers
  colnames(topic_props)[1:k] <- paste0("Topic_", 1:k)
  
  # Get available metadata columns
  meta_cols <- setdiff(names(doc_meta), "doc_id")
  
  if (length(meta_cols) == 0) {
    warning("No metadata columns found beyond doc_id")
  }
  
  # Reshape to long format
  topic_props_long <- topic_props %>%
    pivot_longer(
      cols = starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  # Join with document metadata, preserving all available metadata columns
  result_df <- topic_props_long %>%
    left_join(doc_meta, by = "doc_id")
  
  # Extract topic labels for reference
  top_terms <- labelTopics(final_model, n = 5)
  topic_labels <- sapply(1:k, function(i) {
    paste(top_terms$frex[i,], collapse = ", ")
  })
  names(topic_labels) <- paste0("Topic_", 1:k)
  
  # Prepare final output
  output <- list(
    data = result_df,
    topic_labels = topic_labels
  )
  
  # Include model if requested
  if (include_model) {
    output$model <- final_model
  }
  
  # Add class for method dispatch
  class(output) <- c("nap_topic_model", "list")
  
  # Provide guidance on using the output
  cat("Model processing complete. Access results with:\n")
  cat("- output$data: Data frame with topics and proportions\n")
  cat("- output$topic_labels: Descriptive labels for each topic\n")
  if (include_model) {
    cat("- output$model: Full STM model object\n")
  }
  
  if (use_cache) {
    # Save results
    cat("Saving topic model results to cache...\n")
    saveRDS(output, cache_path)
  }
  
  return(output)
}