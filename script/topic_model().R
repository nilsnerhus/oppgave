
# Load required packages
library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(tidytext)    # Text processing
library(stm)         # Structural Topic Modeling

topic_model <- function(optimal_result, 
                                      final_k = NULL,
                                      include_top_terms = FALSE) {
  
  # Extract data from optimization results
  stm_data <- optimal_result$stm_data
  doc_meta <- optimal_result$doc_meta
  
  # Use provided k or best k from optimization
  k <- ifelse(is.null(final_k), optimal_result$best_k, final_k)
  
  # Fit final STM model
  cat(paste0("Fitting final model with k = ", k, "...\n"))
  final_model <- stm(
    documents = stm_data$documents,
    vocab = stm_data$vocab,
    K = k,
    max.em.its = 100,
    init.type = "Spectral",
    seed = 1234,
    verbose = FALSE
  )
  
  # Extract topic proportions (theta matrix)
  topic_props <- as.data.frame(final_model$theta)
  
  # Add document identifiers
  topic_props$doc_id <- as.integer(rownames(topic_props))
  
  # Rename topic columns with simple numeric identifiers
  colnames(topic_props)[1:k] <- paste0("Topic_", 1:k)
  
  # Store top terms for reference but don't use in labels
  top_terms <- labelTopics(final_model, n = 5)
  topic_labels <- sapply(1:k, function(i) {
    paste(top_terms$frex[i,], collapse = ", ")
  })
  
  # Reshape to long format
  topic_props_long <- topic_props %>%
    pivot_longer(
      cols = starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  # Join with document metadata
  result_df <- topic_props_long %>%
    left_join(doc_meta, by = "doc_id")
  
  # Select and rearrange columns for final output
  final_df <- result_df %>%
    select(date_posted, Country, Region, Income, Geography, Topic, Proportion)
  
  # Return the formatted data
  return(list(
    data = final_df,
    model = final_model,
    topic_labels = setNames(topic_labels, paste0("Topic_", 1:k))  # Store but don't use in main data
  ))
}

