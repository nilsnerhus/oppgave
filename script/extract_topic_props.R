# Purpose: Process topic model results for analysis

library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(stm)         # Structural Topic Models

extract_topic_props <- function(input, 
                        k = NULL,
                        output_path = "data/topic_props.rds") {
  
  # Create output directory if it doesn't exist
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  # Determine input type and extract necessary components
  if (is.list(input) && "best_model" %in% names(input)) {
    # Input is from optimal_topics
    cat("Using best model from optimal_topics output\n")
    model <- input$best_model
    metadata <- input$metadata
    
    # Use provided k or best k from optimization
    if (is.null(k)) {
      k <- input$best_k
      cat("Using optimal k value:", k, "\n")
    } else if (k != input$best_k) {
      warning("Provided k value differs from optimal k. Creating new model.")
      cat("Fitting new model with k =", k, "...\n")
      
      # Use stm_data from input if available
      if ("stm_data" %in% names(input) && 
          all(c("documents", "vocab") %in% names(input$stm_data))) {
        model <- stm(
          documents = input$stm_data$documents,
          vocab = input$stm_data$vocab,
          K = k,
          max.em.its = 100,
          init.type = "Spectral",
          seed = 1234,
        )
      } else {
        stop("Cannot create new model with different k: stm_data not available in input")
      }
    }
  } else if (is.list(input) && all(c("dfm", "metadata", "stm_data", "corpus") %in% names(input))) {
    # Input is from preprocess
    cat("Creating model from preprocessed data\n")
    
    # Extract necessary components
    stm_data <- input$stm_data
    metadata <- input$metadata
    
    # Require explicit k value
    if (is.null(k)) {
      stop("When using preprocessed data directly, k must be explicitly specified")
    } else {
      cat("Using provided k value:", k, "\n")
    }
    
    # Create model
    cat("Fitting model...\n")
    model <- stm(
      documents = stm_data$documents,
      vocab = stm_data$vocab,
      K = k,
      max.em.its = 100,
      init.type = "Spectral",
      seed = 1234,
    )
  } else {
    stop("Invalid input: Expected output from preprocess() or optimal_topics() functions")
  }
  
  # Extract topic proportions (theta matrix)
  cat("Extracting topic proportions...\n")
  topic_props <- as.data.frame(model$theta)
  
  # Add document identifiers
  topic_props$doc_id <- rownames(topic_props)
  
  # Ensure doc_id types match - convert to the same type as in metadata
  if (is.numeric(metadata$doc_id) && !is.numeric(topic_props$doc_id)) {
    topic_props$doc_id <- as.numeric(topic_props$doc_id)
  } else if (is.character(metadata$doc_id) && !is.character(topic_props$doc_id)) {
    topic_props$doc_id <- as.character(topic_props$doc_id)
  }
  
  # Rename topic columns with simple numeric identifiers
  colnames(topic_props)[1:k] <- paste0("Topic_", 1:k)
  
  # Rename topic columns with simple numeric identifiers
  colnames(topic_props)[1:k] <- paste0("Topic_", 1:k)
  
  # Reshape to long format
  topic_props_long <- topic_props %>%
    pivot_longer(
      cols = starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  # Join with document metadata
  result_df <- topic_props_long %>%
    left_join(metadata, by = "doc_id")
  
  # Extract topic labels for reference
  cat("Generating topic labels...\n")
  top_terms <- labelTopics(model, n = 5)
  topic_labels <- sapply(1:k, function(i) {
    paste(top_terms$frex[i,], collapse = ", ")
  })
  names(topic_labels) <- paste0("Topic_", 1:k)
  
  # Prepare final output
  result <- list(
    data = result_df,
    topic_labels = topic_labels,
    model = model
  )
  
  # Save results
  cat("Saving topic model results to", output_path, "...\n")
  saveRDS(result, output_path)
  
  cat("Topic modeling complete!\n")
  return(result)
}