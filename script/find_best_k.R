# Purpose: Find the optimal k-value for topic modeling

library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(stm)         # Structural Topic Models
library(ggplot2)     # Visualization

find_best_k <- function(input,
                           k_min = 20, 
                           k_max = 120, 
                           k_step = 5,
                           seed = 1234,
                           output_path = "data/optimal_topics.rds") {
  
  # Create output directory if it doesn't exist
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  # Check if input has the expected format (from preprocess function)
  if (!is.list(input) || !all(c("dfm", "metadata", "stm_data") %in% names(input))) {
    stop("Input must be output from preprocess() function")
  }
  
  # Extract components from the input
  stm_data <- input$stm_data
  meta <- input$metadata
  
  # Define range of k values to test
  k_values <- seq(k_min, k_max, by = k_step)
  
  # Prepare results data frame
  model_results <- tibble(
    k = integer(),
    semantic_coherence = numeric(),
    exclusivity = numeric(),
    score = numeric()
  )
  
  set.seed(seed)
  
  cat("Testing models with different numbers of topics...\n")
  
  # Test each k value
  for (k in k_values) {
    cat(paste0("Fitting model with k = ", k, "...\n"))
    
    model_k <- tryCatch({
      stm(
        documents = stm_data$documents,
        vocab = stm_data$vocab,
        K = k,
        max.em.its = 75,  # Moderate number of iterations for testing
        init.type = "Spectral",
        seed = seed,
      )
    }, error = function(e) {
      warning(paste("Model failed for k =", k, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(model_k)) {
      cat("Evaluating model quality...\n")
      
      # Calculate diagnostic metrics
      semantic_coherence <- tryCatch({
        mean(semanticCoherence(model_k, stm_data$documents))
      }, error = function(e) {
        warning("Semantic coherence calculation failed: ", e$message)
        NA
      })
      
      exclusivity_score <- tryCatch({
        mean(exclusivity(model_k))
      }, error = function(e) {
        warning("Exclusivity calculation failed: ", e$message)
        NA
      })
      
      # Combined score (we want high coherence and exclusivity)
      combined_score <- semantic_coherence + exclusivity_score
      
      model_results <- rbind(model_results, data.frame(
        k = k,
        semantic_coherence = semantic_coherence,
        exclusivity = exclusivity_score,
        score = combined_score
      ))
    }
  }
  
  if (nrow(model_results) == 0) {
    stop("All models failed. Please check your input data.")
  }
  
  # Find best k based on combined score
  best_model_info <- model_results %>%
    arrange(desc(score)) %>%
    slice(1)
  
  best_k <- best_model_info$k
  
  cat(paste0("Best k value identified: ", best_k, "\n"))
  
  # Fit final model with best k value
  cat(paste0("Fitting final model with k = ", best_k, "...\n"))
  
  best_model <- stm(
    documents = stm_data$documents,
    vocab = stm_data$vocab,
    K = best_k,
    max.em.its = 100,  # More iterations for final model
    init.type = "Spectral",
    seed = seed,
  )
  
  # Create diagnostic plot
  plot <- model_results %>%
    pivot_longer(cols = c(semantic_coherence, exclusivity), 
                 names_to = "metric", 
                 values_to = "value") %>%
    ggplot(aes(x = k, y = value, color = metric)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = best_k, linetype = "dashed") +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = "Topic Model Diagnostics",
      subtitle = paste("Optimal number of topics:", best_k),
      x = "Number of Topics (k)",
      y = "Value"
    ) +
    theme_minimal()
  
  # Prepare results
  result <- list(
    best_k = best_k,
    best_model = best_model,
    diagnostics = model_results,
    plot = plot,
    metadata = meta,
    stm_data = list(vocab = stm_data$vocab, documents = stm_data$documents)
  )
  
  # Save results
  cat("Saving optimal model to", output_path, "...\n")
  saveRDS(result, output_path)
  
  cat("Optimal topic selection complete!\n")
  return(result)
}