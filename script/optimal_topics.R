# =========================================================================
# NAP Optimal topics function
# =========================================================================
# Purpose: Find the optimal k-value for topic modeling using NAP data

# Load required packages
library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(tidytext)    # Text processing
library(digest)      # Cache
library(stm)
library(ggplot2)
library(quanteda)

optimal_topics <- function(processed_data, 
                           k_min = 10, 
                           k_max = 50, 
                           k_step = 10,
                           final_iterations = 100,
                           seed = 1234,
                           use_cache = TRUE,
                           cache_dir = "results/cache/optimal_topics") {
  
  if (use_cache) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Get data hash from processed data if available
    if (inherits(processed_data, "nap_processed") && !is.null(attr(processed_data, "data_hash"))) {
      data_hash <- attr(processed_data, "data_hash")
    } else {
      # Generate new hash if needed
      data_hash <- digest(processed_data$dfm, algo = "md5")
    }
    
    # Create parameter hash
    param_hash <- digest(list(
      k_min = k_min,
      k_max = k_max,
      k_step = k_step,
      final_iterations = final_iterations,
      seed = seed
    ), algo = "md5")
    
    # Create cache path
    cache_id <- paste0(data_hash, "_", param_hash)
    cache_path <- file.path(cache_dir, paste0("optimal_topics_", cache_id, ".rds"))
    
    if (file.exists(cache_path)) {
      cat("Loading cached topic model selection results...\n")
      return(readRDS(cache_path))
    }
  }
  
  # Check if input has the expected format
  if (!inherits(processed_data, "nap_processed")) {
    stop("Input must be output from preprocess() function with class 'nap_processed'")
  }
  
  # Extract components from the processed data
  dfm_object <- processed_data$dfm
  meta <- processed_data$metadata
  
  cat("Converting document-feature matrix to STM format...\n")
  
  # Convert dfm to STM format
  stm_docs <- convert(dfm_object, to = "stm")
  documents <- stm_docs$documents
  vocab <- stm_docs$vocab
  
  # Define range of k values to test
  k_values <- seq(k_min, k_max, by = k_step)
  
  # Prepare results data frame and model storage
  model_results <- tibble(
    k = integer(),
    semantic_coherence = numeric(),
    exclusivity = numeric(),
    score = numeric()
  )
  
  # Store all models in a list
  all_models <- list()
  
  set.seed(seed)
  
  cat("Testing models with different numbers of topics...\n")
  
  for (k in k_values) {
    cat(paste0("Fitting model with k = ", k, "...\n"))
    
    model_k <- tryCatch({
      stm(
        documents = documents,
        vocab = vocab,
        K = k,
        max.em.its = 100, 
        init.type = "Spectral",
        data = meta,
        seed = seed,
        verbose = TRUE,
        )
    }, error = function(e) {
      warning(paste("Model failed for k =", k, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(model_k)) {
      cat("Evaluating model...\n")
      
      # Store model in list
      all_models[[as.character(k)]] <- model_k
      
      semantic_coherence <- tryCatch({
        mean(semanticCoherence(model_k, documents))
      }, error = function(e) {
        warning("Semantic coherence failed: ", e$message)
        NA
      })
      
      exclusivity_score <- tryCatch({
        mean(exclusivity(model_k))
      }, error = function(e) {
        warning("Exclusivity failed: ", e$message)
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
  
  # Get the best model
  best_model <- all_models[[as.character(best_k)]]
  
  # If requested, refine the best model with more iterations
  if (final_iterations > 50) {
    cat(paste0("Refining best model (k = ", best_k, ") with ", final_iterations, " iterations...\n"))
    
    best_model <- stm(
      documents = documents,
      vocab = vocab,
      K = best_k,
      max.em.its = final_iterations,
      init.type = "Spectral",
      data = meta,
      seed = seed,
      verbose = FALSE
    )
  }
  
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
  
  if (use_cache) {
    # Pass along the data hash
    attr(result, "data_hash") <- data_hash
    
    # Save results
    cat("Saving topic model selection results to cache...\n")
    saveRDS(result, cache_path)
  }
  
  # Return results as a structured object
  return(list(
    best_k = best_k,
    best_model = best_model,  # Now including the best model
    diagnostics = model_results,
    plot = plot,
    stm_data = list(documents = documents, vocab = vocab),
    doc_meta = meta
  ))
}