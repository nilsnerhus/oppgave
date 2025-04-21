# =========================================================================
# NAP Optimal topics function
# =========================================================================
# Purpose: Find the optimal k-value for the NAPs

optimal_topics <- function(processed_text, 
                           k_min = 10, 
                           k_max = 50, 
                           k_step = 10,
                           seed = 1234) {
  
  library(dplyr)
  library(tidytext)
  library(stm)
  library(ggplot2)
  library(tidyr)
  library(quanteda)
  
  stm_docs <- convert(docs_dfm, to = "stm")
  documents <- stm_docs$documents
  vocab <- stm_docs$vocab
  
  k_values <- seq(k_min, k_max, by = k_step)
  
  model_results <- data.frame(
    k = integer(),
    semantic_coherence = numeric(),
    exclusivity = numeric(),
    score = numeric()
  )
  
  set.seed(seed)
  
  cat("Testing models with different numbers of topics...\n")
  
  for (k in k_values) {
    cat(paste0("Fitting model with k = ", k, "...\n"))
    
    model_k <- tryCatch({
      stm(
        documents = documents,
        vocab = vocab,
        K = k,
        max.em.its = 50,
        init.type = "Spectral",
        data = meta,
        seed = seed,
        verbose = FALSE
      )
    }, error = function(e) {
      warning(paste("Model failed for k =", k, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(model_k)) {
      cat("Evaluating model...\n")
      
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
      
      model_results <- rbind(model_results, data.frame(
        k = k,
        semantic_coherence = semantic_coherence,
        exclusivity = exclusivity_score,
        score = semantic_coherence + exclusivity_score
      ))
    }
  }
  
  if (nrow(model_results) == 0) {
    stop("All models failed. Please check your input data.")
  }
  
  best_k <- model_results %>%
    arrange(desc(score)) %>%
    slice(1) %>%
    pull(k)
  
  plot <- model_results %>%
    pivot_longer(cols = c(semantic_coherence, exclusivity, held_out), 
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
  
  return(list(
    best_k = best_k,
    diagnostics = model_results,
    plot = plot,
    stm_data = list(documents = documents, vocab = vocab),
    doc_meta = meta
  ))
}
