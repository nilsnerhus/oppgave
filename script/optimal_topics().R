optimal_topics <- function(processed_text, 
                           k_min = 10, 
                           k_max = 50, 
                           k_step = 10,
                           seed = 1234) {
  
  # Gjenoppbygg tekst fra tokens
  cat("Reconstructing documents from tokens...\n")
  full_text <- processed_text %>%
    group_by(doc_id) %>%
    summarise(text = paste(word, collapse = " "), .groups = "drop")
  
  # Behold metadata (alle kolonner unntatt word)
  meta_cols <- setdiff(names(processed_text), c("word"))
  meta <- processed_text %>%
    select(all_of(meta_cols)) %>%
    distinct(doc_id, .keep_all = TRUE)
  
  # Kombiner tekst og metadata
  data_for_stm <- left_join(full_text, meta, by = "doc_id")
  
  # Preprosessering for STM
  cat("Passing data to STM preprocessing...\n")
  processed <- textProcessor(documents = data_for_stm$text, metadata = data_for_stm)
  prep <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  
  documents <- prep$documents
  vocab <- prep$vocab
  meta <- prep$meta
  
  # K-verdier
  k_values <- seq(k_min, k_max, by = k_step)
  
  model_results <- data.frame(
    k = integer(),
    semantic_coherence = numeric(),
    exclusivity = numeric(),
    held_out = numeric(),
    score = numeric()
  )
  
  set.seed(seed)
  
  cat("Testing models with different numbers of topics...\n")
  for (k in k_values) {
    cat(paste0("Fitting model with k = ", k, "...\n"))
    
    model_k <- stm(
      documents = documents,
      vocab = vocab,
      K = k,
      max.em.its = 50,
      init.type = "Spectral",
      data = meta,
      seed = seed,
      verbose = FALSE
    )
    
    heldout <- make.heldout(documents, vocab)
    heldout_likelihood <- eval.heldout(model_k, heldout)
    semantic_coherence <- mean(semanticCoherence(model_k, documents))
    exclusivity_score <- mean(exclusivity(model_k))
    
    model_results <- rbind(model_results, data.frame(
      k = k,
      semantic_coherence = semantic_coherence,
      exclusivity = exclusivity_score,
      held_out = heldout_likelihood,
      score = semantic_coherence + exclusivity_score
    ))
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
