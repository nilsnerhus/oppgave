fit_model <- function(dfm, k, category_map = NULL, iterations = 200, seed = 12345) {  
  
  start_time <- Sys.time()
  set.seed(seed)
  
  ## --- Validation & Setup -----------------------------------------------------
  if (!is.numeric(k) || k <= 0 || k != round(k)) {
    warning("Invalid k value, using default k = 8")
    k <- 8
  }
  
  log_message(paste("Fitting STM with k =", k), "fit_model")
  
  if (!all(c("documents", "vocab", "meta") %in% names(dfm$data))) {
    stop("dfm must be from process_dfm() with documents, vocab, and meta")
  }
  
  docs <- dfm$data$documents
  vocab <- dfm$data$vocab
  meta <- dfm$data$meta
  
  log_message(paste("Fitting STM: k =", k, ",", length(docs), "documents,", 
                    length(vocab), "terms"), "fit_model")
  
  ## --- Build prevalence formula -----------------------------------------------
  prevalence_formula <- build_prevalence_formula(category_map, meta)
  
  ## --- Fit STM model ----------------------------------------------------------
  model_result <- tryCatch({
    stm::stm(
      documents = docs,
      vocab = vocab, 
      K = k,
      data = meta,
      prevalence = prevalence_formula,
      max.em.its = iterations,
      verbose = FALSE
    )
  }, error = function(e) {
    stop("STM fitting failed: ", e$message)
  })
  
  ## --- Handle segmentation (if used) ------------------------------------------
  segmentation_info <- dfm$metadata$segmentation
  used_segmentation <- !is.null(segmentation_info) && segmentation_info$used_segmentation
  
  if (used_segmentation) {
    log_message("Aggregating segments to documents", "fit_model")
    
    final_theta <- aggregate_by_document(
      theta = model_result$theta,
      doc_ids = meta$doc_id,
      segment_map = segmentation_info$segment_to_doc_map
    )
    
    # Create document-level metadata
    final_meta <- get_document_metadata(meta, segmentation_info$segment_to_doc_map)
    
    log_message(paste("Aggregated", nrow(model_result$theta), "segments to", 
                      nrow(final_theta), "documents"), "fit_model")
  } else {
    final_theta <- model_result$theta
    final_meta <- meta
  }
  
  ## --- Display FREX terms -----------------------------------------------------
  log_message("Extracting FREX terms for manual naming", "fit_model")
  
  topic_labels <- stm::labelTopics(model_result, n = 10)
  frex_terms <- list()
  
  cat("\n=== FREX TERMS FOR MANUAL NAMING ===\n")
  for (i in 1:k) {
    terms <- topic_labels$frex[i, 1:5]
    cat("Topic", i, ":", paste(terms, collapse = ", "), "\n")
    frex_terms[[i]] <- topic_labels$frex[i, ]
  }
  cat("=========================================\n")
  cat("Create: topics_table <- c(\"name1\", \"name2\", ...)\n\n")
  
  ## --- Return result ----------------------------------------------------------
  log_message(paste("Model complete:", k, "topics,", 
                    ifelse(model_result$convergence$converged, "converged", "not converged")), 
              "fit_model")
  
  return(create_result(
    data = list(
      model = model_result,
      topic_proportions = final_theta,
      aligned_meta = final_meta,
      category_map = category_map
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      k = k,
      iterations_run = model_result$convergence$its,
      converged = model_result$convergence$converged,
      segmentation_used = used_segmentation,
      success = TRUE
    ),
    diagnostics = list()
  ))
}