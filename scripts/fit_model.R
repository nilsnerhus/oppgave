# Fit STM model with proper segmentation handling
fit_model <- function(dfm, k_result, category_map = NULL, iterations = 200, seed = 12345) {
  
  set.seed(seed)
  start_time <- Sys.time()
  
  # Extract k from k_result
  k <- k_result$data$best_k
  
  ## --- Extract data -----------------------------------------------------------
  docs <- dfm$data$documents
  vocab <- dfm$data$vocab
  meta <- dfm$data$meta  # This is the segmented metadata (222 rows)
  
  log_message(paste("Fitting STM: k =", k, ",", length(docs), "documents"), "fit_model")
  
  ## --- Build prevalence formula (using segmented metadata) -------------------
  prevalence_formula <- NULL
  if (!is.null(category_map)) {
    vars <- unlist(category_map)
    # Only keep variables that exist AND have variation in the segmented metadata
    vars <- vars[sapply(vars, function(v) v %in% names(meta) && length(unique(meta[[v]])) > 1)]
    
    if (length(vars) > 0) {
      prevalence_formula <- as.formula(paste("~", paste(vars, collapse = " + ")))
      log_message(paste("Prevalence formula:", deparse(prevalence_formula)), "fit_model")
    }
  }
  
  ## --- Fit STM model ----------------------------------------------------------
  model_result <- tryCatch({
    stm::stm(
      documents = docs,
      vocab = vocab, 
      K = k,
      data = meta,  # Same metadata as used for prevalence formula
      prevalence = prevalence_formula,
      max.em.its = iterations,
      verbose = FALSE
    )
  }, error = function(e) {
    stop("STM fitting failed: ", e$message)
  })
  
  ## --- Handle segmentation aggregation ----------------------------------------
  final_theta <- model_result$theta
  final_meta <- meta
  
  # If segmentation was used, aggregate back to documents
  if (!is.null(dfm$metadata$segmentation) && dfm$metadata$segmentation$used_segmentation) {
    log_message("Aggregating segments back to documents", "fit_model")
    
    # Group segments by doc_id and average topic proportions
    unique_doc_ids <- unique(meta$doc_id)
    k_topics <- ncol(final_theta)
    
    # Create document-level theta matrix
    doc_theta <- matrix(0, nrow = length(unique_doc_ids), ncol = k_topics)
    for (i in seq_along(unique_doc_ids)) {
      doc_segments <- which(meta$doc_id == unique_doc_ids[i])
      if (length(doc_segments) == 1) {
        doc_theta[i, ] <- final_theta[doc_segments, ]
      } else {
        doc_theta[i, ] <- colMeans(final_theta[doc_segments, , drop = FALSE])
      }
    }
    
    # Create document-level metadata (first occurrence of each doc_id)
    doc_meta <- meta[match(unique_doc_ids, meta$doc_id), ]
    
    final_theta <- doc_theta
    final_meta <- doc_meta
    
    log_message(paste("Aggregated", nrow(model_result$theta), "segments to", nrow(final_theta), "documents"), "fit_model")
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
                    ifelse(model_result$convergence$converged, "converged", "not converged")), "fit_model")
  
  return(create_result(
    data = list(
      model = model_result,
      topic_proportions = final_theta,
      aligned_meta = final_meta,
      category_map = category_map,
      frex_terms = frex_terms
    ),
    metadata = list(
      k = k,
      converged = model_result$convergence$converged,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      segmentation_used = !is.null(dfm$metadata$segmentation) && dfm$metadata$segmentation$used_segmentation
    )
  ))
}