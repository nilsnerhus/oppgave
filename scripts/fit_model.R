
fit_model <- function(dfm, k_result, category_map = NULL, iterations = 200, seed = 12345) {  # Changed: k_result instead of k
  
  start_time <- Sys.time()
  set.seed(seed)
  
  # Extract k from k_result
  k <- k_result$data$best_k  # Added: extract k internally
  
  ## --- Validation & Setup -----------------------------------------------------
  if (!is.numeric(k) || k <= 0 || k != round(k)) {
    warning("Invalid k value from k_result, using default k = 15")
    k <- 15
  }
  
  log_message(paste("Using k =", k, "from find_k result"), "fit_model")
  if (!is.numeric(k) || k <= 0 || k != round(k)) k <- 15
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

## --- Helper functions -------------------------------------------------------

#' Build prevalence formula from category map
build_prevalence_formula <- function(category_map, meta) {
  if (is.null(category_map)) return(NULL)
  
  # Get all variables and filter to those with variation
  all_vars <- unlist(category_map, use.names = FALSE)
  varying_vars <- all_vars[sapply(all_vars, function(var) {
    var %in% names(meta) && length(unique(meta[[var]])) > 1
  })]
  
  if (length(varying_vars) == 0) return(NULL)
  
  formula_string <- paste("~", paste(varying_vars, collapse = " + "))
  log_message(paste("Prevalence formula:", formula_string), "fit_model")
  as.formula(formula_string)
}

#' Aggregate segment-level theta to document level  
aggregate_by_document <- function(theta, doc_ids, segment_map) {
  
  # Get unique documents in order they appear
  unique_docs <- unique(doc_ids)
  k <- ncol(theta)
  
  # Pre-allocate result matrix
  doc_theta <- matrix(0, nrow = length(unique_docs), ncol = k,
                      dimnames = list(unique_docs, colnames(theta)))
  
  # Vectorized aggregation using split-apply-combine
  theta_by_doc <- split.data.frame(theta, doc_ids)
  
  for (i in seq_along(unique_docs)) {
    doc_id <- unique_docs[i]
    doc_segments <- theta_by_doc[[doc_id]]
    
    if (nrow(doc_segments) == 1) {
      doc_theta[i, ] <- as.numeric(doc_segments)
    } else {
      doc_theta[i, ] <- colMeans(doc_segments)
    }
  }
  
  return(doc_theta)
}

#' Extract document-level metadata from segments
get_document_metadata <- function(segment_meta, segment_map) {
  
  # Get first occurrence of each document
  unique_docs <- unique(segment_meta$doc_id)
  first_occurrences <- match(unique_docs, segment_meta$doc_id)
  
  segment_meta[first_occurrences, ]
}