#' @title Calculate Dominance Index for Document Set
#' @description Calculates document-level and corpus-level dominance metrics for a given set of documents.
#'   The dominance index quantifies how concentrated discourse is around the top n topics.
#'
#' @param theta Matrix of topic proportions (documents × topics)
#' @param doc_indices Indices of documents to include
#' @param n Number of top topics to consider (default: 3)
#' @param normalize Whether to normalize dominance values (default: TRUE)
#'
#' @return List with document-level and corpus-level dominance metrics
find_dominance <- function(theta, doc_indices, n = 3, normalize = TRUE) {
  ## --- Setup & Validation ----------------------------------------------------
  # Skip if too few documents
  if (length(doc_indices) < 3) {
    return(NULL)
  }
  
  # Get dimensions
  k <- ncol(theta)
  
  # Calculate normalization baseline
  uniform_baseline <- n/k
  
  ## --- Document-level dominance ----------------------------------------------
  # Calculate how concentrated each document is
  doc_dominance <- apply(theta[doc_indices, , drop = FALSE], 1, function(doc_props) {
    sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
  })
  
  # Calculate metrics
  doc_mean <- mean(doc_dominance)
  doc_var <- var(doc_dominance)
  
  ## --- Corpus-level dominance ------------------------------------------------
  # Calculate how concentrated the average topic distribution is
  corpus_props <- colMeans(theta[doc_indices, , drop = FALSE])
  sorted_idx <- order(corpus_props, decreasing = TRUE)
  corpus_dom <- sum(corpus_props[sorted_idx[1:min(n, k)]])
  corpus_var <- var(corpus_props)
  
  ## --- Normalization ---------------------------------------------------------
  # Normalize if requested (scale 0-1 where 0 = uniform, 1 = complete concentration)
  if (normalize) {
    doc_norm <- (doc_mean - uniform_baseline) / (1 - uniform_baseline)
    corpus_norm <- (corpus_dom - uniform_baseline) / (1 - uniform_baseline)
  } else {
    doc_norm <- doc_mean
    corpus_norm <- corpus_dom
  }
  
  ## --- Return results --------------------------------------------------------
  return(list(
    doc_level = list(raw = doc_mean, normalized = doc_norm, variance = doc_var),
    corpus_level = list(raw = corpus_dom, normalized = corpus_norm, variance = corpus_var, top_indices = sorted_idx[1:min(n, k)])
  ))
}