#' @title Calculate dominance metrics for a set of documents
#' @description Core function that calculates document-level and corpus-level 
#'   dominance metrics for a given set of documents.
#'
#' @param theta Matrix of topic proportions (documents × topics)
#' @param doc_indices Indices of documents to include (default: all)
#' @param n Number of top topics to consider (default: 3)
#' @param normalize Whether to normalize dominance values (default: TRUE)
#'
#' @return List with document-level and corpus-level metrics
find_dominance <- function(model, doc_indices = NULL, n = 3, normalize = TRUE) {
  # Use all documents if none specified
  if (is.null(doc_indices)) {
    doc_indices <- 1:nrow(theta)
  }
  
  theta <- model$data$model$theta
  
  # Skip if too few documents
  if (length(doc_indices) < 3) {
    return(NULL)
  }
  
  # Get number of topics
  k <- ncol(theta)
  
  # Normalization baseline
  uniform_baseline <- n/k
  
  # Document-level dominance (sum of top n topics in each document)
  doc_values <- apply(theta[doc_indices, , drop = FALSE], 1, function(doc_props) {
    sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
  })
  
  # Calculate mean and variance
  doc_mean <- mean(doc_values)
  doc_var <- var(doc_values)
  
  # Corpus-level dominance (sum of top n topics in corpus average)
  corpus_props <- colMeans(theta[doc_indices, , drop = FALSE])
  sorted_props <- sort(corpus_props, decreasing = TRUE)
  corpus_dom <- sum(sorted_props[1:min(n, length(sorted_props))])
  corpus_var <- var(sorted_props[1:min(n, length(sorted_props))])
  
  # Get top topic indices
  top_indices <- order(corpus_props, decreasing = TRUE)[1:min(n, k)]
  
  # Normalize if requested
  if (normalize) {
    doc_norm <- (doc_mean - uniform_baseline) / (1 - uniform_baseline)
    corpus_norm <- (corpus_dom - uniform_baseline) / (1 - uniform_baseline)
  } else {
    doc_norm <- doc_mean
    corpus_norm <- corpus_dom
  }
  
  # Return metrics
  return(list(
    doc_level = list(
      raw = doc_mean,
      normalized = doc_norm,
      variance = doc_var
    ),
    corpus_level = list(
      raw = corpus_dom,
      normalized = corpus_norm,
      variance = corpus_var
    ),
    metadata = list(
      documents = length(doc_indices),
      top_indices = top_indices
    )
  ))
}