#' @title Calculate Dominance Index with Normalization
find_dominance <- function(theta, doc_indices, n = 3) {
  
  # Skip if too few documents
  if (length(doc_indices) < 3) {
    return(NULL)
  }
  
  # Get dimensions
  k <- ncol(theta)
  doc_count <- length(doc_indices)
  
  # Subset to relevant documents
  theta_subset <- theta[doc_indices, , drop = FALSE]
  
  # Calculate document-level dominance (how concentrated each document is)
  doc_dominance <- apply(theta_subset, 1, function(doc_props) {
    sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
  })
  
  # Calculate corpus-level dominance (how concentrated the average is)
  corpus_props <- colMeans(theta_subset)
  sorted_idx <- order(corpus_props, decreasing = TRUE)
  corpus_dom_raw <- sum(corpus_props[sorted_idx[1:min(n, k)]])
  
  # Get top topic indices
  top_indices <- sorted_idx[1:min(n, k)]
  
  # Normalize dominance values to account for k
  normalize_dominance <- function(raw_dominance, k, n_top) {
    # What would we expect with uniform distribution (baseline)
    uniform_baseline <- n_top / k
    
    # Maximum possible dominance is 1.0 (all probability in top n topics)
    max_possible <- 1.0
    
    # Normalize: 0 = uniform distribution, 1 = maximum concentration
    normalized <- (raw_dominance - uniform_baseline) / (max_possible - uniform_baseline)
    
    # Ensure it stays in [0,1] range
    pmax(0, pmin(1, normalized))
  }
  
  # Calculate normalized values
  doc_mean_raw <- mean(doc_dominance)
  doc_mean_normalized <- normalize_dominance(doc_mean_raw, k, min(n, k))
  corpus_normalized <- normalize_dominance(corpus_dom_raw, k, min(n, k))
  
  return(list(
    doc_level = list(
      raw = doc_mean_raw, 
      normalized = doc_mean_normalized
    ),
    corpus_level = list(
      raw = corpus_dom_raw, 
      normalized = corpus_normalized,
      top_indices = top_indices
    )
  ))
}