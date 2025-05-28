#' @title Calculate Dominance Index for Document Set
#' @description Calculates document-level and corpus-level dominance metrics for a given set of documents.
#'   The dominance index quantifies how concentrated discourse is around the top n topics.
#'   When normalize = TRUE, applies rarefaction (sampling without replacement) to standardize sample sizes.
#'
#' @param theta Matrix of topic proportions (documents × topics)
#' @param doc_indices Indices of documents to include
#' @param n Number of top topics to consider (default: 3)
#' @param normalize Whether to apply rarefaction normalization (default: TRUE)
#'
#' @return List with document-level and corpus-level dominance metrics including confidence intervals
find_dominance <- function(theta, doc_indices, n = 3, normalize = TRUE) {
  ## --- Setup & Validation ----------------------------------------------------
  # Skip if too few documents
  if (length(doc_indices) < 3) {
    return(NULL)
  }
  
  # Get dimensions
  k <- ncol(theta)
  doc_count <- length(doc_indices)
  
  ## --- Standard calculations (always performed) ------------------------------
  # Calculate how concentrated each document is
  doc_dominance <- apply(theta[doc_indices, , drop = FALSE], 1, function(doc_props) {
    sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
  })
  
  # Calculate how concentrated the average topic distribution is
  corpus_props <- colMeans(theta[doc_indices, , drop = FALSE])
  sorted_idx <- order(corpus_props, decreasing = TRUE)
  corpus_dom <- sum(corpus_props[sorted_idx[1:min(n, k)]])
  
  # Calculate raw metrics
  doc_mean_raw <- mean(doc_dominance)
  doc_var_raw <- var(doc_dominance)
  corpus_var_raw <- var(corpus_props)
  
  ## --- Apply rarefaction normalization if requested --------------------------
  if (normalize) {
    # Get rarefaction target from environment
    rarefaction_size <- as.numeric(Sys.getenv("RAREFACTION_TARGET", "8"))
    
    # Check if rarefaction is needed
    if (doc_count > rarefaction_size) {
      # Rarefaction parameters
      set.seed(12345 + doc_count)  # Vary seed by doc count for reproducibility
      n_iter <- 1000
      
      # Storage for rarefaction iterations
      doc_means <- numeric(n_iter)
      corpus_doms <- numeric(n_iter)
      
      # Run rarefaction (WITHOUT replacement)
      for (iter in 1:n_iter) {
        # Sample WITHOUT replacement
        sampled_indices <- sample(doc_indices, rarefaction_size, replace = FALSE)
        
        # Calculate document-level dominance for this sample
        sample_doc_dominance <- apply(theta[sampled_indices, , drop = FALSE], 1, function(doc_props) {
          sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
        })
        doc_means[iter] <- mean(sample_doc_dominance)
        
        # Calculate corpus-level dominance for this sample
        sample_corpus_props <- colMeans(theta[sampled_indices, , drop = FALSE])
        sample_sorted_idx <- order(sample_corpus_props, decreasing = TRUE)
        corpus_doms[iter] <- sum(sample_corpus_props[sample_sorted_idx[1:min(n, k)]])
      }
      
      # Calculate rarefied estimates and confidence intervals
      doc_mean_norm <- mean(doc_means)
      doc_var_norm <- var(doc_means)
      doc_ci <- quantile(doc_means, c(0.025, 0.975), names = FALSE)
      
      corpus_dom_norm <- mean(corpus_doms)
      corpus_var_norm <- var(corpus_doms)
      corpus_ci <- quantile(corpus_doms, c(0.025, 0.975), names = FALSE)
      
    } else {
      # If already at or below rarefaction size, use raw values
      doc_mean_norm <- doc_mean_raw
      doc_var_norm <- doc_var_raw
      doc_ci <- c(doc_mean_raw, doc_mean_raw)
      
      corpus_dom_norm <- corpus_dom
      corpus_var_norm <- corpus_var_raw
      corpus_ci <- c(corpus_dom, corpus_dom)
    }
    
    # Use original indices for top topics (most representative)
    top_indices <- sorted_idx[1:min(n, k)]
    
  } else {
    # If not normalizing, use raw values with no uncertainty
    doc_mean_norm <- doc_mean_raw
    doc_var_norm <- doc_var_raw
    doc_ci <- c(doc_mean_raw, doc_mean_raw)
    
    corpus_dom_norm <- corpus_dom
    corpus_var_norm <- corpus_var_raw
    corpus_ci <- c(corpus_dom, corpus_dom)
    
    top_indices <- sorted_idx[1:min(n, k)]
  }
  
  ## --- Return results with confidence intervals ------------------------------
  return(list(
    doc_level = list(
      raw = doc_mean_raw, 
      normalized = doc_mean_norm, 
      variance = doc_var_norm,
      ci_lower = doc_ci[1],
      ci_upper = doc_ci[2]
    ),
    corpus_level = list(
      raw = corpus_dom, 
      normalized = corpus_dom_norm, 
      variance = corpus_var_norm, 
      top_indices = top_indices,
      ci_lower = corpus_ci[1],
      ci_upper = corpus_ci[2]
    )
  ))
}