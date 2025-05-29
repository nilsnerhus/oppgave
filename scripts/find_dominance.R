#' @title Calculate Dominance Index with Bootstrap Confidence Intervals
#' @description Calculates document-level and corpus-level dominance metrics using
#'   bootstrap resampling for confidence intervals instead of rarefaction
#'
#' @param theta Matrix of topic proportions (documents × topics)
#' @param doc_indices Indices of documents to include
#' @param n Number of top topics to consider (default: 3)
#' @param bootstrap Whether to calculate bootstrap confidence intervals (default: TRUE)
#' @param n_bootstrap Number of bootstrap iterations (default: 1000)
#' @param conf_level Confidence level for intervals (default: 0.95)
#'
#' @return List with document-level and corpus-level dominance metrics including confidence intervals
find_dominance <- function(theta, doc_indices, n = 3, bootstrap = TRUE, n_bootstrap = 1000, conf_level = 0.95) {
  ## --- Setup & Validation ----------------------------------------------------
  # Skip if too few documents
  if (length(doc_indices) < 3) {
    return(NULL)
  }
  
  # Get dimensions
  k <- ncol(theta)
  doc_count <- length(doc_indices)
  
  ## --- Calculate main estimates using all available data ---------------------
  # Subset to relevant documents
  theta_subset <- theta[doc_indices, , drop = FALSE]
  
  # Calculate how concentrated each document is (document-level dominance)
  doc_dominance <- apply(theta_subset, 1, function(doc_props) {
    sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
  })
  
  # Calculate how concentrated the average topic distribution is (corpus-level dominance)
  corpus_props <- colMeans(theta_subset)
  sorted_idx <- order(corpus_props, decreasing = TRUE)
  corpus_dom <- sum(corpus_props[sorted_idx[1:min(n, k)]])
  
  # Main estimates
  doc_mean_main <- mean(doc_dominance)
  doc_var_main <- var(doc_dominance)
  corpus_main <- corpus_dom
  
  # Store top topic indices for return
  top_indices <- sorted_idx[1:min(n, k)]
  
  ## --- Bootstrap confidence intervals if requested ---------------------------
  if (bootstrap && n_bootstrap > 0) {
    log_message(paste("Calculating bootstrap CIs with", n_bootstrap, "iterations"), "find_dominance")
    
    # Set seed for reproducibility
    set.seed(12345 + doc_count)
    
    # Storage for bootstrap iterations
    doc_means <- numeric(n_bootstrap)
    corpus_doms <- numeric(n_bootstrap)
    
    # Run bootstrap iterations
    for (iter in 1:n_bootstrap) {
      # Resample documents WITH replacement (same total size)
      boot_indices <- sample(doc_indices, length(doc_indices), replace = TRUE)
      boot_theta <- theta[boot_indices, , drop = FALSE]
      
      # Calculate document-level dominance for this bootstrap sample
      boot_doc_dominance <- apply(boot_theta, 1, function(doc_props) {
        sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
      })
      doc_means[iter] <- mean(boot_doc_dominance)
      
      # Calculate corpus-level dominance for this bootstrap sample
      boot_corpus_props <- colMeans(boot_theta)
      boot_sorted_idx <- order(boot_corpus_props, decreasing = TRUE)
      corpus_doms[iter] <- sum(boot_corpus_props[boot_sorted_idx[1:min(n, k)]])
    }
    
    # Calculate bootstrap confidence intervals
    alpha <- 1 - conf_level
    
    doc_ci <- quantile(doc_means, c(alpha/2, 1 - alpha/2), names = FALSE)
    doc_var_boot <- var(doc_means)
    
    corpus_ci <- quantile(corpus_doms, c(alpha/2, 1 - alpha/2), names = FALSE)
    corpus_var_boot <- var(corpus_doms)
    
  } else {
    # No bootstrap - use point estimates only
    doc_ci <- c(doc_mean_main, doc_mean_main)
    doc_var_boot <- 0
    
    corpus_ci <- c(corpus_main, corpus_main)
    corpus_var_boot <- 0
  }
  
  ## --- Return results --------------------------------------------------------
  return(list(
    doc_level = list(
      raw = doc_mean_main, 
      normalized = doc_mean_main,  # No normalization needed with bootstrap
      variance = doc_var_boot,
      ci_lower = doc_ci[1],
      ci_upper = doc_ci[2]
    ),
    corpus_level = list(
      raw = corpus_main, 
      normalized = corpus_main,    # No normalization needed with bootstrap
      variance = corpus_var_boot, 
      top_indices = top_indices,
      ci_lower = corpus_ci[1],
      ci_upper = corpus_ci[2]
    )
  ))
}