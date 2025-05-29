#' @title Calculate Dominance Index with Bootstrap Confidence Intervals
find_dominance <- function(theta, doc_indices, n = 3, bootstrap = TRUE, n_bootstrap = 1000, conf_level = 0.95) {
  
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
  corpus_dom <- sum(corpus_props[sorted_idx[1:min(n, k)]])
  
  # Main estimates
  doc_mean_main <- mean(doc_dominance)
  doc_var_main <- var(doc_dominance)
  corpus_main <- corpus_dom
  top_indices <- sorted_idx[1:min(n, k)]
  
  # Bootstrap confidence intervals
  if (bootstrap && n_bootstrap > 0) {
    set.seed(12345 + doc_count)
    
    doc_means <- numeric(n_bootstrap)
    corpus_doms <- numeric(n_bootstrap)
    
    for (iter in 1:n_bootstrap) {
      # Resample documents with replacement
      boot_indices <- sample(doc_indices, length(doc_indices), replace = TRUE)
      boot_theta <- theta[boot_indices, , drop = FALSE]
      
      # Calculate document-level dominance for bootstrap sample
      boot_doc_dominance <- apply(boot_theta, 1, function(doc_props) {
        sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
      })
      doc_means[iter] <- mean(boot_doc_dominance)
      
      # Calculate corpus-level dominance for bootstrap sample
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
  
  return(list(
    doc_level = list(
      raw = doc_mean_main, 
      normalized = doc_mean_main,
      variance = doc_var_boot,
      ci_lower = doc_ci[1],
      ci_upper = doc_ci[2]
    ),
    corpus_level = list(
      raw = corpus_main, 
      normalized = corpus_main,
      variance = corpus_var_boot, 
      top_indices = top_indices,
      ci_lower = corpus_ci[1],
      ci_upper = corpus_ci[2]
    )
  ))
}