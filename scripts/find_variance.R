#' @title Calculate Variance Explained with Bootstrap Confidence Intervals
find_variance <- function(theta, doc_indices, category_var, topics_to_analyze, 
                          bootstrap = TRUE, n_bootstrap = 1000, conf_level = 0.95) {
  
  # Skip if too few categories or topics
  if (length(unique(na.omit(category_var))) <= 1 || length(topics_to_analyze) == 0) {
    return(NULL)
  }
  
  # Work on document subset
  theta_subset <- theta[doc_indices, , drop = FALSE]
  category_subset <- category_var[doc_indices]
  
  # Calculate variance for each topic
  topic_variances <- numeric()
  for (topic_id in topics_to_analyze) {
    topic_props <- theta_subset[, topic_id]
    
    # Total variance
    total_mean <- mean(topic_props, na.rm = TRUE)
    ss_total <- sum((topic_props - total_mean)^2, na.rm = TRUE)
    
    if (ss_total > 0) {
      # Between-group variance
      group_stats <- aggregate(topic_props, by = list(category_subset), 
                               FUN = function(x) c(mean = mean(x, na.rm = TRUE), n = sum(!is.na(x))))
      ss_between <- sum(group_stats$x[,"n"] * (group_stats$x[,"mean"] - total_mean)^2, na.rm = TRUE)
      
      topic_variances <- c(topic_variances, ss_between / ss_total)
    }
  }
  
  # Main estimate
  main_estimate <- if (length(topic_variances) > 0) mean(topic_variances) else return(NULL)
  
  # Bootstrap confidence intervals
  if (bootstrap && n_bootstrap > 0) {
    set.seed(12345 + length(doc_indices))
    
    bootstrap_estimates <- numeric(n_bootstrap)
    
    for (iter in 1:n_bootstrap) {
      # Resample documents with replacement from the subset
      boot_indices <- sample(doc_indices, length(doc_indices), replace = TRUE)
      boot_theta <- theta[boot_indices, , drop = FALSE]
      boot_category_var <- category_var[boot_indices]
      
      boot_topic_variances <- numeric()
      for (topic_id in topics_to_analyze) {
        topic_props <- boot_theta[, topic_id]
        total_mean <- mean(topic_props, na.rm = TRUE)
        ss_total <- sum((topic_props - total_mean)^2, na.rm = TRUE)
        
        if (ss_total > 0) {
          group_stats <- aggregate(topic_props, by = list(boot_category_var), 
                                   FUN = function(x) c(mean = mean(x, na.rm = TRUE), n = sum(!is.na(x))))
          ss_between <- sum(group_stats$x[,"n"] * (group_stats$x[,"mean"] - total_mean)^2, na.rm = TRUE)
          boot_topic_variances <- c(boot_topic_variances, ss_between / ss_total)
        }
      }
      
      bootstrap_estimates[iter] <- if (length(boot_topic_variances) > 0) mean(boot_topic_variances) else NA
    }
    
    bootstrap_estimates <- bootstrap_estimates[!is.na(bootstrap_estimates)]
    
    if (length(bootstrap_estimates) > 0) {
      alpha <- 1 - conf_level
      ci_bounds <- quantile(bootstrap_estimates, c(alpha/2, 1 - alpha/2), names = FALSE)
      ci_lower <- ci_bounds[1]
      ci_upper <- ci_bounds[2]
      bootstrap_var <- var(bootstrap_estimates)
    } else {
      ci_lower <- ci_upper <- main_estimate
      bootstrap_var <- 0
    }
  } else {
    ci_lower <- ci_upper <- main_estimate
    bootstrap_var <- 0
  }
  
  return(list(
    raw = main_estimate,
    normalized = main_estimate,
    variance = bootstrap_var,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_topics_analyzed = length(topics_to_analyze),
    n_docs = length(doc_indices)
  ))
}