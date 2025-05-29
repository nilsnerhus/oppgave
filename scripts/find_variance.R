#' @title Calculate Variance Explained with Bootstrap Confidence Intervals
#' @description Core function to calculate variance explained by a categorical variable
#'   using bootstrap resampling for confidence intervals, matching find_dominance approach
#'
#' @param theta Matrix of topic proportions (documents × topics)
#' @param doc_indices Indices of documents to include (for compatibility, uses all)
#' @param category_var Categorical variable for variance decomposition
#' @param topics_to_analyze Vector of topic IDs to analyze
#' @param bootstrap Whether to calculate bootstrap confidence intervals (default: TRUE)
#' @param n_bootstrap Number of bootstrap iterations (default: 1000)
#' @param conf_level Confidence level for intervals (default: 0.95)
#'
#' @return List with variance metrics and confidence intervals
find_variance <- function(theta, doc_indices, category_var, topics_to_analyze, 
                          bootstrap = TRUE, n_bootstrap = 1000, conf_level = 0.95) {
  ## --- Setup & Validation ----------------------------------------------------
  # Skip if too few documents or invalid inputs
  if (length(unique(na.omit(category_var))) <= 1) {
    return(NULL)
  }
  
  if (length(topics_to_analyze) == 0) {
    return(NULL)
  }
  
  # Get dimensions
  n_docs <- nrow(theta)
  
  ## --- Calculate main estimate using all data ---------------------------------
  # Calculate variance explained for each topic
  topic_variances <- numeric()
  
  for (topic_id in topics_to_analyze) {
    topic_props <- theta[, topic_id]
    var_explained <- calculate_variance_explained_single(topic_props, category_var)
    
    if (!is.na(var_explained)) {
      topic_variances <- c(topic_variances, var_explained)
    }
  }
  
  # Calculate main estimate (average across topics)
  main_estimate <- if (length(topic_variances) > 0) {
    mean(topic_variances)
  } else {
    return(NULL)
  }
  
  ## --- Bootstrap confidence intervals if requested ---------------------------
  if (bootstrap && n_bootstrap > 0) {
    # Set seed for reproducibility (matching dominance approach)
    set.seed(12345 + n_docs)
    
    bootstrap_estimates <- replicate(n_bootstrap, {
      # Resample documents with replacement (same approach as dominance)
      boot_indices <- sample(1:n_docs, n_docs, replace = TRUE)
      boot_theta <- theta[boot_indices, ]
      boot_category_var <- category_var[boot_indices]
      
      # Calculate variance for each topic in bootstrap sample
      boot_topic_variances <- numeric()
      
      for (topic_id in topics_to_analyze) {
        topic_props <- boot_theta[, topic_id]
        var_explained <- calculate_variance_explained_single(topic_props, boot_category_var)
        
        if (!is.na(var_explained)) {
          boot_topic_variances <- c(boot_topic_variances, var_explained)
        }
      }
      
      # Return average variance for this bootstrap sample
      if (length(boot_topic_variances) > 0) {
        mean(boot_topic_variances)
      } else {
        NA
      }
    })
    
    # Remove NA values from bootstrap results
    bootstrap_estimates <- bootstrap_estimates[!is.na(bootstrap_estimates)]
    
    if (length(bootstrap_estimates) > 0) {
      # Calculate confidence intervals (matching dominance approach)
      alpha <- 1 - conf_level
      ci_bounds <- quantile(bootstrap_estimates, c(alpha/2, 1 - alpha/2), names = FALSE)
      ci_lower <- ci_bounds[1]
      ci_upper <- ci_bounds[2]
      bootstrap_var <- var(bootstrap_estimates)
    } else {
      ci_lower <- NA
      ci_upper <- NA
      bootstrap_var <- 0
    }
  } else {
    # No bootstrap requested
    ci_lower <- main_estimate
    ci_upper <- main_estimate
    bootstrap_var <- 0
  }
  
  ## --- Return results (matching dominance structure) ------------------------
  return(list(
    raw = main_estimate,
    normalized = main_estimate,  # No normalization needed with bootstrap
    variance = bootstrap_var,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_topics_analyzed = length(topics_to_analyze),
    n_docs = n_docs
  ))
}

#' @title Calculate variance explained by a categorical variable for a single topic
#' @description Helper function to calculate variance explained using ANOVA-style decomposition
#' @param topic_props Vector of topic proportions for one topic
#' @param category_var Categorical variable
#' @return Proportion of variance explained (0-1)
calculate_variance_explained_single <- function(topic_props, category_var) {
  # Skip if variable has only one level or all NAs
  if (length(unique(na.omit(category_var))) <= 1) {
    return(NA)
  }
  
  # Total variance
  total_mean <- mean(topic_props, na.rm = TRUE)
  ss_total <- sum((topic_props - total_mean)^2, na.rm = TRUE)
  
  if (ss_total == 0) return(0)
  
  # Between-group variance
  group_stats <- aggregate(topic_props, 
                           by = list(category = category_var), 
                           FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                               n = sum(!is.na(x))))
  
  ss_between <- sum(group_stats$x[,"n"] * 
                      (group_stats$x[,"mean"] - total_mean)^2, 
                    na.rm = TRUE)
  
  # Return proportion (not percentage)
  return(ss_between / ss_total)
}