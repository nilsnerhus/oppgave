#' @title Test Statistical Significance of Group Effects on Topics
#' @description Tests whether categorical groups have statistically significant effects
#'   on specific topics using STM's estimateEffect. Clean, efficient implementation.
#'   
#' @param stm_model STM model object from fit_model()
#' @param stm_meta Original metadata used for model fitting
#' @param col_name Column name of the categorical variable to test
#' @param col_value Specific value to test (for multi-level variables) or TRUE for binary
#' @param top_topics Vector of topic indices to test
#'
#' @return List with p-value and significance indicator
find_variance <- function(stm_model, stm_meta, col_name, col_value = TRUE, top_topics) {
  
  # Quick validation and setup
  if (!col_name %in% names(stm_meta) || length(top_topics) == 0) {
    return(list(p_value = NA_real_, significant = FALSE))
  }
  
  # Create test variable efficiently
  test_var <- if (!missing(col_value) && !is.logical(col_value)) {
    stm_meta[[col_name]] == col_value
  } else {
    stm_meta[[col_name]]
  }
  
  # Check variation
  if (length(unique(test_var[!is.na(test_var)])) <= 1) {
    return(list(p_value = NA_real_, significant = FALSE))
  }
  
  # Run STM test with error handling
  tryCatch({
    # Create formula string efficiently
    topics_str <- if (length(top_topics) == 1) {
      as.character(top_topics)
    } else {
      paste0("c(", paste(top_topics, collapse = ","), ")")
    }
    
    # Prepare metadata and run test
    temp_meta <- stm_meta
    temp_meta$test_group <- as.factor(test_var)
    
    stm_effects <- stm::estimateEffect(
      formula = as.formula(paste(topics_str, "~ test_group")),
      stmobj = stm_model,
      metadata = temp_meta,
      uncertainty = "None"
    )
    
    # Extract minimum p-value efficiently
    p_values <- vapply(seq_along(top_topics), function(i) {
      if (length(stm_effects$parameters) >= i) {
        results <- stm_effects$parameters[[i]][[1]]
        coefs <- results$est
        vcov <- results$vcov
        
        if (length(coefs) >= 2 && nrow(vcov) >= 2) {
          t_stat <- coefs[2] / sqrt(vcov[2, 2])
          df <- nrow(temp_meta) - 2
          return(2 * pt(abs(t_stat), df, lower.tail = FALSE))
        }
      }
      return(NA_real_)
    }, numeric(1))
    
    min_p <- min(p_values, na.rm = TRUE)
    
    return(list(
      p_value = if(is.finite(min_p)) min_p else NA_real_,
      significant = is.finite(min_p) && min_p < 0.05
    ))
    
  }, error = function(e) {
    return(list(p_value = NA_real_, significant = FALSE))
  })
}