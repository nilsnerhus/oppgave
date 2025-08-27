#' @title Enhanced Statistical Significance Testing with Effect Sizes
#' @description Tests group effects on topics using STM's estimateEffect and extracts 
#'   comprehensive statistical information including effect sizes and confidence intervals.
find_variance <- function(stm_model, stm_meta, col_name, col_value = TRUE, top_topics) {
  
  # Quick validation and setup
  if (!col_name %in% names(stm_meta) || length(top_topics) == 0) {
    return(list(
      effect_size = NA_real_,
      std_error = NA_real_,
      conf_lower = NA_real_,
      conf_upper = NA_real_,
      p_value = NA_real_, 
      significant = FALSE,
      r_squared = NA_real_
    ))
  }
  
  # Create test variable efficiently
  test_var <- if (!missing(col_value) && !is.logical(col_value)) {
    stm_meta[[col_name]] == col_value
  } else {
    stm_meta[[col_name]]
  }
  
  # Check variation
  if (length(unique(test_var[!is.na(test_var)])) <= 1) {
    return(list(
      effect_size = NA_real_,
      std_error = NA_real_,
      conf_lower = NA_real_,
      conf_upper = NA_real_,
      p_value = NA_real_, 
      significant = FALSE,
      r_squared = NA_real_
    ))
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
    
    # Extract comprehensive statistics
    results_list <- lapply(seq_along(top_topics), function(i) {
      if (length(stm_effects$parameters) >= i) {
        results <- stm_effects$parameters[[i]][[1]]
        coefs <- results$est
        vcov <- results$vcov
        
        if (length(coefs) >= 2 && nrow(vcov) >= 2) {
          # Extract effect size and standard error
          effect_size <- coefs[2]  # Coefficient for the group effect
          std_error <- sqrt(vcov[2, 2])  # Standard error
          
          # Calculate confidence intervals (95%)
          conf_lower <- effect_size - 1.96 * std_error
          conf_upper <- effect_size + 1.96 * std_error
          
          # Calculate t-statistic and p-value
          t_stat <- effect_size / std_error
          df <- nrow(temp_meta) - 2
          p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)
          
          # Calculate R-squared (proportion of variance explained)
          # This is approximate - you might want to extract this differently
          r_squared <- if (length(results$rsquared) > 0) results$rsquared else NA_real_
          
          return(list(
            effect_size = effect_size,
            std_error = std_error,
            conf_lower = conf_lower,
            conf_upper = conf_upper,
            p_value = p_value,
            significant = p_value < 0.05,
            r_squared = r_squared,
            t_statistic = t_stat,
            degrees_freedom = df
          ))
        }
      }
      return(NULL)
    })
    
    # Filter out NULL results and find the most significant effect
    valid_results <- results_list[!sapply(results_list, is.null)]
    
    if (length(valid_results) > 0) {
      # Return the result with the smallest p-value
      best_result <- valid_results[[which.min(sapply(valid_results, function(x) x$p_value))]]
      return(best_result)
    } else {
      return(list(
        effect_size = NA_real_,
        std_error = NA_real_,
        conf_lower = NA_real_,
        conf_upper = NA_real_,
        p_value = NA_real_, 
        significant = FALSE,
        r_squared = NA_real_
      ))
    }
    
  }, error = function(e) {
    return(list(
      effect_size = NA_real_,
      std_error = NA_real_,
      conf_lower = NA_real_,
      conf_upper = NA_real_,
      p_value = NA_real_, 
      significant = FALSE,
      r_squared = NA_real_
    ))
  })
}
