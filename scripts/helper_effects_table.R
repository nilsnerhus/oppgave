#' @title Create effects summary table
#' @description Creates a formatted table showing percentage of variance in topic
#'   prevalence explained by each metadata dimension, respecting p-value threshold
#'
#' @param stm_effects Output from stm_effects() function
#' @param p_threshold P-value threshold for significance (default: 0.05)
#'
#' @return A data frame summarizing variance explained by each dimension
create_effects_table <- function(
    stm_effects,
    p_threshold = 0.05
) {
  # Initialize results table
  result_table <- data.frame(
    category = character(),
    dimension = character(),
    explained_var = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
  
  # Process each dimension's effects
  for (category in names(stm_effects$effects)) {
    # Extract main category name
    main_category <- if (grepl("^Geography_", category)) "Geography" else category
    
    # Extract dimension name
    dimension <- if (grepl("^Geography_", category)) {
      # Extract the part after "Geography_"
      gsub("^Geography_", "", category)
    } else {
      # Get from the category itself
      sub_dim <- names(stm_effects$effects[[category]]$effects$formula)[2]
      sub_dim <- gsub("^~", "", sub_dim)
      trimws(sub_dim)
    }
    
    # Get variance explained
    variance <- stm_effects$effects[[category]]$variance
    
    # Check if effect is significant
    # This is a simplified approach - a more comprehensive one would check
    # significance for each topic and aggregate
    significant <- FALSE
    
    # Try to determine significance
    tryCatch({
      effects_obj <- stm_effects$effects[[category]]$effects
      
      # Check significance for at least one topic
      k <- stm_effects$metadata$model_k
      for (i in 1:k) {
        s <- summary(effects_obj, topics = i)
        
        # Different handling based on dimension type
        if (length(s$tables) > 1 || nrow(s$tables[[1]]) > 1) {
          # Categorical - check levels
          for (tbl in s$tables) {
            if (any(tbl$pval < p_threshold)) {
              significant <- TRUE
              break
            }
          }
        } else {
          # Binary - check single p-value
          if (s$tables[[1]]$pval < p_threshold) {
            significant <- TRUE
            break
          }
        }
        
        if (significant) break
      }
      
    }, error = function(e) {
      warning(paste("Error checking significance for", dimension, ":", e$message))
    })
    
    # Add to results
    result_table <- rbind(result_table, data.frame(
      category = main_category,
      dimension = dimension,
      explained_var = if (significant) round(variance * 100, 1) else NA,
      significant = significant,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by explained variance (descending)
  result_table <- result_table[order(result_table$explained_var, decreasing = TRUE), ]
  
  # Replace NA with "Not sig." for display
  result_table$explained_var_display <- ifelse(
    is.na(result_table$explained_var),
    "Not sig.",
    as.character(result_table$explained_var)
  )
  
  return(result_table)
}