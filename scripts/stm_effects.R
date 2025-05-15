#' @title Calculate STM effects for metadata variables
#' @description Estimates how metadata variables explain variance in topic prevalence
#'   using the structural topic model estimateEffect function
#'   
#' @param model The complete structural topic model output from fit_model()
#' @param category_map List mapping categories to dimensions
#' @param uncertainty Number of uncertainty draws (default: 20)
#'
#' @return A list with estimated effects and calculated variance explained
stm_effects <- function(
    model,
    category_map,
    uncertainty = 20
) {
  # Extract STM model and metadata
  stm_model <- model$data$model
  metadata_df <- model$data$topic_proportions
  
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize storage for results
  effects_results <- list()
  diagnostics <- list(
    processing_issues = character()
  )
  
  log_message("Beginning STM effects analysis", "stm_effects")
  
  # Process each category
  for (category_name in names(category_map)) {
    dimensions <- category_map[[category_name]]
    if (length(dimensions) == 0) next
    
    log_message(paste("Processing", category_name, "effects"), "stm_effects")
    
    # For categorical dimensions like Income and Region
    if (category_name %in% c("Income", "Region")) {
      dim <- dimensions[1]
      
      # Skip if dimension not in metadata
      if (!dim %in% names(metadata_df)) {
        warning <- paste("Dimension", dim, "not found in metadata")
        log_message(warning, "stm_effects", "WARNING")
        diagnostics$processing_issues <- c(diagnostics$processing_issues, warning)
        next
      }
      
      # Create formula for this dimension
      formula <- stats::as.formula(paste("~", dim))
      
      # Try to estimate effects
      tryCatch({
        # Estimate effects for all topics
        effects <- stm::estimateEffect(
          formula, 
          stmobj = stm_model, 
          metadata = metadata_df,
          uncertainty = uncertainty
        )
        
        # Extract R-squared as a measure of variance explained
        # Categorical variables need a different approach
        if (is.factor(metadata_df[[dim]]) || is.character(metadata_df[[dim]])) {
          # For categorical variables, we need to run individual models
          variance_explained <- calculate_categorical_variance(effects, stm_model$K)
        } else {
          # For numeric variables, we can use a simpler approach
          variance_explained <- calculate_numeric_variance(effects, stm_model$K)
        }
        
        # Store results with variance explained
        effects_results[[category_name]] <- list(
          effects = effects,
          variance = variance_explained
        )
        
        log_message(paste(category_name, "explains approximately", 
                          round(variance_explained * 100, 1), "% of topic variance"), 
                    "stm_effects")
        
      }, error = function(e) {
        error_msg <- paste("Error estimating effects for", category_name, ":", e$message)
        log_message(error_msg, "stm_effects", "ERROR")
        diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
      })
      
    } else if (category_name == "Geography") {
      # For Geography (multiple binary dimensions)
      # Process each binary dimension separately
      for (dim in dimensions) {
        # Skip if dimension not in metadata
        if (!dim %in% names(metadata_df)) {
          warning <- paste("Dimension", dim, "not found in metadata")
          log_message(warning, "stm_effects", "WARNING")
          diagnostics$processing_issues <- c(diagnostics$processing_issues, warning)
          next
        }
        
        # Format dimension name
        dim_name <- switch(dim,
                           "is_sids" = "SIDS",
                           "is_lldc" = "LLDC",
                           "is_ldc" = "LDC",
                           gsub("is_", "", dim))
        
        # Create formula for this dimension
        formula <- stats::as.formula(paste("~", dim))
        
        # Try to estimate effects
        tryCatch({
          # Ensure binary dimensions are properly formatted
          if (!is.logical(metadata_df[[dim]])) {
            if (is.character(metadata_df[[dim]]) || is.factor(metadata_df[[dim]])) {
              metadata_df[[dim]] <- tolower(as.character(metadata_df[[dim]])) %in% c("true", "yes", "1", "t")
            } else if (is.numeric(metadata_df[[dim]])) {
              metadata_df[[dim]] <- metadata_df[[dim]] > 0
            }
          }
          
          # Estimate effects
          effects <- stm::estimateEffect(
            formula, 
            stmobj = stm_model, 
            metadata = metadata_df,
            uncertainty = uncertainty
          )
          
          # Calculate variance explained (simpler for binary)
          variance_explained <- calculate_binary_variance(effects, stm_model$K)
          
          # Store with geography prefix
          effects_results[[paste0("Geography_", dim_name)]] <- list(
            effects = effects,
            variance = variance_explained
          )
          
          log_message(paste(dim_name, "explains approximately", 
                            round(variance_explained * 100, 1), "% of topic variance"), 
                      "stm_effects")
          
        }, error = function(e) {
          error_msg <- paste("Error estimating effects for", dim_name, ":", e$message)
          log_message(error_msg, "stm_effects", "ERROR")
          diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
        })
      }
    }
  }
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    categories = names(effects_results),
    uncertainty = uncertainty,
    model_k = model$metadata$k
  )
  
  # Create final result
  final_result <- list(
    effects = effects_results,
    metadata = metadata,
    diagnostics = diagnostics
  )
  
  log_message("STM effects analysis complete", "stm_effects")
  
  return(final_result)
}

#' @title Calculate variance explained for categorical variables
#' @description Helper function to calculate approximate variance explained
#'   by categorical variables across topics
#'
#' @param effects The estimateEffect object
#' @param k Number of topics
#'
#' @return Proportion of variance explained (0-1)
calculate_categorical_variance <- function(effects, k) {
  # This is a simplified approach
  # Initialize total and residual sum of squares
  total_variance <- 0
  
  # Loop through topics and calculate average variance explained
  for (i in 1:k) {
    # Get summary for this topic
    tryCatch({
      s <- summary(effects, topics = i)
      
      # Add this topic's explained variance
      # For categorical variables, r2 is already calculated
      if (!is.null(s$r2)) {
        total_variance <- total_variance + s$r2
      }
    }, error = function(e) {
      # Skip on error
    })
  }
  
  # Return average across topics
  return(total_variance / k)
}

#' @title Calculate variance explained for binary variables
#' @description Helper function to calculate approximate variance explained
#'   by binary variables across topics
#'
#' @param effects The estimateEffect object
#' @param k Number of topics
#'
#' @return Proportion of variance explained (0-1)
calculate_binary_variance <- function(effects, k) {
  # Initialize sum
  total_variance <- 0
  valid_topics <- 0
  
  # Loop through topics
  for (i in 1:k) {
    tryCatch({
      # Get summary for this topic
      s <- summary(effects, topics = i)
      
      # For binary predictors, calculate effect size
      # Cohen's f² = t² / df_residual as an approximation of explained variance
      table <- s$tables[[1]]
      t_value <- table$est / table$se
      f_squared <- t_value^2 / s$model$df.residual
      
      # Convert to R-squared equivalent: R² = f² / (1 + f²)
      r_squared <- f_squared / (1 + f_squared)
      
      # Only count significant effects (p < 0.05)
      if (table$pval < 0.05) {
        total_variance <- total_variance + r_squared
        valid_topics <- valid_topics + 1
      }
    }, error = function(e) {
      # Skip on error
    })
  }
  
  # Return average across significant topics, or 0 if none significant
  if (valid_topics > 0) {
    return(total_variance / k)  # Divide by all topics, not just significant ones
  } else {
    return(0)
  }
}

#' @title Calculate variance explained for numeric variables
#' @description Helper function to calculate approximate variance explained
#'   by numeric variables across topics
#'
#' @param effects The estimateEffect object
#' @param k Number of topics
#'
#' @return Proportion of variance explained (0-1)
calculate_numeric_variance <- function(effects, k) {
  # Similar to binary calculation but for numeric predictors
  return(calculate_binary_variance(effects, k))
}