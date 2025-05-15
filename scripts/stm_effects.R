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
  
  ## --- Input validation ------------------------------------------------------
  # Check model structure
  if (is.null(model) || !is.list(model) || is.null(model$data) || 
      is.null(model$data$model)) {
    stop("Invalid model: must contain STM model object in model$data$model")
  }
  
  # Extract STM model and metadata
  stm_model <- model$data$model
  
  # Get topic data with metadata
  if (is.null(model$data$topic_proportions)) {
    stop("Model missing topic_proportions")
  }
  
  metadata_df <- model$data$topic_proportions
  
  # Check category_map
  if (is.null(category_map) || !is.list(category_map)) {
    stop("category_map must be a list mapping categories to dimensions")
  }
  
  ## --- Process each category ------------------------------------------------
  for (category_name in names(category_map)) {
    log_message(paste("Processing", category_name, "effects"), "stm_effects")
    
    dimensions <- category_map[[category_name]]
    if (length(dimensions) == 0) next
    
    # For categorical dimensions like Income and Region
    if (category_name %in% c("Income", "Region")) {
      dim <- dimensions[1]  # Should be one dimension per category
      
      # Skip if dimension not in metadata
      if (!dim %in% names(metadata_df)) {
        warning_msg <- paste("Dimension", dim, "not found in metadata")
        log_message(warning_msg, "stm_effects", "WARNING")
        diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
        next
      }
      
      # Make sure the variable is a factor
      if (!is.factor(metadata_df[[dim]])) {
        # Convert to factor if needed
        metadata_df[[dim]] <- as.factor(metadata_df[[dim]])
      }
      
      # Create formula string and convert to formula
      formula_str <- paste("~", dim)
      formula <- stats::as.formula(formula_str)
      
      # Try to estimate effects with better error handling
      tryCatch({
        # Log formula for debugging
        log_message(paste("Using formula:", formula_str), "stm_effects")
        
        # Estimate effects for all topics
        effects <- stm::estimateEffect(
          formula = formula, 
          stmobj = stm_model, 
          metadata = metadata_df,
          uncertainty = uncertainty
        )
        
        # Extract R-squared as a measure of variance explained
        # Use a safer approach for categorical variables
        variance_summary <- summary(effects)
        
        # Default to 0 if we can't calculate
        variance_explained <- 0
        
        # Try to extract R-squared from summary
        if (!is.null(variance_summary) && !is.null(variance_summary$r2)) {
          variance_explained <- variance_summary$r2
        } else {
          # Calculate categorical variance using model fit
          topic_models <- effects$parameters
          total_variance <- 0
          valid_models <- 0
          
          # Average R-squared across topics
          for (topic_idx in 1:length(topic_models)) {
            if (!is.null(topic_models[[topic_idx]])) {
              model_summary <- summary(topic_models[[topic_idx]])
              if (!is.null(model_summary) && !is.null(model_summary$r.squared)) {
                total_variance <- total_variance + model_summary$r.squared
                valid_models <- valid_models + 1
              }
            }
          }
          
          if (valid_models > 0) {
            variance_explained <- total_variance / valid_models
          }
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
        
        # Add debugging info
        log_message(paste("Formula used:", formula_str), "stm_effects", "DEBUG")
        log_message(paste("Variable type:", class(metadata_df[[dim]])), "stm_effects", "DEBUG")
        log_message(paste("First few values:", paste(head(metadata_df[[dim]]), collapse=", ")), "stm_effects", "DEBUG")
      })
      
    } else if (category_name == "Geography") {
      # For Geography category (binary dimensions)
      
      # Process each binary dimension separately
      for (dim in dimensions) {
        # Skip if dimension not in metadata
        if (!dim %in% names(metadata_df)) {
          warning_msg <- paste("Dimension", dim, "not found in metadata")
          log_message(warning_msg, "stm_effects", "WARNING")
          diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
          next
        }
        
        # Format dimension name
        dim_name <- switch(dim,
                           "is_sids" = "SIDS",
                           "is_lldc" = "LLDC",
                           "is_ldc" = "LDC",
                           gsub("is_", "", dim))
        
        # Ensure binary dimensions are properly formatted
        if (!is.logical(metadata_df[[dim]])) {
          if (is.character(metadata_df[[dim]]) || is.factor(metadata_df[[dim]])) {
            metadata_df[[dim]] <- tolower(as.character(metadata_df[[dim]])) %in% c("true", "yes", "1", "t")
          } else if (is.numeric(metadata_df[[dim]])) {
            metadata_df[[dim]] <- metadata_df[[dim]] > 0
          }
        }
        
        # Make sure it's a factor for STM
        metadata_df[[dim]] <- as.factor(metadata_df[[dim]])
        
        # Create formula string and convert to formula
        formula_str <- paste("~", dim)
        formula <- stats::as.formula(formula_str)
        
        # Try to estimate effects
        tryCatch({
          # Log formula for debugging
          log_message(paste("Using formula:", formula_str), "stm_effects")
          
          # Estimate effects
          effects <- stm::estimateEffect(
            formula = formula, 
            stmobj = stm_model, 
            metadata = metadata_df,
            uncertainty = uncertainty
          )
          
          # Calculate variance explained
          variance_summary <- summary(effects)
          
          # Default to 0 if we can't calculate
          variance_explained <- 0
          
          # Try to extract R-squared from summary
          if (!is.null(variance_summary) && !is.null(variance_summary$r2)) {
            variance_explained <- variance_summary$r2
          } else {
            # Calculate binary variance using model fit
            topic_models <- effects$parameters
            total_variance <- 0
            valid_models <- 0
            
            # Average R-squared across topics
            for (topic_idx in 1:length(topic_models)) {
              if (!is.null(topic_models[[topic_idx]])) {
                model_summary <- summary(topic_models[[topic_idx]])
                if (!is.null(model_summary) && !is.null(model_summary$r.squared)) {
                  total_variance <- total_variance + model_summary$r.squared
                  valid_models <- valid_models + 1
                }
              }
            }
            
            if (valid_models > 0) {
              variance_explained <- total_variance / valid_models
            }
          }
          
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
          
          # Add debugging info
          log_message(paste("Formula used:", formula_str), "stm_effects", "DEBUG")
          log_message(paste("Variable type:", class(metadata_df[[dim]])), "stm_effects", "DEBUG")
          log_message(paste("First few values:", paste(head(metadata_df[[dim]]), collapse=", ")), "stm_effects", "DEBUG")
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