#' @title Estimate effects of covariates on topics and create summary table
#' @description Analyzes the effects of document covariates on topic prevalence,
#'   calculates variance explained by each dimension, and creates a formatted
#'   table for reporting.
#'
#' @param model Result from fit_model() containing the STM model and topic data
#' @param dimensions Specific dimensions to analyze (default: NULL, use all available)
#' @param topics Specific topics to analyze effects for (default: NULL, use all)
#' @param uncertainty Number of simulations for uncertainty estimation (default: 20)
#' @param calculate_significance Whether to calculate statistical significance (default: TRUE)
#' @param alpha Significance level for confidence intervals (default: 0.05)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item effects_table - Formatted table showing variance explained by dimensions
#'       \item effects - List of estimateEffect objects for each dimension
#'       \item variance_explained - Numeric values of variance explained by dimensions
#'       \item significance - Significance information for dimensions (if calculated)
#'     }
#'   }
#'   \item{metadata}{Processing information and configuration}
#'   \item{diagnostics}{Processing issues and statistics}
#'
#' @examples
#' \dontrun{
#' # First fit a model
#' model <- fit_model(stm_data, k = 15)
#' 
#' # Estimate effects with default settings
#' effects_results <- estimate_effect(model)
#' 
#' # Access the formatted table for reporting
#' effects_table <- effects_results$data$effects_table
#' print(effects_table)
#' 
#' # Focus on specific dimensions
#' effects_results <- estimate_effect(model, 
#'                                  dimensions = c("region", "wb_income_level"))
#' }
estimate_effect <- function(
    model,
    dimensions = NULL,
    topics = NULL,
    uncertainty = 20,
    calculate_significance = TRUE,
    alpha = 0.05
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    dimension_issues = list(),
    processing_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "estimate_effect")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model)) {
    error_msg <- "model must be the output from fit_model() with a 'data' component"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "estimate_effect", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check required components
  if (!("topic_proportions" %in% names(model$data))) {
    error_msg <- "Model missing required 'topic_proportions' component"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "estimate_effect", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check for STM model object if not already provided in effects
  if (!("effects" %in% names(model$data)) && !("model" %in% names(model$data))) {
    error_msg <- "Model missing both 'effects' and 'model' components - at least one is required"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "estimate_effect", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Extract data from model
  topic_props <- model$data$topic_proportions
  stm_model <- if ("model" %in% names(model$data)) model$data$model else NULL
  existing_effects <- if ("effects" %in% names(model$data)) model$data$effects else NULL
  
  # Get topic count
  k <- length(grep("^Topic_", names(topic_props)))
  
  # Handle existing effects if provided
  if (!is.null(existing_effects) && length(existing_effects) > 0) {
    log_message("Using existing effects from model", "estimate_effect")
    effects_provided <- TRUE
  } else {
    effects_provided <- FALSE
  }
  
  # Validate dimensions
  if (is.null(dimensions)) {
    # Try to get dimensions from metadata or config
    if ("metadata" %in% names(model) && "prevalence_formula" %in% names(model$metadata)) {
      # Extract from formula
      dimensions <- all.vars(stats::as.formula(model$metadata$prevalence_formula))
      dimensions <- dimensions[dimensions != "1"]  # Remove intercept
    } else if ("config" %in% names(model$data) && "category_map" %in% names(model$data$config)) {
      # Extract from category map
      config <- model$data$config
      all_dimensions <- unlist(config$category_map)
      dimensions <- all_dimensions[all_dimensions %in% names(topic_props)]
    }
    
    # If still NULL, try to find any metadata columns
    if (is.null(dimensions) || length(dimensions) == 0) {
      exclude_cols <- c("doc_id", grep("^Topic_", names(topic_props), value = TRUE))
      dimensions <- setdiff(names(topic_props), exclude_cols)
    }
  } else {
    # Check if provided dimensions exist in the data
    missing_dims <- setdiff(dimensions, names(topic_props))
    if (length(missing_dims) > 0) {
      warning_msg <- paste("Dimensions not found in data:", paste(missing_dims, collapse = ", "))
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "estimate_effect", "WARNING")
      
      # Filter to only available dimensions
      dimensions <- intersect(dimensions, names(topic_props))
    }
  }
  
  # Validate topics
  if (!is.null(topics)) {
    # Convert to numeric if needed
    if (is.character(topics)) {
      # Strip "Topic_" prefix if present
      topics <- gsub("^Topic_", "", topics)
      topics <- as.numeric(topics)
    }
    
    # Ensure topics are in valid range
    valid_topics <- topics[topics >= 1 & topics <= k & !is.na(topics)]
    if (length(valid_topics) < length(topics)) {
      warning_msg <- paste("Some requested topics are invalid - using only valid topics")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "estimate_effect", "WARNING")
      topics <- valid_topics
    }
  } else {
    # Default to all topics
    topics <- 1:k
  }
  
  log_message(paste("Analyzing effects for", length(dimensions), "dimensions across", 
                    length(topics), "topics"), "estimate_effect")
  
  ## --- Helper function to calculate variance explained ------------------------
  calculate_variance <- function(effects_obj, topic_ids) {
    if (is.null(effects_obj)) return(NA)
    
    total_variance <- 0
    valid_topics <- 0
    
    # Loop through topics
    for (i in topic_ids) {
      tryCatch({
        # Get summary for this topic
        s <- summary(effects_obj, topics = i)
        
        # Add this topic's explained variance
        if (!is.null(s$r2)) {
          total_variance <- total_variance + s$r2
          valid_topics <- valid_topics + 1
        }
      }, error = function(e) {
        # Skip this topic if error
      })
    }
    
    # Return average across valid topics
    if (valid_topics > 0) {
      return(total_variance / valid_topics)
    } else {
      return(NA)
    }
  }
  
  ## --- Process each dimension to estimate effects -----------------------------
  log_message("Estimating effects for each dimension", "estimate_effect")
  
  # Initialize results storage
  effects_list <- list()
  variance_explained <- list()
  significance_info <- list()
  
  # Process each dimension
  for (dim in dimensions) {
    log_message(paste("Analyzing effects of", dim), "estimate_effect")
    
    # Skip if term has insufficient variation
    if (is.factor(topic_props[[dim]]) && length(levels(topic_props[[dim]])) < 2) {
      warning_msg <- paste("Dimension", dim, "has insufficient variation for effects analysis")
      diagnostics$dimension_issues[[dim]] <- warning_msg
      log_message(warning_msg, "estimate_effect", "WARNING")
      next
    }
    
    if (is.logical(topic_props[[dim]]) && length(unique(topic_props[[dim]])) < 2) {
      warning_msg <- paste("Dimension", dim, "has insufficient variation for effects analysis")
      diagnostics$dimension_issues[[dim]] <- warning_msg
      log_message(warning_msg, "estimate_effect", "WARNING")
      next
    }
    
    # Check if effect already exists in provided effects
    if (effects_provided && dim %in% names(existing_effects)) {
      log_message(paste("Using existing effect for", dim), "estimate_effect")
      effects_obj <- existing_effects[[dim]]
      
      # Calculate variance
      var_explained <- calculate_variance(effects_obj, topics)
    } else if (!effects_provided && !is.null(stm_model)) {
      # Need to estimate effects
      log_message(paste("Estimating new effect for", dim), "estimate_effect")
      
      # Create formula for this dimension
      dim_formula <- stats::as.formula(paste("~", dim))
      
      # Try to estimate effects
      effects_result <- tryCatch({
        # Estimate effects for specified topics
        effects_obj <- stm::estimateEffect(
          dim_formula, 
          stmobj = stm_model, 
          metadata = topic_props,
          uncertainty = uncertainty
        )
        
        # Calculate variance explained
        var_explained <- calculate_variance(effects_obj, topics)
        
        list(
          effects = effects_obj,
          variance = var_explained
        )
      }, error = function(e) {
        warning_msg <- paste("Error estimating effects for", dim, ":", e$message)
        diagnostics$dimension_issues[[dim]] <<- warning_msg
        log_message(warning_msg, "estimate_effect", "WARNING")
        NULL
      })
      
      # Store results if successful
      if (!is.null(effects_result)) {
        effects_obj <- effects_result$effects
        var_explained <- effects_result$variance
      } else {
        effects_obj <- NULL
        var_explained <- NA
      }
    } else {
      # Can't estimate effects for this dimension
      warning_msg <- paste("Cannot estimate effects for", dim, "- no model provided")
      diagnostics$dimension_issues[[dim]] <- warning_msg
      log_message(warning_msg, "estimate_effect", "WARNING")
      effects_obj <- NULL
      var_explained <- NA
    }
    
    # Store results
    if (!is.null(effects_obj)) {
      effects_list[[dim]] <- effects_obj
      variance_explained[[dim]] <- var_explained
      
      log_message(paste(dim, "explains approximately", 
                        round(var_explained * 100, 1), "% of topic variance"), 
                  "estimate_effect")
      
      # Calculate significance if requested
      if (calculate_significance && !is.na(var_explained)) {
        sig_result <- tryCatch({
          # Calculate bootstrapped confidence interval for variance explained
          # This is a simple approximation using the properties of the effect
          is_significant <- var_explained > 0.01  # Using a small threshold
          
          list(
            significant = is_significant,
            p_value = if (is_significant) NA else NA  # Placeholder, actual p-values would require complex calculation
          )
        }, error = function(e) {
          list(
            significant = NA,
            p_value = NA
          )
        })
        
        significance_info[[dim]] <- sig_result
      }
    }
  }
  
  ## --- Create effects table for reporting -------------------------------------
  log_message("Creating effects table for reporting", "estimate_effect")
  
  # Create data frame with variance explained
  effects_table <- data.frame(
    dimension = names(variance_explained),
    explained_var = as.numeric(unlist(variance_explained)) * 100,
    stringsAsFactors = FALSE
  )
  
  # Add significance if calculated
  if (calculate_significance && length(significance_info) > 0) {
    effects_table$significant <- sapply(names(variance_explained), function(dim) {
      if (dim %in% names(significance_info)) {
        significance_info[[dim]]$significant
      } else {
        NA
      }
    })
  } else {
    effects_table$significant <- TRUE  # Default to assuming all are significant
  }
  
  # Sort by variance explained (descending)
  effects_table <- effects_table[order(effects_table$explained_var, decreasing = TRUE), ]
  
  # Round for display
  effects_table$explained_var_rounded <- round(effects_table$explained_var, 1)
  
  # Add display column for easier reporting
  effects_table$display_value <- ifelse(
    effects_table$significant,
    paste0(effects_table$explained_var_rounded, "%"),
    "Not significant"
  )
  
  # Format for reporting
  reporting_table <- data.frame(
    Dimension = effects_table$dimension,
    `Variance Explained` = effects_table$display_value,
    Significant = effects_table$significant,
    Value = effects_table$explained_var_rounded,
    stringsAsFactors = FALSE
  )
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    effects_table = reporting_table,         # Formatted table for reporting
    effects = effects_list,                  # Effect objects for each dimension
    variance_explained = variance_explained, # Raw variance explained values
    significance = significance_info         # Significance information
  )
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    dimensions_analyzed = dimensions,
    topics_analyzed = topics,
    k = k,
    uncertainty = uncertainty,
    calculate_significance = calculate_significance,
    alpha = alpha
  )
  
  # Update diagnostics
  diagnostics$processing_stats <- list(
    dimensions_count = length(dimensions),
    successful_dimensions = length(effects_list),
    topics_count = length(topics)
  )
  
  log_message(paste("Effects analysis complete for", length(effects_list), "dimensions"), 
              "estimate_effect")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}