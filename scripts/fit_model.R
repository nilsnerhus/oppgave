#' @title Fit Structural Topic Model
#' @description Fits a Structural Topic Model (STM) with the specified number of topics
#'   and configuration options. Handles prevalence and content covariates, calculates
#'   effects, and prepares output for analysis.
#'
#' @param stm_data Result from prepare_stm() containing STM input data and metadata
#' @param k Number of topics to use (required)
#' @param iterations Maximum number of EM iterations (default: 200)
#' @param seed Random seed for reproducibility (default: 1234)
#' @param init_type Model initialization method (default: "Spectral")
#' @param prevalence_formula Custom formula for prevalence covariates (default: NULL, auto-generated)
#' @param content_covariates Variables affecting word distribution within topics (default: NULL)
#' @param kappa Regularization parameter (default: NULL)
#' @param gamma_prior Prior on document-topic proportions (default: NULL)
#' @param sigma_prior Prior on topic-word distributions (default: NULL)
#' @param include_full_model Whether to include the full stm object in output (default: TRUE)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item model - Fitted STM model object (if include_full_model is TRUE)
#'       \item topic_proportions - Document-topic proportion matrix (wide format)
#'       \item topic_data - Long-format topic data with document metadata
#'       \item topic_correlations - Topic correlation matrix
#'       \item effects - Estimated effects of covariates on topic prevalence
#'       \item variance_explained - Percentage of variance explained by each covariate
#'     }
#'   }
#'   \item{metadata}{Processing information and model parameters}
#'   \item{diagnostics}{Model quality metrics and processing details}
#'
#' @examples
#' \dontrun{
#' # First prepare data for STM
#' stm_data <- prepare_stm(tokens, metadata)
#' 
#' # Find optimal K
#' k_result <- search_k(stm_data)
#' 
#' # Fit model with optimal K
#' model <- fit_model(stm_data, k = k_result$data$best_k)
#' 
#' # Fit model with custom settings
#' model <- fit_model(stm_data, k = 15, iterations = 500, 
#'                   prevalence_formula = ~ region + wb_income_level)
#' }
fit_model <- function(
    stm_data,
    k,
    iterations = 200,
    seed = 1234,
    init_type = "Spectral",
    prevalence_formula = NULL,
    content_covariates = NULL,
    kappa = NULL,
    gamma_prior = NULL,
    sigma_prior = NULL,
    include_full_model = TRUE
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    model_quality = list(),
    effects_analysis = list(),
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "fit_model")
  
  # Validate stm_data
  if (!is.list(stm_data) || 
      !"data" %in% names(stm_data) ||
      !all(c("stm_input", "metadata") %in% names(stm_data$data))) {
    error_msg <- "stm_data must be the output from prepare_stm() with stm_input and metadata"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate k is provided and is a positive integer
  if (missing(k) || !is.numeric(k) || k < 2 || k != round(k)) {
    error_msg <- "k must be provided as a positive integer of at least 2"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Extract data from input
  stm_input <- stm_data$data$stm_input
  metadata_df <- stm_data$data$metadata
  
  # Extract original metadata and configuration if available
  config <- NULL
  if ("config" %in% names(stm_data$data)) {
    config <- stm_data$data$config
  }
  
  # Extract segmentation information if available
  segments <- NULL
  if ("segments" %in% names(stm_data$data)) {
    segments <- stm_data$data$segments
  }
  
  ## --- Prepare prevalence formula ---------------------------------------------
  log_message("Preparing prevalence formula", "fit_model")
  
  # Initialize formula
  prev_formula <- stats::as.formula("~ 1")  # Default to intercept only
  
  if (!is.null(prevalence_formula)) {
    # Use provided formula
    if (is.character(prevalence_formula)) {
      tryCatch({
        prev_formula <- stats::as.formula(prevalence_formula)
        log_message(paste("Using provided prevalence formula:", prevalence_formula), "fit_model")
      }, error = function(e) {
        warning_msg <- paste("Error parsing prevalence formula:", e$message)
        diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
        log_message(warning_msg, "fit_model", "WARNING")
        log_message("Falling back to intercept-only prevalence formula", "fit_model", "WARNING")
      })
    } else if (inherits(prevalence_formula, "formula")) {
      prev_formula <- prevalence_formula
      log_message(paste("Using provided prevalence formula:", deparse(prev_formula)), "fit_model")
    } else {
      warning_msg <- "Invalid prevalence formula, using intercept-only"
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
    }
  } else if (!is.null(config) && "category_map" %in% names(config)) {
    # Auto-generate from category map
    all_dimensions <- unlist(config$category_map)
    
    # Check which dimensions are available in metadata
    available_dims <- all_dimensions[all_dimensions %in% names(metadata_df)]
    
    if (length(available_dims) > 0) {
      # Create formula with all available dimensions
      formula_str <- paste("~", paste(available_dims, collapse = " + "))
      prev_formula <- stats::as.formula(formula_str)
      log_message(paste("Using auto-generated formula from category_map:", formula_str), "fit_model")
    } else {
      log_message("No usable dimensions for prevalence formula, using intercept-only", "fit_model")
    }
  } else {
    log_message("No prevalence formula or category_map provided, using intercept-only", "fit_model")
  }
  
  ## --- Prepare content covariates ---------------------------------------------
  log_message("Preparing content covariates", "fit_model")
  
  # Initialize content covariates
  model_content <- NULL
  
  # Process content covariates if provided
  if (!is.null(content_covariates)) {
    # Check which content covariates are available in metadata
    available_content <- content_covariates[content_covariates %in% names(metadata_df)]
    
    if (length(available_content) > 0) {
      # Create a formula for content covariates
      content_formula <- stats::as.formula(paste("~", paste(available_content, collapse = " + ")))
      
      # Create design matrix (without intercept)
      model_content <- stats::model.matrix(content_formula, data = metadata_df)[, -1, drop = FALSE]
      
      log_message(paste("Using content covariates:", paste(available_content, collapse = ", ")), "fit_model")
    } else {
      warning_msg <- "Specified content covariates not available in metadata"
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
    }
  }
  
  ## --- Fit STM model ----------------------------------------------------------
  log_message(paste("Fitting STM model with k =", k), "fit_model")
  
  # Set seed for reproducibility 
  set.seed(seed)
  
  # Fit model with configured options
  stm_model <- tryCatch({
    stm::stm(
      documents = stm_input$documents,
      vocab = stm_input$vocab,
      K = k,
      prevalence = prev_formula,
      content = model_content,
      data = metadata_df,
      max.em.its = iterations,
      init.type = init_type,
      verbose = FALSE,
      gamma.prior = gamma_prior,
      sigma.prior = sigma_prior,
      kappa = kappa,
      seed = seed
    )
  }, error = function(e) {
    error_msg <- paste("Error fitting STM model:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    stop(error_msg)
  })
  
  ## --- Calculate model quality metrics ----------------------------------------
  log_message("Calculating model quality metrics", "fit_model")
  
  # Calculate semantic coherence
  coherence <- tryCatch({
    mean(stm::semanticCoherence(stm_model, stm_input$documents))
  }, error = function(e) {
    warning_msg <- paste("Error calculating coherence:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    NA
  })
  
  # Calculate exclusivity
  exclusivity <- tryCatch({
    mean(stm::exclusivity(stm_model))
  }, error = function(e) {
    warning_msg <- paste("Error calculating exclusivity:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    NA
  })
  
  # Calculate held-out likelihood if available
  heldout <- tryCatch({
    if (!is.null(stm_model$heldout)) {
      stm_model$heldout$bound
    } else {
      NA
    }
  }, error = function(e) {
    NA
  })
  
  # Store quality metrics
  model_quality <- list(
    coherence = coherence,
    exclusivity = exclusivity,
    heldout = heldout,
    iterations = stm_model$convergence$its,
    converged = stm_model$convergence$converged
  )
  
  diagnostics$model_quality <- model_quality
  
  ## --- Extract topic information ----------------------------------------------
  log_message("Extracting topic information", "fit_model")
  
  # Extract document-topic proportions
  topic_proportions <- stm_model$theta
  
  # Add document IDs
  topic_props_df <- as.data.frame(topic_proportions)
  topic_props_df$doc_id <- rownames(stm_input$documents.dimnames)
  
  # Rename topic columns
  names(topic_props_df)[1:k] <- paste0("Topic_", 1:k)
  
  ## --- Join with metadata -----------------------------------------------------
  log_message("Joining topic proportions with document metadata", "fit_model")
  
  # Get metadata columns (excluding doc_id)
  metadata_cols <- setdiff(names(metadata_df), names(topic_props_df))
  
  # Join with metadata
  if (length(metadata_cols) > 0) {
    topic_props_with_meta <- dplyr::left_join(
      topic_props_df,
      metadata_df[, c("doc_id", metadata_cols)],
      by = "doc_id"
    )
  } else {
    topic_props_with_meta <- topic_props_df
  }
  
  # Create long format for easier analysis
  topic_data <- topic_props_with_meta %>%
    tidyr::pivot_longer(
      cols = starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  ## --- Aggregate segments if used ---------------------------------------------
  if (!is.null(segments)) {
    log_message("Aggregating segment-level results to document level", "fit_model")
    
    # Merge topic proportions with segment mapping
    segment_props <- dplyr::left_join(
      topic_props_df,
      segments,
      by = c("doc_id" = "segment_id")
    )
    
    # Get topic columns
    topic_cols <- paste0("Topic_", 1:k)
    
    # Aggregate to document level using weighted average
    doc_topic_props <- segment_props %>%
      dplyr::group_by(original_doc_id) %>%
      dplyr::summarize(
        across(all_of(topic_cols), ~ weighted.mean(., token_count, na.rm = TRUE)),
        .groups = "drop"
      )
    
    # Rename for consistency
    names(doc_topic_props)[names(doc_topic_props) == "original_doc_id"] <- "doc_id"
    
    # Create aggregated long format
    aggregated_topic_data <- doc_topic_props %>%
      tidyr::pivot_longer(
        cols = starts_with("Topic_"),
        names_to = "Topic",
        values_to = "Proportion"
      )
    
    # Add aggregated version to the output
    aggregated_topic_props <- doc_topic_props
  }
  
  ## --- Run effects analysis ---------------------------------------------------
  log_message("Running effects analysis", "fit_model")
  
  # Initialize lists for effects and variance explained
  effects_list <- list()
  variance_explained <- list()
  
  # Helper function to calculate variance explained
  calculate_variance <- function(effects_obj, topic_count) {
    total_variance <- 0
    
    # Loop through topics
    for (i in 1:topic_count) {
      # Get summary for this topic
      s <- summary(effects_obj, topics = i)
      
      # Add this topic's explained variance
      if (!is.null(s$r2)) {
        total_variance <- total_variance + s$r2
      }
    }
    
    # Return average across topics
    return(total_variance / topic_count)
  }
  
  # Get formula terms from prevalence formula
  formula_terms <- all.vars(prev_formula)
  
  # Process each term in the formula
  for (term in formula_terms) {
    # Skip intercept
    if (term == "1") next
    
    log_message(paste("Analyzing effects of", term), "fit_model")
    
    # Skip if term not in metadata
    if (!term %in% names(topic_props_with_meta)) {
      warning_msg <- paste("Term", term, "not found in metadata, skipping effects analysis")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      next
    }
    
    # Skip if term has insufficient variation
    if (is.factor(topic_props_with_meta[[term]]) && length(levels(topic_props_with_meta[[term]])) < 2) {
      warning_msg <- paste("Term", term, "has insufficient variation for effects analysis")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      next
    }
    
    if (is.logical(topic_props_with_meta[[term]]) && length(unique(topic_props_with_meta[[term]])) < 2) {
      warning_msg <- paste("Term", term, "has insufficient variation for effects analysis")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      next
    }
    
    # Create formula for this term
    term_formula <- stats::as.formula(paste("~", term))
    
    # Try to estimate effects
    effects_result <- tryCatch({
      # Estimate effects for all topics
      effects <- stm::estimateEffect(
        term_formula, 
        stmobj = stm_model, 
        metadata = topic_props_with_meta,
        uncertainty = 20  # Default value
      )
      
      # Calculate variance explained
      var_explained <- calculate_variance(effects, k)
      
      list(
        effects = effects,
        variance = var_explained
      )
    }, error = function(e) {
      warning_msg <- paste("Error estimating effects for", term, ":", e$message)
      diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      NULL
    })
    
    # Store results if successful
    if (!is.null(effects_result)) {
      effects_list[[term]] <- effects_result$effects
      variance_explained[[term]] <- effects_result$variance
      
      log_message(paste(term, "explains approximately", 
                        round(effects_result$variance * 100, 1), "% of topic variance"), 
                  "fit_model")
    }
  }
  
  # Store effect analysis summary in diagnostics
  diagnostics$effects_analysis$dimensions <- names(effects_list)
  diagnostics$effects_analysis$variance_explained <- variance_explained
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Determine what to include in the result
  result_data <- list(
    # Include full model only if requested
    model = if(include_full_model) stm_model else NULL,
    
    # Always include these components
    topic_proportions = topic_props_with_meta,
    topic_data = topic_data,
    topic_correlations = stm_model$beta$cor,
    effects = effects_list,
    variance_explained = variance_explained
  )
  
  # Add aggregated results if segmentation was used
  if (!is.null(segments)) {
    result_data$aggregated_proportions <- aggregated_topic_props
    result_data$aggregated_topic_data <- aggregated_topic_data
  }
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k = k,
    prevalence_formula = deparse(prev_formula),
    content_covariates = if(!is.null(content_covariates)) content_covariates else NULL,
    segmentation_used = !is.null(segments),
    iterations = iterations,
    documents = nrow(topic_props_df),
    segments = if(!is.null(segments)) nrow(segments) else NA,
    seed = seed,
    model_included = include_full_model
  )
  
  # Add diagnostics summary 
  diagnostics$summary <- list(
    model_converged = model_quality$converged,
    coherence = model_quality$coherence,
    exclusivity = model_quality$exclusivity,
    dimensions_analyzed = length(effects_list),
    issues_count = length(diagnostics$processing_issues)
  )
  
  log_message(sprintf("Model fitting complete: k = %d, coherence = %.3f, exclusivity = %.3f", 
                      k, coherence, exclusivity), 
              "fit_model")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}