#' @title Fit Structural Topic Model
#' @description Fits a Structural Topic Model (STM) with the specified number of topics
#'   and configuration options. Accepts either a direct k value or the output from find_k().
#'   Automatically generates prevalence formulas based on category map if available.
#'   
#' @param dfm Result from process_dfm containing documents, vocabulary, and metadata
#' @param k Number of topics or result from find_k (default: 15)
#' @param iterations Maximum number of EM iterations (default: 200)
#' @param prevalence Formula for topic prevalence (default: NULL, auto-generated from category map)
#' @param seed Random seed for reproducibility (default: 12345)
#' @param init_type Model initialization method (default: "Spectral")
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item model - Fitted STM model object
#'       \item summary - Basic model summary statistics
#'       \item topics - Top terms for each topic
#'       \item effects - Estimated covariate effects (if applicable)
#'     }
#'   }
#'   \item{metadata}{Processing information and model parameters}
#'   \item{diagnostics}{Model quality metrics and processing details}
#'
#' @examples
#' \dontrun{
#' # Process documents first
#' dfm <- auto_cache(process_dfm, tokens, metadata)
#' 
#' # Find optimal k
#' k_result <- auto_cache(find_k, dfm)
#' 
#' # Fit model with optimal k and auto-generated prevalence
#' model <- auto_cache(fit_model, dfm, k = k_result)
#' 
#' # Or with custom prevalence formula
#' model <- auto_cache(fit_model, dfm, k = 15, prevalence = ~region + s(year))
#' }
fit_model <- function(
    dfm,
    k = 15,
    iterations = 200,
    prevalence = NULL,
    seed = 12345,
    init_type = "Spectral"
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    model_stats = list(),
    prevalence_info = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "fit_model")
  
  # Validate dfm structure
  if (!is.list(dfm) || 
      !"data" %in% names(dfm) ||
      !all(c("documents", "vocab", "meta") %in% names(dfm$data))) {
    error_msg <- "dfm must be the output from process_dfm() with documents, vocab, and meta components"
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
  
  # Extract k value - handle either direct numeric or result from find_k
  topics_k <- k
  if (is.list(k) && "data" %in% names(k) && "best_k" %in% names(k$data)) {
    topics_k <- k$data$best_k
    log_message(paste("Using k =", topics_k, "from find_k result"), "fit_model")
  } else if (is.numeric(k)) {
    topics_k <- as.numeric(k)
    log_message(paste("Using specified k =", topics_k), "fit_model")
  } else {
    error_msg <- "k must be either a numeric value or the result from find_k()"
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
  
  # Validate that k is a positive integer
  if (!is.numeric(topics_k) || topics_k < 1 || topics_k != round(topics_k)) {
    error_msg <- paste("Invalid k value:", topics_k, "- must be a positive integer")
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
  
  # Extract components from dfm
  docs <- dfm$data$documents
  vocab <- dfm$data$vocab
  meta <- dfm$data$meta
  
  ## --- Generate prevalence formula -------------------------------------------
  log_message("Preparing prevalence formula", "fit_model")
  
  # Use provided formula if not NULL
  if (!is.null(prevalence)) {
    if (inherits(prevalence, "formula")) {
      log_message(paste("Using provided prevalence formula:", deparse(prevalence)), "fit_model")
      prev_formula <- prevalence
    } else if (is.character(prevalence)) {
      # Convert string to formula
      prev_formula <- stats::as.formula(prevalence)
      log_message(paste("Converted string to prevalence formula:", prevalence), "fit_model")
    } else {
      warning_msg <- "Invalid prevalence format, using intercept-only model"
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      prev_formula <- ~1
    }
  } else {
    # Try to generate formula from category map
    config <- NULL
    
    # Look for category map in dfm
    if ("metadata" %in% names(dfm) && "category_map" %in% names(dfm$metadata)) {
      config <- dfm$metadata$category_map
    } else if ("config" %in% names(dfm$data)) {
      if ("category_map" %in% names(dfm$data$config)) {
        config <- dfm$data$config$category_map
      }
    }
    
    if (!is.null(config)) {
      log_message("Found category map, generating prevalence formula", "fit_model")
      
      # Extract dimensions to use in formula
      all_dimensions <- unlist(config)
      valid_dimensions <- character(0)
      
      # Validate each dimension
      for (dim in all_dimensions) {
        if (dim %in% names(meta)) {
          # Check for sufficient variation
          if (is.factor(meta[[dim]]) || is.character(meta[[dim]])) {
            n_levels <- length(unique(meta[[dim]]))
            if (n_levels >= 2) {
              valid_dimensions <- c(valid_dimensions, dim)
            } else {
              msg <- paste("Dimension", dim, "has insufficient variation, skipping")
              log_message(msg, "fit_model")
            }
          } else if (is.numeric(meta[[dim]])) {
            if (length(unique(meta[[dim]])) > 5) {
              # Use splines for continuous variables with many levels
              valid_dimensions <- c(valid_dimensions, paste0("s(", dim, ")"))
              log_message(paste("Using spline for continuous dimension:", dim), "fit_model")
            } else if (length(unique(meta[[dim]])) >= 2) {
              valid_dimensions <- c(valid_dimensions, dim)
            } else {
              msg <- paste("Dimension", dim, "has insufficient variation, skipping")
              log_message(msg, "fit_model")
            }
          } else if (is.logical(meta[[dim]])) {
            if (length(unique(meta[[dim]])) >= 2) {
              valid_dimensions <- c(valid_dimensions, dim)
            } else {
              msg <- paste("Dimension", dim, "has insufficient variation, skipping")
              log_message(msg, "fit_model")
            }
          }
        } else {
          msg <- paste("Dimension", dim, "not found in metadata, skipping")
          log_message(msg, "fit_model")
        }
      }
      
      # Create formula from valid dimensions
      if (length(valid_dimensions) > 0) {
        formula_str <- paste("~", paste(valid_dimensions, collapse = " + "))
        prev_formula <- stats::as.formula(formula_str)
        log_message(paste("Generated prevalence formula:", formula_str), "fit_model")
        
        # Store dimensions used in diagnostics
        diagnostics$prevalence_info$dimensions_used <- valid_dimensions
      } else {
        prev_formula <- ~1
        log_message("No valid dimensions found, using intercept-only model", "fit_model")
      }
    } else {
      # Default to intercept-only if no category map found
      prev_formula <- ~1
      log_message("No category map found, using intercept-only model", "fit_model")
    }
  }
  
  # Store formula for diagnostics
  diagnostics$prevalence_info$formula <- deparse(prev_formula)
  
  ## --- Fit STM model ----------------------------------------------------------
  log_message(paste("Fitting STM model with k =", topics_k), "fit_model")
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Fit model with error handling
  model_result <- tryCatch({
    stm::stm(
      documents = docs,
      vocab = vocab,
      K = topics_k,
      prevalence = prev_formula,
      data = meta,
      max.em.its = iterations,
      init.type = init_type,
      verbose = FALSE
    )
  }, error = function(e) {
    error_msg <- paste("Error fitting STM model:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    
    # Try with intercept-only as fallback
    if (deparse(prev_formula) != "~1") {
      log_message("Attempting fallback with intercept-only model", "fit_model", "WARNING")
      tryCatch({
        stm::stm(
          documents = docs,
          vocab = vocab,
          K = topics_k,
          prevalence = ~1,
          data = meta,
          max.em.its = iterations,
          init.type = init_type,
          verbose = FALSE
        )
      }, error = function(e2) {
        log_message(paste("Fallback also failed:", e2$message), "fit_model", "ERROR")
        NULL
      })
    } else {
      NULL
    }
  })
  
  # Check if model fitting was successful
  if (is.null(model_result)) {
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Extract model information ----------------------------------------------
  log_message("Extracting model information", "fit_model")
  
  # Calculate basic model quality metrics
  coherence <- tryCatch({
    mean(stm::semanticCoherence(model_result, docs))
  }, error = function(e) {
    warning_msg <- paste("Error calculating coherence:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    NA
  })
  
  exclusivity <- tryCatch({
    mean(stm::exclusivity(model_result))
  }, error = function(e) {
    warning_msg <- paste("Error calculating exclusivity:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    NA
  })
  
  # Extract top terms for each topic
  topic_terms <- tryCatch({
    stm::labelTopics(model_result)
  }, error = function(e) {
    warning_msg <- paste("Error extracting topic terms:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    NULL
  })
  
  # Create model summary
  model_summary <- list(
    k = topics_k,
    document_count = length(docs),
    vocabulary_size = length(vocab),
    iterations_run = model_result$convergence$its,
    converged = model_result$convergence$converged,
    coherence = coherence,
    exclusivity = exclusivity
  )
  
  # Calculate covariate effects if using non-intercept-only model
  effects <- NULL
  if (deparse(prev_formula) != "~1") {
    effects <- tryCatch({
      stm::estimateEffect(prev_formula, model_result, meta)
    }, error = function(e) {
      warning_msg <- paste("Error estimating effects:", e$message)
      diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      NULL
    })
  }
  
  # Store model quality metrics in diagnostics
  diagnostics$model_stats <- model_summary
  
  log_message(paste("Model fitting complete with k =", topics_k, 
                    "- coherence =", round(coherence, 3),
                    "exclusivity =", round(exclusivity, 3)), 
              "fit_model")
  
  ## --- Calculate processing time and create result ---------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Return standardized result
  return(create_result(
    data = list(
      model = model_result,
      summary = model_summary,
      topics = topic_terms,
      effects = effects
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      k = topics_k,
      max_iterations = iterations,
      prevalence_formula = deparse(prev_formula),
      init_type = init_type,
      seed = seed,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}