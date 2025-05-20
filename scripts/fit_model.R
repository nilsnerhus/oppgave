#' @title Fit Structural Topic Model
#' @description Fits an STM model with customizable parameters, generating prevalence formula from category map.
#'   
#' @param dfm Result from process_dfm containing documents, vocabulary, and metadata
#' @param k Number of topics (default: 15)
#' @param category_map List mapping categories to dimension names (default: NULL)
#' @param iterations Maximum number of EM iterations (default: 200)
#' @param seed Random seed for reproducibility (default: 12345)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item model - Fitted STM model object
#'       \item summary - Basic model summary statistics
#'     }
#'   }
#'   \item{metadata}{Processing information and model parameters}
#'   \item{diagnostics}{Model quality metrics and processing details}
fit_model <- function(dfm, k = 15, category_map = NULL, iterations = 200, seed = 12345) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    model_stats = list(),
    prevalence_info = list()
  )
  
  # Set seed for reproducibility
  set.seed(seed)
  
  ## --- Input validation -------------------------------------------------------
  # Validate k is a positive number
  if (!is.numeric(k) || k <= 0 || k != round(k)) {
    warning_msg <- "k must be a positive integer, using default k = 15"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    k <- 15
  }
  
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
  
  ## --- Extract components -----------------------------------------------------
  # Extract components from dfm
  docs <- dfm$data$documents
  vocab <- dfm$data$vocab
  meta <- dfm$data$meta
  
  log_message(paste("Fitting STM model with k =", k), "fit_model")
  
  ## --- Generate prevalence formula --------------------------------------------
  if (is.null(category_map)) {
    # Use intercept-only model
    prev_formula <- ~1
    log_message("No category map provided, using intercept-only model", "fit_model")
  } else {
    # Extract all dimensions from category map
    all_dimensions <- unlist(category_map)
    log_message(paste("Found", length(all_dimensions), "dimensions in category map"), "fit_model")
    
    # Check which dimensions exist in metadata
    valid_dimensions <- character(0)
    for (dim in all_dimensions) {
      if (dim %in% names(meta)) {
        valid_dimensions <- c(valid_dimensions, dim)
      } else {
        log_message(paste("Dimension", dim, "not found in metadata, skipping"), "fit_model")
      }
    }
    
    # Build formula
    if (length(valid_dimensions) > 0) {
      formula_str <- paste("~", paste(valid_dimensions, collapse = " + "))
      prev_formula <- stats::as.formula(formula_str)
      log_message(paste("Generated prevalence formula:", formula_str), "fit_model")
    } else {
      prev_formula <- ~1
      log_message("No valid dimensions found, using intercept-only model", "fit_model")
    }
  }
  
  # Store formula in diagnostics
  diagnostics$prevalence_info$formula <- deparse(prev_formula)
  
  ## --- Fit STM model ----------------------------------------------------------
  model_result <- tryCatch({
    stm::stm(
      documents = docs,
      vocab = vocab,
      K = k,
      prevalence = prev_formula,
      data = meta,
      max.em.its = iterations,
      verbose = FALSE
    )
  }, error = function(e) {
    error_msg <- paste("Error fitting STM model:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    NULL
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
  
  log_message("STM model fitting complete", "fit_model")
  
  ## --- Create model summary ---------------------------------------------------
  model_summary <- list(
    k = k,
    document_count = length(docs),
    vocabulary_size = length(vocab),
    iterations_run = model_result$convergence$its,
    converged = model_result$convergence$converged,
    prevalence_formula = deparse(prev_formula)
  )
  
  # Store model quality metrics in diagnostics
  diagnostics$model_stats <- model_summary
  
  ## --- Calculate processing time and create result ---------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Return standardized result
  return(create_result(
    data = list(
      model = model_result,
      summary = model_summary
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      k = k,
      max_iterations = iterations,
      prevalence_formula = deparse(prev_formula),
      seed = seed,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}