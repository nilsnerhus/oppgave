#' @title Select the best k value from searchK results
#' @description Processes the results from run_searchK() to determine the optimal 
#'   number of topics based on semantic coherence, exclusivity, and residuals.
#'   This function handles different data structures that may be returned by searchK.
#'
#' @param searchK_result Result from run_searchK() containing raw searchK output
#' @param coherence_weight Weight for semantic coherence (default: 0.35)
#' @param exclusivity_weight Weight for exclusivity (default: 0.35)
#' @param residual_weight Weight for residuals (default: 0.30)
#' @param complexity_penalty Penalty for higher topic counts (default: 0.05)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item best_k - Optimal number of topics determined
#'       \item metrics - Data frame with evaluation metrics for each k
#'     }
#'   }
#'   \item{metadata}{Processing information and selection parameters}
#'   \item{diagnostics}{Selection process details and issues encountered}
#'
find_k <- function(
    searchK_result,
    coherence_weight = 0.35,
    exclusivity_weight = 0.35,
    residual_weight = 0.30,
    complexity_penalty = 0.05
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    k_values = NULL,
    best_k_metrics = NULL,
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating searchK results", "find_k")
  
  # Check if searchK_result is valid
  if (!is.list(searchK_result) || 
      !"data" %in% names(searchK_result) ||
      !"searchK_results" %in% names(searchK_result$data)) {
    error_msg <- "searchK_result must be the output from run_searchK() with searchK_results"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "find_k", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Extract searchK results with error handling
  k_search <- searchK_result$data$searchK_results
  k_values <- searchK_result$data$k_values
  
  # Store k values in diagnostics
  diagnostics$k_values <- k_values
  
  # Extract results with robust error handling
  k_results <- tryCatch({
    if ("results" %in% names(k_search)) {
      log_message("Found 'results' component in searchK output", "find_k")
      k_search$results
    } else {
      error_msg <- "No 'results' component found in searchK output"
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
      log_message(error_msg, "find_k", "ERROR")
      NULL
    }
  }, error = function(e) {
    error_msg <- paste("Error extracting results:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "find_k", "ERROR")
    NULL
  })
  
  # Check if we have valid results to process
  if (is.null(k_results)) {
    log_message("No valid results to process", "find_k", "ERROR")
    return(create_result(
      data = list(
        best_k = 15,  # Default fallback
        metrics = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Determine best K -------------------------------------------------------
  log_message("Analyzing results to find optimal K", "find_k")
  
  # Robust extraction function to handle different data structures
  extract_values <- function(metric_name) {
    tryCatch({
      values <- k_results[[metric_name]]
      
      # Handle different types of results
      if (is.list(values)) {
        # If it's a list, try to extract numeric values
        values <- unlist(values)
      }
      
      # Ensure it's numeric
      if (!is.numeric(values)) {
        log_message(paste("Non-numeric", metric_name, "- using default values"), 
                    "find_k", "WARNING")
        values <- rep(0.5, length(k_values))
      }
      
      # Handle missing or invalid values
      if (length(values) != length(k_values) || any(is.na(values)) || any(is.infinite(values))) {
        log_message(paste("Invalid", metric_name, "- using default values"), 
                    "find_k", "WARNING")
        values <- rep(0.5, length(k_values))
      }
      
      return(values)
      
    }, error = function(e) {
      log_message(paste("Error extracting", metric_name, ":", e$message), 
                  "find_k", "WARNING")
      rep(0.5, length(k_values))  # Default values if extraction fails
    })
  }
  
  # Robust normalize function
  normalize <- function(x) {
    tryCatch({
      if (length(x) == 0 || !is.numeric(x) || all(is.na(x))) {
        return(rep(0.5, length(k_values)))
      }
      
      if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
        return(rep(0.5, length(x)))
      }
      
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }, error = function(e) {
      log_message(paste("Normalization error:", e$message), "find_k", "WARNING")
      rep(0.5, length(k_values))
    })
  }
  
  # Extract and normalize metrics with robust error handling
  semcoh <- extract_values("semcoh")
  exclus <- extract_values("exclus")
  residual <- extract_values("residual")
  
  semcoh_norm <- normalize(semcoh)
  exclus_norm <- normalize(exclus)
  residual_norm <- 1 - normalize(residual)  # Invert so higher is better
  
  # Calculate combined score with weights and complexity penalty
  k_norm <- normalize(k_values)  # Normalize k values
  
  combined_score <- tryCatch({
    coherence_weight * semcoh_norm + 
      exclusivity_weight * exclus_norm + 
      residual_weight * residual_norm - 
      complexity_penalty * k_norm
  }, error = function(e) {
    log_message(paste("Error calculating combined score:", e$message), 
                "find_k", "ERROR")
    rep(0.5, length(k_values))
  })
  
  # Find best K with error handling
  best_idx <- tryCatch({
    which.max(combined_score)
  }, error = function(e) {
    log_message(paste("Error finding max score:", e$message), 
                "find_k", "ERROR")
    1  # Default to first k value if error
  })
  
  # Handle invalid index
  if (length(best_idx) == 0 || best_idx < 1 || best_idx > length(k_values)) {
    log_message("Invalid best index, using middle k value", "find_k", "WARNING")
    best_idx <- ceiling(length(k_values)/2)
  }
  
  best_k <- k_values[best_idx]
  log_message(paste("Selected optimal K =", best_k), "find_k")
  
  # Store metrics for best k in diagnostics
  diagnostics$best_k_metrics <- list(
    k = best_k,
    coherence = semcoh[best_idx],
    exclusivity = exclus[best_idx],
    residual = residual[best_idx],
    combined_score = combined_score[best_idx]
  )
  
  ## --- Create metrics dataframe -----------------------------------------------
  log_message("Creating metrics dataframe", "find_k")
  
  # Convert to dataframe for easier plotting and analysis
  metrics_df <- data.frame(
    k = k_values,
    coherence = semcoh,
    exclusivity = exclus, 
    residual = residual,
    semcoh_norm = semcoh_norm,
    exclus_norm = exclus_norm,
    residual_norm = residual_norm,
    combined_score = combined_score,
    is_best = k_values == best_k
  )
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    best_k = best_k,
    metrics = metrics_df
  )
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    weights = list(
      coherence = coherence_weight,
      exclusivity = exclusivity_weight,
      residual = residual_weight,
      complexity_penalty = complexity_penalty
    ),
    success = TRUE
  )
  
  log_message(paste("K selection complete, optimal K =", best_k), "find_k")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}