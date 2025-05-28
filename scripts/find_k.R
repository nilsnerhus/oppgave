#' @title Find Optimal Number of Topics for STM
#' @description Determines the optimal number of topics (k) for Structural Topic 
#'   Modeling by running searchK and analyzing semantic coherence, exclusivity,
#'   and other metrics. Uses a weighted composite score to balance interpretability
#'   and statistical fit.
#'   
#' @param dfm Result from process_dfm containing documents, vocabulary, and metadata
#' @param range Vector of k values to try (default: c(5, 10, 15, 20, 25))
#' @param iterations Maximum number of EM iterations for each model (default: 75)
#' @param coherence Weight for semantic coherence in selection (default: 0.4)
#' @param exclusivity Weight for exclusivity in selection (default: 0.4)
#' @param residual Weight for residuals in score (default: 0.2)
#' @param penalty Penalty factor for higher k values (default: 0.05)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item best_k - Recommended number of topics (single numeric value)
#'       \item metrics - Data frame with metrics for each k value
#'       \item searchK_results - Raw output from stm::searchK for further analysis
#'     }
#'   }
#'   \item{metadata}{Processing information and parameters used}
#'   \item{diagnostics}{Issues encountered and processing details}
#'
#' @examples
#' \dontrun{
#' # Process documents first
#' dfm <- auto_cache(process_dfm, tokens, metadata)
#' 
#' # Find optimal k
#' k_result <- auto_cache(find_k, dfm)
#' print(k_result$data$best_k) # Single numeric value
#' 
#' # Use custom range and weights
#' k_result <- auto_cache(find_k, dfm, range = c(10, 15, 20, 25, 30))
#' }
find_k <- function(
    dfm,
    range = c(10, 15, 20, 25, 30, 35, 40),
    iterations = 100,
    coherence = 0.20,
    exclusivity = 0.15,
    residual = 0.15,
    penalty = 0
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    k_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "find_k")
  
  # Validate dfm structure
  if (!is.list(dfm) || 
      !"data" %in% names(dfm) ||
      !all(c("documents", "vocab", "meta") %in% names(dfm$data))) {
    error_msg <- "dfm must be the output from process_dfm() with documents, vocab, and meta components"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "find_k", "ERROR")
    
    return(create_result(
      data = list(
        best_k = NA_integer_,
        metrics = NULL,
        searchK_results = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate range
  if (!is.numeric(range) || length(range) < 1) {
    error_msg <- "range must be a numeric vector with at least one k value"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "find_k", "ERROR")
    
    return(create_result(
      data = list(
        best_k = NA_integer_,
        metrics = NULL,
        searchK_results = NULL
      ),
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
  
  # Validate documents
  if (length(docs) == 0) {
    error_msg <- "Document list is empty"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "find_k", "ERROR")
    
    return(create_result(
      data = list(
        best_k = NA_integer_,
        metrics = NULL,
        searchK_results = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Run searchK ------------------------------------------------------------
  log_message(paste("Running searchK with range:", paste(range, collapse = ", ")), "find_k")
  
  # Run searchK with error handling
  search_result <- tryCatch({
    stm::searchK(
      documents = docs,
      vocab = vocab,
      K = range,
      prevalence = ~1,  # Simple model for comparison
      data = meta,
      max.em.its = iterations,
      verbose = FALSE
    )
  }, error = function(e) {
    error_msg <- paste("Error in STM searchK:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "find_k", "ERROR")
    NULL
  })
  
  # Check if searchK was successful
  if (is.null(search_result)) {
    log_message("searchK failed, attempting alternate approach", "find_k", "WARNING")
    
    # Try running individual models as fallback
    metrics_list <- list()
    
    for (k in range) {
      log_message(paste("Trying k =", k), "find_k")
      
      model_result <- tryCatch({
        # Fit individual model
        model <- stm::stm(
          documents = docs,
          vocab = vocab,
          K = k,
          data = meta,
          max.em.its = iterations,
          verbose = FALSE
        )
        
        # Calculate metrics manually
        coherence_val <- mean(stm::semanticCoherence(model, docs))
        exclusivity_val <- mean(stm::exclusivity(model))
        residual_val <- mean(model$convergence$bound) # Approximation
        
        # Return as list
        list(
          k = k,
          coherence = coherence_val,
          exclusivity = exclusivity_val,
          residual = residual_val
        )
      }, error = function(e) {
        warning_msg <- paste("Error fitting model with k =", k, ":", e$message)
        diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
        log_message(warning_msg, "find_k", "WARNING")
        NULL
      })
      
      if (!is.null(model_result)) {
        metrics_list[[length(metrics_list) + 1]] <- model_result
      }
    }
    
    # Check if we have any successful models
    if (length(metrics_list) == 0) {
      error_msg <- "All models failed to fit, cannot determine optimal k"
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
      log_message(error_msg, "find_k", "ERROR")
      
      return(create_result(
        data = list(
          best_k = NA_integer_,
          metrics = NULL,
          searchK_results = NULL
        ),
        metadata = list(
          timestamp = Sys.time(),
          success = FALSE
        ),
        diagnostics = diagnostics
      ))
    }
    
    # Convert list to data frame
    metrics_df <- do.call(rbind, lapply(metrics_list, function(x) {
      data.frame(
        k = x$k,
        coherence = x$coherence,
        exclusivity = x$exclusivity,
        residual = x$residual
      )
    }))
    
    # Create a minimal searchK-like result
    search_result <- list(
      results = metrics_df,
      K = range[range %in% metrics_df$k]
    )
    
    log_message("Created metrics from individual models", "find_k")
  }
  
  ## --- Analyze searchK results to find best k ---------------------------------
  log_message("Analyzing searchK results to find optimal k", "find_k")
  
  # Check if we have valid searchK results
  if (is.null(search_result) || !("results" %in% names(search_result))) {
    log_message("No valid searchK results found, using default k", "find_k", "WARNING")
    best_k <- ifelse(length(range) > 0, range[ceiling(length(range)/2)], 15)
    metrics_display <- NULL  # No metrics to display
  } else {
    log_message("Found searchK results, extracting key metrics", "find_k")
    
    # Get the length of results (number of K values tested)
    num_k <- length(search_result$results$K)
    log_message(paste("Found", num_k, "K values to evaluate"), "find_k")
    
    # Create simple vectors for metrics
    k_values <- unlist(search_result$results$K)
    exclusivity_vals <- unlist(search_result$results$exclus)
    coherence_vals <- unlist(search_result$results$semcoh)
    residual_vals <- unlist(search_result$results$residual)
    
    # Ensure we have valid metrics
    valid_metrics <- TRUE
    if (is.null(k_values) || length(k_values) == 0) {
      log_message("Missing K values in results", "find_k", "WARNING")
      valid_metrics <- FALSE
    }
    if (is.null(coherence_vals) || length(coherence_vals) == 0) {
      # Try heldout as alternative
      coherence_vals <- -unlist(search_result$results$heldout)
      if (is.null(coherence_vals) || length(coherence_vals) == 0) {
        log_message("Missing coherence metrics in results", "find_k", "WARNING")
        valid_metrics <- FALSE
      }
    }
    if (is.null(exclusivity_vals) || length(exclusivity_vals) == 0) {
      log_message("Missing exclusivity metrics in results", "find_k", "WARNING")
      valid_metrics <- FALSE
    }
    
    if (valid_metrics) {
      # Normalize metrics (0-1 scale)
      norm_coherence <- (coherence_vals - min(coherence_vals)) / (max(coherence_vals) - min(coherence_vals))
      norm_exclusivity <- (exclusivity_vals - min(exclusivity_vals)) / (max(exclusivity_vals) - min(exclusivity_vals))
      
      # For residuals, invert so higher is better
      if (!is.null(residual_vals) && length(residual_vals) > 0) {
        norm_residual <- 1 - (residual_vals - min(residual_vals)) / (max(residual_vals) - min(residual_vals))
      } else {
        # Set to neutral value if missing
        norm_residual <- rep(0.5, length(k_values))
      }
      
      # Calculate scores using the FUNCTION PARAMETERS (not renamed variables)
      scores <- (coherence * norm_coherence) + 
        (exclusivity * norm_exclusivity) + 
        (residual * norm_residual) - 
        (penalty * k_values)
      
      # Find best k
      best_idx <- which.max(scores)
      best_k <- k_values[best_idx]
      
      log_message(paste("Selected best k =", best_k, "with score =", 
                        round(scores[best_idx], 4)), "find_k")
      
      # Create a simple display table (no fancy formatting)
      metrics_display <- data.frame(
        K = k_values,
        Coherence = round(coherence_vals, 3),
        Exclusivity = round(exclusivity_vals, 3),
        Score = round(scores, 3),
        Selected = (k_values == best_k)
      )
    } else {
      # Default if metrics are missing
      log_message("Could not extract valid metrics, using default k", "find_k", "WARNING")
      best_k <- ifelse(length(range) > 0, range[ceiling(length(range)/2)], 15)
      metrics_display <- NULL
    }
  }
  
  # Store results in diagnostics
  diagnostics$k_stats <- list(
    best_k = best_k,
    weights = list(
      coherence = coherence,
      exclusivity = exclusivity,
      residual = residual,
      penalty = penalty
    )
  )
  
  # Log the selection
  log_message(paste("Final selection: k =", best_k), "find_k")

  ## --- Calculate processing time and create result ---------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # For now, just return search results without calculating the best k
  log_message("Returning searchK results without calculating best k", "find_k")
  
  # Return standardized result
  return(create_result(
    data = list(
      best_k = best_k,  # This should be set correctly now
      metrics = metrics_display,
      searchK_results = search_result
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      k_range = range,
      max_iterations = iterations,
      weights = list(
        coherence = coherence,
        exclusivity = exclusivity,
        residual = residual,
        penalty = penalty
      ),
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}