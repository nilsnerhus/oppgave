#' @title Find Optimal Number of Topics for STM (Simplified)
#' @description Determines the optimal number of topics (k) for Structural Topic 
#'   Modeling by running searchK and analyzing semantic coherence and exclusivity.
#'   
#' @param dfm Result from process_dfm containing documents, vocabulary, and metadata
#' @param k_range Vector of c(min, max, step) for k values to test (default: c(5, 20, 5))
#' @param iterations Maximum number of EM iterations for each model (default: 50)
#' @param coherence_weight Weight for semantic coherence in selection (default: 0.5)
#' @param exclusivity_weight Weight for exclusivity in selection (default: 0.5)
#'
#' @return A list containing best_k, metrics table, and searchK results
#'
#' @examples
#' \dontrun{
#' # Find optimal k testing 5, 10, 15, 20
#' k_result <- auto_cache(find_k, dfm, k_range = c(5, 20, 5))
#' print(k_result$data$best_k)
#' }
find_k <- function(
    dfm,
    k_range = c(4, 20, 2),
    iterations = 50,
    coherence_weight = 0.50,
    exclusivity_weight = 0.50
) {
  start_time <- Sys.time()
  
  ## --- Validate inputs --------------------------------------------------------
  if (!is.list(dfm) || !all(c("documents", "vocab", "meta") %in% names(dfm$data))) {
    stop("dfm must be the output from process_dfm()")
  }
  
  if (length(k_range) != 3 || !is.numeric(k_range) || any(k_range <= 0)) {
    stop("k_range must be c(min, max, step) with positive values")
  }
  
  # Generate k sequence
  k_values <- seq(from = k_range[1], to = k_range[2], by = k_range[3])
  if (length(k_values) < 2) {
    stop("k_range must generate at least 2 values")
  }
  
  log_message(paste("Testing k values:", paste(k_values, collapse = ", ")), "find_k")
  
  ## --- Run searchK ------------------------------------------------------------
  search_result <- tryCatch({
    stm::searchK(
      documents = dfm$data$documents,
      vocab = dfm$data$vocab,
      K = k_values,
      prevalence = ~1,
      data = dfm$data$meta,
      max.em.its = iterations,
      verbose = FALSE
    )
  }, error = function(e) {
    stop("STM searchK failed: ", e$message)
  })
  
  ## --- Process results --------------------------------------------------------
  # Extract and convert to numeric vectors
  k_tested <- as.numeric(unlist(search_result$results$K))
  coherence_vals <- as.numeric(unlist(search_result$results$semcoh))
  exclusivity_vals <- as.numeric(unlist(search_result$results$exclus))
  
  # Use heldout if coherence missing or problematic
  if (is.null(coherence_vals) || all(is.na(coherence_vals))) {
    heldout_vals <- as.numeric(unlist(search_result$results$heldout))
    coherence_vals <- -heldout_vals  # Convert to positive (higher is better)
    log_message("Using heldout likelihood instead of semantic coherence", "find_k", "WARNING")
  }
  
  # Debug info
  log_message(paste("Extracted", length(k_tested), "k values,", 
                    length(coherence_vals), "coherence values,", 
                    length(exclusivity_vals), "exclusivity values"), "find_k")
  
  # Validate metrics
  if (is.null(k_tested) || is.null(exclusivity_vals) || is.null(coherence_vals) ||
      length(k_tested) != length(coherence_vals) || length(k_tested) != length(exclusivity_vals)) {
    stop("searchK results have inconsistent or missing metrics")
  }
  
  ## --- Calculate best k -------------------------------------------------------
  # Robust normalize function
  normalize <- function(x) {
    x <- as.numeric(x)  # Ensure numeric
    x <- x[!is.na(x)]   # Remove NAs
    
    if (length(x) == 0) {
      stop("No valid values to normalize")
    }
    
    if (length(x) == 1) {
      return(0.5)  # Single value gets middle score
    }
    
    range_x <- range(x, na.rm = TRUE)
    if (range_x[1] == range_x[2]) {
      return(rep(0.5, length(x)))  # All same values get middle score
    }
    
    (x - range_x[1]) / (range_x[2] - range_x[1])
  }
  
  # Normalize metrics and ensure same length
  norm_coherence <- normalize(coherence_vals)
  norm_exclusivity <- normalize(exclusivity_vals)
  
  # Ensure all vectors have same length (in case normalize removed NAs)
  min_length <- min(length(k_tested), length(norm_coherence), length(norm_exclusivity))
  k_tested <- k_tested[1:min_length]
  norm_coherence <- norm_coherence[1:min_length]
  norm_exclusivity <- norm_exclusivity[1:min_length]
  coherence_vals <- coherence_vals[1:min_length]
  exclusivity_vals <- exclusivity_vals[1:min_length]
  
  # Calculate composite scores and find best
  scores <- (coherence_weight * norm_coherence) + (exclusivity_weight * norm_exclusivity)
  best_k <- k_tested[which.max(scores)]
  
  log_message(paste("Selected k =", best_k), "find_k")
  
  ## --- Create results ---------------------------------------------------------
  metrics_table <- data.frame(
    K = k_tested,
    Coherence = round(coherence_vals, 3),
    Exclusivity = round(exclusivity_vals, 3),
    Score = round(scores, 3),
    Selected = (k_tested == best_k)
  )
  
  return(create_result(
    data = list(
      best_k = best_k,
      metrics = metrics_table,
      searchK_results = search_result
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      k_range = k_range,
      k_tested = k_tested,
      weights = c(coherence = coherence_weight, exclusivity = exclusivity_weight),
      success = TRUE
    ),
    diagnostics = list()
  ))
}