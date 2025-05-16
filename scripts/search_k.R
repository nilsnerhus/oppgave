#' @title Search for optimal number of topics for STM
#' @description Determines the optimal number of topics for a structural topic model
#'   by evaluating model quality metrics across a range of topic counts. Uses a
#'   combination of semantic coherence, exclusivity, and residuals to select the best k.
#'
#' @param stm_data Result from prepare_stm() containing STM input data and metadata
#' @param min_k Minimum number of topics to try (default: 5)
#' @param max_k Maximum number of topics to try (default: 40)
#' @param step_k Step size for k search (default: 5)
#' @param seed Random seed for reproducibility (default: 1234)
#' @param iterations Maximum EM algorithm iterations (default: 100)
#' @param coherence_weight Weight for semantic coherence (default: 0.35)
#' @param exclusivity_weight Weight for exclusivity (default: 0.35)
#' @param residual_weight Weight for held-out likelihood (default: 0.30)
#' @param complexity_penalty Penalty for higher topic counts (default: 0.05)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item best_k - Optimal number of topics determined
#'       \item metrics - Data frame with evaluation metrics for each k
#'       \item plots - Visualizations of metrics across k values
#'     }
#'   }
#'   \item{metadata}{Processing information and search parameters}
#'   \item{diagnostics}{K selection details and processing issues}
#'
#' @examples
#' \dontrun{
#' # First prepare data for STM
#' stm_data <- prepare_stm(tokens, metadata)
#' 
#' # Search for optimal k with defaults
#' k_result <- search_k(stm_data)
#' 
#' # Custom search configuration
#' k_result <- search_k(stm_data, min_k = 10, max_k = 50, step_k = 10,
#'                      coherence_weight = 0.4, exclusivity_weight = 0.4, 
#'                      residual_weight = 0.2)
#' 
#' # Use the optimal k value in fit_model
#' model <- fit_model(stm_data, k = k_result$data$best_k)
#' }
search_k <- function(
    stm_data,
    min_k = 5,
    max_k = 40,
    step_k = 5,
    seed = 1234,
    iterations = 100,
    coherence_weight = 0.35,
    exclusivity_weight = 0.35,
    residual_weight = 0.30,
    complexity_penalty = 0.05
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    k_values = NULL,
    best_k_metrics = NULL,
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "search_k")
  
  # Validate stm_data
  if (!is.list(stm_data) || 
      !"data" %in% names(stm_data) ||
      !all(c("stm_input", "metadata") %in% names(stm_data$data))) {
    error_msg <- "stm_data must be the output from prepare_stm() with stm_input and metadata"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "search_k", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate parameter ranges
  if (min_k < 2) {
    warning_msg <- "min_k must be at least 2, setting to 2"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "search_k", "WARNING")
    min_k <- 2
  }
  
  if (max_k <= min_k) {
    warning_msg <- paste("max_k must be greater than min_k, setting to", min_k + 10)
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "search_k", "WARNING")
    max_k <- min_k + 10
  }
  
  if (step_k <= 0) {
    warning_msg <- "step_k must be positive, setting to 1"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "search_k", "WARNING")
    step_k <- 1
  }
  
  # Validate weights sum to 1
  weights_sum <- coherence_weight + exclusivity_weight + residual_weight
  
  if (abs(weights_sum - 1) > 0.01) {
    warning_msg <- paste("Weights do not sum to 1 (sum =", weights_sum, ").",
                         "Normalizing weights.")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "search_k", "WARNING")
    
    # Normalize weights
    coherence_weight <- coherence_weight / weights_sum
    exclusivity_weight <- exclusivity_weight / weights_sum
    residual_weight <- residual_weight / weights_sum
  }
  
  # Extract data from input
  stm_input <- stm_data$data$stm_input
  metadata <- stm_data$data$metadata
  
  ## --- Create a simplified prevalence formula ---------------------------------
  log_message("Creating prevalence formula for search", "search_k")
  
  # A simplified prevalence formula improves search_k performance
  # We'll use the first variable from the category map if available
  
  prevalence_formula <- stats::as.formula("~ 1")  # Default to intercept only
  
  if ("config" %in% names(stm_data$data)) {
    config <- stm_data$data$config
    
    if ("category_map" %in% names(config)) {
      # Get all dimensions from category_map
      all_dimensions <- unlist(config$category_map)
      
      # Check which dimensions are available in metadata
      available_dims <- all_dimensions[all_dimensions %in% names(metadata)]
      
      if (length(available_dims) > 0) {
        # For search phase, just use the first dimension for simplicity
        search_formula_str <- paste("~", available_dims[1])
        prevalence_formula <- stats::as.formula(search_formula_str)
        log_message(paste("Using simplified formula for searchK:", 
                          search_formula_str), "search_k")
      } else {
        log_message("No usable dimensions for prevalence formula, using intercept-only", 
                    "search_k")
      }
    }
  }
  
  ## --- Prepare k values -------------------------------------------------------
  log_message(paste("Determining optimal number of topics (min =", min_k, 
                    ", max =", max_k, 
                    ", step =", step_k, ")"), "search_k")
  
  # Generate sequence of k values to test
  k_values <- seq(min_k, max_k, by = step_k)
  log_message(paste("Testing K values:", paste(k_values, collapse = ", ")), "search_k")
  
  # Store k values in diagnostics
  diagnostics$k_values <- k_values
  
  ## --- Run searchK ------------------------------------------------------------
  log_message("Running searchK to evaluate topic models", "search_k")
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Run searchK
  k_search <- tryCatch({
    stm::searchK(
      documents = stm_input$documents,
      vocab = stm_input$vocab,
      K = k_values,
      prevalence = prevalence_formula,  # Use simplified formula
      data = metadata,
      max.em.its = iterations,
      init.type = "Spectral",
      verbose = FALSE,
      seed = seed
    )
  }, error = function(e) {
    error_msg <- paste("Error in searchK:", e$message)
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "search_k", "ERROR")
    stop(error_msg)
  })
  
  # Extract results
  k_results <- k_search$results
  
  ## --- Determine best K -------------------------------------------------------
  log_message("Analyzing results to find optimal K", "search_k")
  
  # Calculate combined score with balanced weights and complexity penalty
  # Normalize metrics to 0-1 scale
  normalize <- function(x) {
    if (max(x) == min(x)) return(rep(0.5, length(x)))
    (x - min(x)) / (max(x) - min(x))
  }
  
  # For each metric, higher is better except residual (lower is better)
  semcoh_norm <- normalize(k_results$semcoh)
  exclus_norm <- normalize(k_results$exclus)
  residual_norm <- 1 - normalize(k_results$residual)  # Invert so higher is better
  
  # Calculate combined score
  combined_score <- coherence_weight * semcoh_norm + 
    exclusivity_weight * exclus_norm + 
    residual_weight * residual_norm - 
    complexity_penalty * normalize(k_results$K)  # Complexity penalty
  
  # Find best K
  best_idx <- which.max(combined_score)
  best_k <- k_values[best_idx]
  
  log_message(paste("Selected optimal K =", best_k), "search_k")
  
  # Store metrics for best k in diagnostics
  diagnostics$best_k_metrics <- list(
    k = best_k,
    coherence = k_results$semcoh[best_idx],
    exclusivity = k_results$exclus[best_idx],
    residual = k_results$residual[best_idx],
    held_out = k_results$heldout[best_idx],
    semantic = k_results$semantic[best_idx],
    combined_score = combined_score[best_idx]
  )
  
  ## --- Create metrics dataframe -----------------------------------------------
  log_message("Creating metrics dataframe for visualization", "search_k")
  
  # Convert to dataframe for easier plotting and analysis
  metrics_df <- data.frame(
    k = k_results$K,
    coherence = k_results$semcoh,
    exclusivity = k_results$exclus, 
    residual = k_results$residual,
    held_out = k_results$heldout,
    semantic = k_results$semantic,
    combined_score = combined_score,
    is_best = k_results$K == best_k
  )
  
  ## --- Create plots -----------------------------------------------------------
  log_message("Creating visualization of metrics", "search_k")
  
  # Create visualization plots using base R for simplicity
  # (This creates an R object with the plotting function, not an actual image)
  
  plot_metrics <- function() {
    par(mfrow = c(2, 2))
    
    # Plot 1: Coherence
    plot(k_results$K, k_results$semcoh, type = "b", 
         main = "Semantic Coherence", 
         xlab = "Number of Topics (K)", ylab = "Coherence",
         col = "blue", pch = 16)
    points(best_k, k_results$semcoh[best_idx], col = "red", pch = 16, cex = 1.5)
    
    # Plot 2: Exclusivity
    plot(k_results$K, k_results$exclus, type = "b", 
         main = "Exclusivity", 
         xlab = "Number of Topics (K)", ylab = "Exclusivity",
         col = "blue", pch = 16)
    points(best_k, k_results$exclus[best_idx], col = "red", pch = 16, cex = 1.5)
    
    # Plot 3: Residuals
    plot(k_results$K, k_results$residual, type = "b", 
         main = "Residuals", 
         xlab = "Number of Topics (K)", ylab = "Residual",
         col = "blue", pch = 16)
    points(best_k, k_results$residual[best_idx], col = "red", pch = 16, cex = 1.5)
    
    # Plot 4: Combined Score
    plot(k_results$K, combined_score, type = "b", 
         main = "Combined Score", 
         xlab = "Number of Topics (K)", ylab = "Score",
         col = "blue", pch = 16)
    points(best_k, combined_score[best_idx], col = "red", pch = 16, cex = 1.5)
    
    # Reset par
    par(mfrow = c(1, 1))
  }
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    best_k = best_k,
    metrics = metrics_df,
    plots = plot_metrics  # Function to generate plots
  )
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k_values = k_values,
    weights = list(
      coherence = coherence_weight,
      exclusivity = exclusivity_weight,
      residual = residual_weight,
      complexity_penalty = complexity_penalty
    ),
    iterations = iterations,
    seed = seed,
    success = TRUE
  )
  
  log_message(paste("K selection complete, optimal K =", best_k), "search_k")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}