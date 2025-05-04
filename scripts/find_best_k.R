#' @title Find optimal number of topics for topic modeling
#' @description Identifies the ideal number of topics (k value) for structural
#'   topic modeling by fitting multiple models with different k values and
#'   evaluating quality metrics like semantic coherence and exclusivity.
#'
#' @param input Pre-processed corpus from prepare_corpus() function
#' @param k_min Minimum number of topics to try (default: 20)
#' @param k_max Maximum number of topics to try (default: 120)
#' @param k_step Increment between k values (default: 5)
#' @param complexity_penalty Coefficient for penalizing higher k values (default: 0.02)
#' @param max_iterations Maximum number of EM algorithm iterations (default: 75)
#' @param seed Random seed for reproducibility (default: 1234)
#' @param output_path Path to save results (default: "data/best_k.rds")
#'
#' @return A list containing:
#'   \item{data}{Best model, best k value, diagnostics, plot, metadata, and stm data}
#'   \item{metadata}{Processing information including timing and model parameters}
#'   \item{diagnostics}{Information about model fitting issues and comparisons}
#'
#' @examples
#' \dontrun{
#' # Find optimal k with default parameters
#' best_k_result <- find_best_k(corpus_data)
#' 
#' # Try a smaller range with finer increments and more iterations
#' best_k_result <- find_best_k(corpus_data, 
#'                              k_min = 10, 
#'                              k_max = 50, 
#'                              k_step = 2, 
#'                              max_iterations = 100)
#' }

find_best_k <- function(
    input,
    k_min = 20, 
    k_max = 200, 
    k_step = 5,
    complexity_penalty = 0.02,
    max_iterations = 75,
    seed = 1234,
    output_path = "data/best_k.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    failed_models = list(),
    processing_issues = character(),
    model_comparisons = list()
  )
  
  ## --- Input validation ------------------------------------------------------
  log_message("Validating input parameters", "find_best_k")
  
  tryCatch({
    # Validate k parameters
    if (!is.numeric(k_min) || k_min <= 0) 
      stop("k_min must be a positive number")
    if (!is.numeric(k_max) || k_max <= k_min) 
      stop("k_max must be greater than k_min")
    if (!is.numeric(k_step) || k_step <= 0) 
      stop("k_step must be a positive number")
    if (!is.numeric(complexity_penalty) || complexity_penalty < 0)
      stop("complexity_penalty must be a non-negative number")
    if (!is.numeric(max_iterations) || max_iterations <= 0)
      stop("max_iterations must be a positive number")
    
    # Check input structure - first try standard format
    if ("data" %in% names(input) && is.list(input$data) && 
        all(c("dfm", "metadata", "stm_data") %in% names(input$data))) {
      # New standardized structure
      log_message("Found corpus in standard nested format", "find_best_k")
    } else if (all(c("dfm", "metadata", "stm_data") %in% names(input))) {
      # Legacy format
      log_message("Found corpus in legacy format", "find_best_k")
    } else {
      stop("Input missing required components: dfm, metadata, or stm_data")
    }
    
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "find_best_k", "ERROR")
    stop(e$message)
  })
  
  ## --- Extract components ----------------------------------------------------
  log_message("Extracting corpus components", "find_best_k")
  
  # Extract stm_data, handling both nested and legacy formats
  if ("data" %in% names(input) && "stm_data" %in% names(input$data)) {
    # New nested format
    stm_data <- input$data$stm_data
    log_message("Extracted stm_data from nested input", "find_best_k")
  } else {
    # Legacy format
    stm_data <- input$stm_data
    log_message("Extracted stm_data from direct input", "find_best_k")
  }
  
  # Extract document_metadata, handling both formats
  if ("data" %in% names(input) && "metadata" %in% names(input$data)) {
    # New nested format
    document_metadata <- input$data$metadata
    log_message("Extracted document_metadata from nested input", "find_best_k")
  } else {
    # Legacy format
    document_metadata <- input$metadata
    log_message("Extracted document_metadata from direct input", "find_best_k")
  }
  
  # Final validation that we have the necessary components
  if (!all(c("documents", "vocab") %in% names(stm_data))) {
    error_msg <- "Missing required stm_data components: documents or vocab"
    log_message(error_msg, "find_best_k", "ERROR")
    stop(error_msg)
  }
  
  ## --- Prepare for model testing ---------------------------------------------
  log_message("Preparing to test models", "find_best_k")
  
  # Define range of k values to test
  k_values <- seq(k_min, k_max, by = k_step)
  
  # Prepare results data frame
  model_results <- tibble::tibble(
    k = integer(),
    semantic_coherence = numeric(),
    exclusivity = numeric(),
    penalty = numeric(),
    score = numeric()
  )
  
  # Storage for models
  all_models <- list()
  
  # Set seed for reproducibility
  set.seed(seed)
  
  ## --- Test models with different k values -----------------------------------
  log_message(paste("Testing", length(k_values), "models with k from", k_min, "to", k_max),
              "find_best_k")
  
  for (k in k_values) {
    log_message(paste("Fitting model with k =", k), "find_best_k")
    
    # Use time_operation for expensive model fitting
    model_k <- time_operation({
      tryCatch({
        # Ensure parameters are passed correctly as named arguments
        stm::stm(
          documents = stm_data$documents,
          vocab = stm_data$vocab,
          K = k,
          max.em.its = max_iterations,  # Using the new parameter name
          init.type = "Spectral",
          seed = seed
        )
      }, error = function(e) {
        log_message(paste("Model failed for k =", k, ":", e$message), 
                    "find_best_k", "WARNING")
        diagnostics$failed_models[[paste0("k_", k)]] <- e$message
        return(NULL)
      })
    }, "find_best_k")
    
    if (!is.null(model_k)) {
      log_message("Evaluating model quality", "find_best_k")
      
      # Calculate semantic coherence
      semantic_coherence <- tryCatch({
        mean(stm::semanticCoherence(model_k, stm_data$documents))
      }, error = function(e) {
        log_message(paste("Semantic coherence calculation failed:", e$message), 
                    "find_best_k", "WARNING")
        diagnostics$processing_issues <- c(
          diagnostics$processing_issues,
          paste("Semantic coherence failed for k =", k, ":", e$message)
        )
        NA_real_
      })
      
      # Calculate exclusivity
      exclusivity_score <- tryCatch({
        mean(stm::exclusivity(model_k))
      }, error = function(e) {
        log_message(paste("Exclusivity calculation failed:", e$message), 
                    "find_best_k", "WARNING")
        diagnostics$processing_issues <- c(
          diagnostics$processing_issues,
          paste("Exclusivity failed for k =", k, ":", e$message)
        )
        NA_real_
      })
      
      # Calculate complexity penalty
      penalty <- complexity_penalty * k
      
      # Combined score (higher is better) with complexity penalty
      combined_score <- semantic_coherence + exclusivity_score - penalty
      
      # Add to results
      model_results <- dplyr::bind_rows(
        model_results, 
        tibble::tibble(
          k = k,
          semantic_coherence = semantic_coherence,
          exclusivity = exclusivity_score,
          penalty = penalty,
          score = combined_score
        )
      )
      
      # Store model in list
      model_key <- paste0("k_", k)
      all_models[[model_key]] <- model_k
    }
  }
  
  ## --- Find best model -------------------------------------------------------
  if (nrow(model_results) == 0) {
    log_message("All models failed. Cannot determine best k.", "find_best_k", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = start_time,
        k_min = k_min,
        k_max = k_max,
        k_step = k_step,
        complexity_penalty = complexity_penalty,
        max_iterations = max_iterations,
        seed = seed,
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Find best k based on combined score
  best_model_info <- model_results %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice(1)
  
  best_k <- best_model_info$k
  best_model_key <- paste0("k_", best_k)
  
  log_message(paste("Best k value identified:", best_k), "find_best_k")
  
  # Get the best model from our stored models
  best_model <- all_models[[best_model_key]]
  
  ## --- Create diagnostic plot ------------------------------------------------
  log_message("Creating diagnostic plot", "find_best_k")
  
  # Create basic metrics plot
  metrics_plot <- model_results %>%
    tidyr::pivot_longer(
      cols = c(semantic_coherence, exclusivity), 
      names_to = "metric", 
      values_to = "value"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = k, y = value, color = metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = best_k, linetype = "dashed") +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggplot2::labs(
      title = "Topic Model Diagnostic Metrics",
      subtitle = paste("Optimal number of topics:", best_k),
      x = "Number of Topics (k)",
      y = "Value"
    ) +
    ggplot2::theme_minimal()
  
  # Create combined score plot showing penalty effect
  score_plot <- model_results %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = k, y = semantic_coherence + exclusivity_score, 
                                    color = "Combined (no penalty)")) +
    ggplot2::geom_line(ggplot2::aes(x = k, y = score, color = "Final Score (with penalty)")) +
    ggplot2::geom_vline(xintercept = best_k, linetype = "dashed") +
    ggplot2::labs(
      title = "Topic Model Scores with Complexity Penalty",
      subtitle = paste("Complexity penalty coefficient:", complexity_penalty),
      x = "Number of Topics (k)",
      y = "Score",
      color = "Score Type"
    ) +
    ggplot2::theme_minimal()
  
  # Combine plots into a list
  plots <- list(
    metrics = metrics_plot,
    scores = score_plot
  )
  
  ## --- Prepare result --------------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Prepare result data - explicitly named components for clarity
  result_data <- list(
    best_k = best_k,
    best_model = best_model,
    diagnostics = model_results,
    plots = plots,  # Include both plots
    document_metadata = document_metadata,  # Renamed to be explicit and consistent
    stm_data = list(
      vocab = stm_data$vocab, 
      documents = stm_data$documents
    )
  )
  
  # Prepare result metadata (about the processing itself)
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k_min = k_min,
    k_max = k_max,
    k_step = k_step,
    complexity_penalty = complexity_penalty,
    max_iterations = max_iterations,
    models_tested = nrow(model_results),
    models_failed = length(diagnostics$failed_models),
    seed = seed,
    success = TRUE
  )
  
  # Add model comparison details to diagnostics
  diagnostics$model_comparisons <- list(
    best_k = best_k,
    best_coherence = best_model_info$semantic_coherence,
    best_exclusivity = best_model_info$exclusivity,
    best_penalty = best_model_info$penalty,
    all_metrics = model_results
  )
  
  log_message(paste("Optimal topic selection complete! Best k =", best_k, 
                    "(with complexity penalty =", complexity_penalty, ")"), 
              "find_best_k")
  
  # Split the model from the results to avoid large files in Git
  model_path <- gsub("\\.rds$", ".model.rds", output_path)
  saveRDS(best_model, model_path)  # Save model separately
  result_data$best_model <- NULL  # Remove from main result
  result_data$best_model_path <- model_path  # Store path instead
  
  # Clean up memory by removing stored models
  rm(all_models)
  gc()
  
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}