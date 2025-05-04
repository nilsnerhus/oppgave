#' @title Find optimal number of topics for topic modeling
#' @description Identifies the ideal number of topics (k value) for structural
#'   topic modeling by fitting multiple models with different k values and
#'   evaluating quality metrics like semantic coherence and exclusivity.
#'
#' @param input Pre-processed corpus from prepare_corpus() function
#' @param k_min Minimum number of topics to try (default: 20)
#' @param k_max Maximum number of topics to try (default: 120)
#' @param k_step Increment between k values (default: 5)
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
#' # Try a smaller range with finer increments
#' best_k_result <- find_best_k(corpus_data, k_min = 10, k_max = 50, k_step = 2)
#' }

find_best_k <- function(
    input,
    k_min = 20, 
    k_max = 200, 
    k_step = 5,
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
    
    # Check corpus format
    validate_input(input, c("dfm", "metadata", "stm_data"), "find_best_k")
    
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "find_best_k", "ERROR")
    stop(e$message)
  })
  
  ## --- Extract components ----------------------------------------------------
  log_message("Extracting corpus components", "find_best_k")
  
  stm_data <- input$stm_data
  meta <- input$metadata
  
  ## --- Prepare for model testing ---------------------------------------------
  log_message("Preparing to test models", "find_best_k")
  
  # Define range of k values to test
  k_values <- seq(k_min, k_max, by = k_step)
  
  # Prepare results data frame
  model_results <- tibble::tibble(
    k = integer(),
    semantic_coherence = numeric(),
    exclusivity = numeric(),
    complexity_penalty = numeric(),
    score = numeric()
  )
  
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
        stm::stm(
          documents = stm_data$documents,
          vocab = stm_data$vocab,
          K = k,
          max.em.its = 75,  # Moderate iterations for testing
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
      # Check for convergence issues
      bound_history <- model_k$convergence$bound
      n_iter <- length(bound_history)
      
      # Check if maximum iterations were reached
      reached_max_iter <- n_iter >= 75  # Using the same value as max.em.its above
      
      # Calculate rate of change in last few iterations
      if (n_iter > 5) {
        final_changes <- diff(tail(bound_history, 6))
        avg_change <- mean(abs(final_changes))
        
        # If still changing significantly at end, probably hasn't converged
        if (avg_change > 1e-3 || reached_max_iter) {
          # Add to diagnostics
          convergence_warning <- sprintf(
            "Model k=%d may not have fully converged (iterations: %d, change rate: %.6f)",
            k, n_iter, avg_change
          )
          log_message(convergence_warning, "find_best_k", "WARNING")
          
          diagnostics$model_comparisons[[paste0("k_", k, "_convergence")]] <- list(
            iterations_used = n_iter,
            reached_max = reached_max_iter,
            final_change_rate = avg_change,
            recommended_iter = 150
          )
        }
      }
      
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
      
      # Combined score (higher is better)
      combined_score <- semantic_coherence + exclusivity_score
      
      # Add to results
      model_results <- dplyr::bind_rows(
        model_results, 
        tibble::tibble(
          k = k,
          semantic_coherence = semantic_coherence,
          exclusivity = exclusivity_score,
          score = combined_score
        )
      )
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
  
  log_message(paste("Best k value identified:", best_k), "find_best_k")
  
  ## --- Create diagnostic plot ------------------------------------------------
  log_message("Creating diagnostic plot", "find_best_k")
  
  # Create data for metrics side-by-side plot
  metrics_df <- tibble::tibble(
    k = rep(model_results$k, 3),
    metric = c(
      rep("Semantic Coherence", nrow(model_results)),
      rep("Exclusivity", nrow(model_results)),
      rep("Complexity Penalty", nrow(model_results))
    ),
    value = c(
      model_results$semantic_coherence,
      model_results$exclusivity,
      model_results$complexity_penalty * (-1)  # Invert for visual clarity
    )
  )
    
  # Create metrics comparison plot
  p1 <- ggplot2::ggplot(metrics_df, ggplot2::aes(x = k, y = value, color = metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = best_k, linetype = "longdash", color = "darkgrey") +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggplot2::labs(
      title = "Topic Model Quality Metrics",
      subtitle = paste("Comparing metrics across different topic counts"),
      x = "Number of Topics (k)",
      y = "Value"
    ) +
    ggplot2::scale_color_manual(values = c(
      "Semantic Coherence" = "blue", 
      "Exclusivity" = "darkgreen",
      "Complexity Penalty" = "red"
    )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  
  # Create combined score plot
  p2 <- ggplot2::ggplot(model_results, ggplot2::aes(x = k, y = score)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = best_k, linetype = "longdash", color = "darkgrey") +
    ggplot2::annotate("text", x = best_k + 2, y = min(model_results$score), 
                      label = paste("Best k =", best_k), hjust = 0) +
    ggplot2::labs(
      title = "Combined Topic Model Score",
      subtitle = "Coherence + 1.2*Exclusivity - Complexity Penalty",
      x = "Number of Topics (k)",
      y = "Combined Score"
    ) +
    ggplot2::theme_minimal()
  
  plots <- list(metrics = p1, combined_score = p2)
  
  ## --- Fit final model -------------------------------------------------------
  log_message(paste("Fitting final model with k =", best_k), "find_best_k")
  
  best_model <- time_operation({
    stm::stm(
      documents = stm_data$documents,
      vocab = stm_data$vocab,
      K = best_k,
      max.em.its = 150,
      init.type = "Spectral",
      seed = seed
    )
  }, "find_best_k")
  
  # Final convergence check for the best model
  if (!is.null(best_model)) {
    bound_history <- best_model$convergence$bound
    n_iter <- length(bound_history)
    reached_max_iter <- n_iter >= 100 # or whatever max.em.its is for final model
    
    if (n_iter > 5) {
      final_changes <- diff(tail(bound_history, 6))
      avg_change <- mean(abs(final_changes))
      
      if (avg_change > 1e-4 || reached_max_iter) {
        recommended_iter <- max(150, round(max.em.its * 1.5))
        log_message(sprintf(
          "WARNING: Best model (k=%d) may need more iterations for full convergence.\n  Current: %d iterations, change rate: %.6f\n  Recommended: %d iterations",
          best_k, n_iter, avg_change, recommended_iter
        ), "find_best_k", "WARNING")
        
        # Add to result metadata
        result_metadata$convergence_issues <- TRUE
        result_metadata$recommended_iterations <- recommended_iter
      }
    }
  }
  
  ## --- Prepare result --------------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Prepare result data
  result_data <- list(
    best_k = best_k,
    best_model = best_model,
    diagnostics = model_results,
    metrics_plot = plots$metrics,         # Individual metrics plot
    combined_score_plot = plots$combined_score,  # Combined score plot
    metadata = meta,
    stm_data = list(
      vocab = stm_data$vocab, 
      documents = stm_data$documents
    )
  )
  
  # Prepare metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k_min = k_min,
    k_max = k_max,
    k_step = k_step,
    models_tested = nrow(model_results),
    models_failed = length(diagnostics$failed_models),
    seed = seed,
    complexity_penalty_factor = 0.1,  # Document factor used in penalty
    exclusivity_weight = 1.2,         # Weight given to exclusivity
    success = TRUE
  )
  
  # Add model comparison details to diagnostics
  diagnostics$model_comparisons <- list(
    best_k = best_k,
    best_coherence = best_model_info$semantic_coherence,
    best_exclusivity = best_model_info$exclusivity,
    best_complexity_penalty = best_model_info$complexity_penalty,
    metrics_by_k = model_results
  )
  
  log_message(paste("Optimal topic selection complete! Best k =", best_k), "find_best_k")
  
  # Split the model from the results to avoid large files in Git
  model_path <- gsub("\\.rds$", ".model.rds", output_path)
  saveRDS(best_model, model_path)  # Save model separately
  result_data$best_model <- NULL   # Remove from main result
  result_data$best_model_path <- model_path  # Store path instead
  
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}