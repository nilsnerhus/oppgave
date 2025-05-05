#' @title Find optimal number of topics for topic modeling with enhanced metrics
#' @description Identifies the ideal number of topics (k value) for structural
#'   topic modeling by evaluating models on coherence, exclusivity, and perplexity.
#'
#' @param input Pre-processed corpus from prepare_corpus() function
#' @param k_min Minimum number of topics to try (default: 20)
#' @param k_max Maximum number of topics to try (default: 120)
#' @param k_step Increment between k values (default: 5)
#' @param complexity Coefficient for penalizing higher k values (default: 0.02)
#' @param max_iterations Maximum number of EM algorithm iterations (default: 75)
#' @param coherence Weight for coherence in scoring (default: 1.0, 0 to disable)
#' @param exclusivity Weight for exclusivity in scoring (default: 1.0, 0 to disable)
#' @param perplexity Weight for perplexity in scoring (default: 0.5, 0 to disable)
#' @param seed Random seed for reproducibility (default: 1234)
#' @param output_path Path to save results (default: "data/best_k.rds")
#'
#' @return A list containing:
#'   \item{data}{Best model, best k value, diagnostics, plots, and metric contributions}
#'   \item{metadata}{Processing information and parameters}
#'   \item{diagnostics}{Information about model fitting issues and comparisons}
#'
#' @examples
#' \dontrun{
#' # Basic usage with default metrics
#' best_k_result <- find_best_k(corpus_data)
#' 
#' # Emphasize perplexity more in model selection
#' best_k_result <- find_best_k(corpus_data, perplexity = 1.0, coherence = 0.5)
#' }

find_best_k <- function(
    input,
    k_min = 20, 
    k_max = 60, 
    k_step = 5,
    complexity = 0.02,
    max_iterations = 75,
    coherence = 1.0,
    exclusivity = 1.0,
    perplexity = 0.5,
    seed = 1234,
    output_path = "data/best_k.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Set constants
  held_out_proportion <- 0.1  # Fixed proportion for perplexity testing
  verbose <- TRUE  # Always provide detailed logging
  normalize <- TRUE  # Always normalize metrics
  
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
    if (!is.numeric(complexity) || complexity < 0)
      stop("complexity must be a non-negative number")
    if (!is.numeric(max_iterations) || max_iterations <= 0)
      stop("max_iterations must be a positive number")
    
    # Check input structure
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
  } else {
    # Legacy format
    stm_data <- input$stm_data
  }
  
  # Extract document_metadata, handling both formats
  if ("data" %in% names(input) && "metadata" %in% names(input$data)) {
    # New nested format
    document_metadata <- input$data$metadata
  } else {
    # Legacy format
    document_metadata <- input$metadata
  }
  
  # Final validation that we have the necessary components
  if (!all(c("documents", "vocab") %in% names(stm_data))) {
    error_msg <- "Missing required stm_data components: documents or vocab"
    log_message(error_msg, "find_best_k", "ERROR")
    stop(error_msg)
  }
  
  ## --- Prepare held-out data if using perplexity -----------------------------
  heldout_data <- NULL
  use_perplexity <- perplexity > 0
  
  if (use_perplexity) {
    log_message("Creating held-out data structure for perplexity evaluation", "find_best_k")
    
    # Prepare held-out data using STM's make.heldout function
    tryCatch({
      # Set seed for reproducibility
      if (!is.null(seed)) set.seed(seed)
      
      # Create held-out data
      heldout_data <- stm::make.heldout(
        documents = stm_data$documents,
        vocab = stm_data$vocab,
        proportion = held_out_proportion,
        seed = seed
      )
      
      # Update documents to use the held-out version
      stm_data$documents <- heldout_data$documents
      
      log_message(paste("Successfully created held-out data with", 
                        held_out_proportion * 100, "% of words held out"), 
                  "find_best_k")
    }, error = function(e) {
      log_message(paste("Failed to create held-out data:", e$message), 
                  "find_best_k", "WARNING")
      diagnostics$processing_issues <- c(
        diagnostics$processing_issues,
        paste("Held-out data creation failed:", e$message)
      )
      
      # Disable perplexity if we can't create held-out data
      use_perplexity <- FALSE
    })
  }
  
  ## --- Prepare for model testing ---------------------------------------------
  log_message("Preparing to test models", "find_best_k")
  
  # Define range of k values to test
  k_values <- seq(k_min, k_max, by = k_step)
  
  # Prepare results data frame
  model_results <- tibble::tibble(
    k = integer(),
    coherence = numeric(),
    exclusivity = numeric(),
    perplexity = numeric(),
    penalty = numeric()
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
    
    # Fit the model
    model_k <- tryCatch({
      stm::stm(
        documents = stm_data$documents,
        vocab = stm_data$vocab,
        K = k,
        max.em.its = max_iterations,
        init.type = "Spectral",
        seed = seed
      )
    }, error = function(e) {
      log_message(paste("Model failed for k =", k, ":", e$message), 
                  "find_best_k", "WARNING")
      diagnostics$failed_models[[paste0("k_", k)]] <- e$message
      return(NULL)
    })
    
    if (!is.null(model_k)) {
      log_message("Evaluating model quality", "find_best_k")
      
      # Calculate coherence if enabled
      coherence_val <- if (coherence > 0) {
        tryCatch({
          mean(stm::semanticCoherence(model_k, stm_data$documents))
        }, error = function(e) {
          log_message(paste("Coherence calculation failed:", e$message), 
                      "find_best_k", "WARNING")
          diagnostics$processing_issues <- c(
            diagnostics$processing_issues,
            paste("Coherence failed for k =", k, ":", e$message)
          )
          NA_real_
        })
      } else {
        0
      }
      
      # Calculate exclusivity if enabled
      exclusivity_val <- if (exclusivity > 0) {
        tryCatch({
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
      } else {
        0
      }
      
      # Calculate perplexity if enabled and we have held-out data
      perplexity_val <- if (use_perplexity && !is.null(heldout_data)) {
        tryCatch({
          # Evaluate held-out likelihood using STM's eval.heldout function
          heldout_eval <- stm::eval.heldout(model_k, heldout_data$missing)
          
          # Convert to perplexity (lower is better)
          exp(-1 * heldout_eval$expected.heldout)
        }, error = function(e) {
          log_message(paste("Perplexity calculation failed:", e$message), 
                      "find_best_k", "WARNING")
          diagnostics$processing_issues <- c(
            diagnostics$processing_issues,
            paste("Perplexity failed for k =", k, ":", e$message)
          )
          NA_real_
        })
      } else {
        0
      }
      
      # Calculate complexity penalty
      penalty <- complexity * k
      
      # Store model
      all_models[[paste0("k_", k)]] <- model_k
      
      # Add to results
      model_results <- dplyr::bind_rows(
        model_results, 
        tibble::tibble(
          k = k,
          coherence = coherence_val,
          exclusivity = exclusivity_val,
          perplexity = perplexity_val,
          penalty = penalty
        )
      )
    }
  }
  
  ## --- Calculate final scores with normalized metrics -----------------------
  if (nrow(model_results) == 0) {
    log_message("All models failed. Cannot determine best k.", "find_best_k", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = start_time,
        k_min = k_min,
        k_max = k_max,
        k_step = k_step,
        complexity = complexity,
        max_iterations = max_iterations,
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Normalize metrics
  log_message("Normalizing metrics for comparison", "find_best_k")
  
  # Helper function to normalize a column
  normalize_col <- function(x) {
    if (length(x) <= 1 || all(is.na(x)) || (max(x, na.rm = TRUE) == min(x, na.rm = TRUE))) {
      return(rep(0, length(x)))
    }
    result <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    result[is.na(result)] <- 0
    return(result)
  }
  
  # Add normalized columns
  if (coherence > 0) {
    model_results$coherence_norm <- normalize_col(model_results$coherence)
  } else {
    model_results$coherence_norm <- 0
  }
  
  if (exclusivity > 0) {
    model_results$exclusivity_norm <- normalize_col(model_results$exclusivity)
  } else {
    model_results$exclusivity_norm <- 0
  }
  
  if (use_perplexity) {
    # For perplexity, lower is better, so invert
    model_results$perplexity_norm <- 1 - normalize_col(model_results$perplexity)
  } else {
    model_results$perplexity_norm <- 0
  }
  
  # Calculate combined score
  model_results$score <- (coherence * model_results$coherence_norm) +
    (exclusivity * model_results$exclusivity_norm) +
    (perplexity * model_results$perplexity_norm) -
    model_results$penalty
  
  # Find best k
  best_model_info <- model_results %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice(1)
  
  best_k <- best_model_info$k
  
  log_message(paste("Best k value identified:", best_k), "find_best_k")
  
  # Get the best model
  best_model_key <- paste0("k_", best_k)
  best_model <- all_models[[best_model_key]]
  
  ## --- Create diagnostic plots -----------------------------------------------
  log_message("Creating diagnostic plots", "find_best_k")
  
  # Create metrics plot
  metrics_plot <- model_results %>%
    tidyr::pivot_longer(
      cols = c(coherence, exclusivity, perplexity), 
      names_to = "metric", 
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%  # Remove any NA values
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
  
  # Create score plot
  score_plot <- model_results %>%
    ggplot2::ggplot(ggplot2::aes(x = k, y = score)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = best_k, linetype = "dashed") +
    ggplot2::labs(
      title = "Topic Model Combined Score",
      subtitle = paste("Complexity penalty coefficient:", complexity),
      x = "Number of Topics (k)",
      y = "Score"
    ) +
    ggplot2::theme_minimal()
  
  # Combine plots into a list
  plots <- list(
    metrics = metrics_plot,
    score = score_plot
  )
  
  ## --- Create metrics contribution analysis ---------------------------------
  log_message("Analyzing metric contributions", "find_best_k")
  
  best_idx <- which(model_results$k == best_k)
  
  metric_contributions <- tibble::tibble(
    metric = c("Coherence", "Exclusivity", "Perplexity", "Complexity Penalty"),
    raw_value = c(
      model_results$coherence[best_idx],
      model_results$exclusivity[best_idx],
      model_results$perplexity[best_idx],
      model_results$penalty[best_idx]
    ),
    normalized_value = c(
      model_results$coherence_norm[best_idx],
      model_results$exclusivity_norm[best_idx],
      model_results$perplexity_norm[best_idx],
      NA
    ),
    weight = c(
      coherence,
      exclusivity,
      perplexity,
      1.0
    ),
    contribution = c(
      coherence * model_results$coherence_norm[best_idx],
      exclusivity * model_results$exclusivity_norm[best_idx],
      perplexity * model_results$perplexity_norm[best_idx],
      -model_results$penalty[best_idx]
    )
  )
  
  ## --- Prepare result --------------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Prepare result data
  result_data <- list(
    best_k = best_k,
    best_model = best_model,
    diagnostics = model_results,
    plots = plots,
    metric_contributions = metric_contributions,
    document_metadata = document_metadata,
    stm_data = list(
      vocab = stm_data$vocab, 
      documents = stm_data$documents
    )
  )
  
  # Prepare result metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k_min = k_min,
    k_max = k_max,
    k_step = k_step,
    complexity = complexity,
    max_iterations = max_iterations,
    coherence = coherence,
    exclusivity = exclusivity,
    perplexity = perplexity,
    held_out_proportion = held_out_proportion,
    normalize = normalize,
    models_tested = nrow(model_results),
    models_failed = length(diagnostics$failed_models),
    seed = seed,
    success = TRUE
  )
  
  # Add model comparison details to diagnostics
  diagnostics$model_comparisons <- list(
    best_k = best_k,
    best_coherence = best_model_info$coherence,
    best_exclusivity = best_model_info$exclusivity,
    best_perplexity = best_model_info$perplexity,
    all_metrics = model_results
  )
  
  log_message(paste("Optimal topic selection complete! Best k =", best_k, 
                    "(with complexity =", complexity, ")"), "find_best_k")
  
  # Split the model from the results to avoid large files in Git
  model_path <- gsub("\\.rds$", ".model.rds", output_path)
  saveRDS(best_model, model_path)  # Save model separately
  result_data$best_model <- NULL  # Remove from main result
  result_data$best_model_path <- model_path  # Store path instead
  
  # Clean up memory
  rm(all_models)
  gc()
  
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}