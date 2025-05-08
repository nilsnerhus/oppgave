#' @title Fit optimal structural topic model
#' @description Identifies the ideal number of topics (if not specified),
#'   fits a structural topic model, and extracts document-topic proportions
#'   with aggregation from segments to documents if segmentation was used.
#'
#' @param corpus Output from prepare_corpus() function
#' @param k Number of topics to use (default: NULL, auto-determines optimal k)
#' @param k_min Minimum number of topics to try if auto-determining (default: 20)
#' @param k_max Maximum number of topics to try if auto-determining (default: 80)
#' @param k_step Step size for k search (default: 10)
#' @param prevalence.formula Formula for metadata covariates (default: NULL)
#' @param seed Random seed for reproducibility (default: 1234)
#' @param iterations Maximum EM algorithm iterations (default: 150)
#' @param output_path Path to save results (default: "data/topic_model.rds")
#'
#' @return A list containing:
#'   \item{data}{Document-topic proportions, topic labels, and metadata}
#'   \item{metadata}{Processing information and model parameters}
#'   \item{diagnostics}{Model quality metrics and processing information}
#'
#' @examples
#' \dontrun{
#' # Fit model with automatic k determination
#' model <- fit_model(corpus)
#' 
#' # Fit model with specified k and metadata
#' model <- fit_model(corpus, k = 50, 
#'                   prevalence.formula = "~ region + wb_income_level")
#' }

fit_model <- function(
    corpus,
    k = NULL,
    k_min = 20,
    k_max = 80, 
    k_step = 10,
    prevalence.formula = NULL,
    seed = 1234,
    iterations = 150,
    output_path = "data/topic_model.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    model_quality = list(),
    k_selection = list(),
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input corpus", "fit_model")
  
  tryCatch({
    # Check that corpus has the expected structure
    if (!is.list(corpus) || !"data" %in% names(corpus)) {
      stop("Input corpus must be the output from prepare_corpus()")
    }
    
    # Validate corpus data components
    required_components <- c("stm_data")
    missing_components <- setdiff(required_components, names(corpus$data))
    if (length(missing_components) > 0) {
      stop("Corpus missing required components: ", paste(missing_components, collapse = ", "))
    }
    
    # Validate stm_data structure
    stm_required <- c("documents", "vocab")
    stm_missing <- setdiff(stm_required, names(corpus$data$stm_data))
    if (length(stm_missing) > 0) {
      stop("STM data missing required components: ", paste(stm_missing, collapse = ", "))
    }
    
    # Validate k parameters
    if (!is.null(k)) {
      if (!is.numeric(k) || k < 2) {
        stop("k must be a numeric value greater than 1")
      }
    } else {
      if (!is.numeric(k_min) || !is.numeric(k_max) || !is.numeric(k_step)) {
        stop("k_min, k_max, and k_step must be numeric values")
      }
      if (k_min < 2) {
        stop("k_min must be at least 2")
      }
      if (k_max <= k_min) {
        stop("k_max must be greater than k_min")
      }
      if (k_step <= 0) {
        stop("k_step must be positive")
      }
    }
    
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "fit_model", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, e$message)
    stop(e$message)
  })
  
  ## --- Extract corpus components ---------------------------------------------
  log_message("Extracting corpus components", "fit_model")
  
  # Extract STM data
  stm_data <- corpus$data$stm_data
  documents <- stm_data$documents
  vocab <- stm_data$vocab
  
  # Extract metadata if available
  if ("meta" %in% names(stm_data)) {
    metadata <- stm_data$meta
  } else if ("metadata" %in% names(corpus$data)) {
    metadata <- corpus$data$metadata
  } else {
    metadata <- NULL
    log_message("No metadata found in corpus", "fit_model", "WARNING")
  }
  
  # Check for segmentation
  use_segmentation <- FALSE
  segment_mapping <- NULL
  
  if ("segments" %in% names(corpus$data)) {
    use_segmentation <- TRUE
    segment_mapping <- corpus$data$segments$mapping
    log_message("Detected segmented corpus", "fit_model")
    
    # Check if we have token counts for weighting
    if ("metadata" %in% names(corpus$data$segments) && 
        "token_count" %in% names(corpus$data$segments$metadata)) {
      segment_weights <- corpus$data$segments$metadata$token_count
      log_message("Using token counts for segment weighting", "fit_model")
    } else {
      # Create equal weights if token counts not available
      segment_weights <- rep(1, nrow(segment_mapping))
      log_message("No token counts found, using equal weights for segments", "fit_model", "WARNING")
    }
  }
  
  ## --- Process prevalence formula --------------------------------------------
  log_message("Processing prevalence formula", "fit_model")
  
  # Handle prevalence formula
  if (is.null(prevalence.formula)) {
    # Default to no covariates
    prev_formula <- stats::as.formula("~ 1")
    log_message("Using default prevalence formula (no covariates)", "fit_model")
  } else if (is.character(prevalence.formula)) {
    # Convert string to formula
    tryCatch({
      prev_formula <- stats::as.formula(prevalence.formula)
      log_message(paste("Using prevalence formula:", prevalence.formula), "fit_model")
    }, error = function(e) {
      log_message(paste("Error parsing prevalence formula:", e$message), "fit_model", "ERROR")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                         paste("Formula error:", e$message))
      prev_formula <- stats::as.formula("~ 1")
      log_message("Falling back to default prevalence formula (no covariates)", "fit_model", "WARNING")
    })
  } else if (inherits(prevalence.formula, "formula")) {
    # Use provided formula
    prev_formula <- prevalence.formula
    log_message(paste("Using provided prevalence formula:", deparse(prev_formula)), "fit_model")
  } else {
    # Invalid input, use default
    log_message("Invalid prevalence formula, using default (no covariates)", "fit_model", "WARNING")
    prev_formula <- stats::as.formula("~ 1")
  }
  
  ## --- Determine optimal k (if not provided) ---------------------------------
  if (is.null(k)) {
    log_message(paste("Determining optimal number of topics (k_min =", k_min, 
                      ", k_max =", k_max, ", k_step =", k_step, ")"), "fit_model")
    
    # Generate sequence of k values to test
    k_values <- seq(k_min, k_max, by = k_step)
    
    # Set seed for reproducibility
    set.seed(seed)
    
    # Search for optimal k
    k_search <- tryCatch({
      stm::searchK(
        documents = documents,
        vocab = vocab,
        K = k_values,
        prevalence = prev_formula,
        data = metadata,
        max.em.its = iterations,
        init.type = "Spectral",
        verbose = TRUE,
        seed = seed
      )
    }, error = function(e) {
      log_message(paste("Error in searchK:", e$message), "fit_model", "ERROR")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                         paste("searchK error:", e$message))
      stop(paste("Error determining optimal k:", e$message))
    })
    
    # Store searchK results
    diagnostics$k_selection$k_values <- k_values
    diagnostics$k_selection$metrics <- k_search$results
    
    # Determine best k
    # Calculate a combined score that balances different metrics
    k_metrics <- data.frame(k_search$results)
    
    # Normalize metrics to 0-1 scale
    normalize <- function(x) {
      if (max(x) == min(x)) return(rep(0, length(x)))
      return((x - min(x)) / (max(x) - min(x)))
    }
    
    # For semanticCoherence and exclusivity, higher is better
    # For residual, lower is better (so we invert)
    k_metrics$norm_coherence <- normalize(k_metrics$semcoh)
    k_metrics$norm_exclusivity <- normalize(k_metrics$exclus)
    k_metrics$norm_residual <- 1 - normalize(k_metrics$residual)
    
    # Calculate combined score (equal weight to each normalized metric)
    k_metrics$score <- (k_metrics$norm_coherence + 
                          k_metrics$norm_exclusivity + 
                          k_metrics$norm_residual) / 3
    
    # Add small penalty for model complexity
    k_metrics$score <- k_metrics$score - 0.01 * normalize(k_metrics$K)
    
    # Find k with highest score
    best_k_idx <- which.max(k_metrics$score)
    best_k <- k_metrics$K[best_k_idx]
    
    log_message(paste("Selected optimal k =", best_k), "fit_model")
    
    # Store selection results
    diagnostics$k_selection$best_k <- best_k
    diagnostics$k_selection$scores <- k_metrics
    
    # Use best k for final model
    k <- best_k
  }
  
  ## --- Fit final STM model --------------------------------------------------
  log_message(paste("Fitting final STM model with k =", k), "fit_model")
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Fit model
  stm_model <- tryCatch({
    stm::stm(
      documents = documents,
      vocab = vocab,
      K = k,
      prevalence = prev_formula,
      data = metadata,
      max.em.its = iterations,
      init.type = "Spectral",
      verbose = FALSE,
      seed = seed
    )
  }, error = function(e) {
    log_message(paste("Error fitting STM model:", e$message), "fit_model", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("STM fitting error:", e$message))
    stop(paste("Error fitting STM model:", e$message))
  })
  
  ## --- Extract topic information --------------------------------------------
  log_message("Extracting topic information", "fit_model")
  
  # Extract document-topic proportions
  topic_proportions <- stm_model$theta
  
  # Get topic labels using different methods
  topic_labels <- tryCatch({
    stm::labelTopics(stm_model, n = 10)
  }, error = function(e) {
    log_message(paste("Error extracting topic labels:", e$message), "fit_model", "WARNING")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("Label extraction error:", e$message))
    NULL
  })
  
  # Create data frames for different label types
  if (!is.null(topic_labels)) {
    # Highest probability labels
    prob_labels <- data.frame(
      topic_id = 1:k,
      label_type = "probability",
      words = apply(topic_labels$prob, 1, paste, collapse = ", ")
    )
    
    # FREX labels (balancing frequency and exclusivity)
    frex_labels <- data.frame(
      topic_id = 1:k,
      label_type = "frex",
      words = apply(topic_labels$frex, 1, paste, collapse = ", ")
    )
    
    # Lift labels
    lift_labels <- data.frame(
      topic_id = 1:k,
      label_type = "lift",
      words = apply(topic_labels$lift, 1, paste, collapse = ", ")
    )
    
    # Score labels
    score_labels <- data.frame(
      topic_id = 1:k,
      label_type = "score",
      words = apply(topic_labels$score, 1, paste, collapse = ", ")
    )
    
    # Combine all label types
    all_labels <- rbind(prob_labels, frex_labels, lift_labels, score_labels)
    
    # Create a wide format for easier use
    label_summary <- data.frame(
      topic_id = 1:k,
      prob_words = prob_labels$words,
      frex_words = frex_labels$words,
      lift_words = lift_labels$words,
      score_words = score_labels$words
    )
  } else {
    # Create empty label data frames if extraction failed
    all_labels <- data.frame(topic_id = integer(0), label_type = character(0), words = character(0))
    label_summary <- data.frame(topic_id = 1:k)
  }
  
  ## --- Aggregate results if segmentation was used ---------------------------
  if (use_segmentation) {
    log_message("Aggregating segment-level results to document level", "fit_model")
    
    # Prepare data frame of topic proportions
    segment_props <- as.data.frame(topic_proportions)
    
    # Add segment ID
    segment_props$segment_id <- rownames(segment_props)
    
    # Join with segment mapping to get original document IDs
    segment_info <- merge(
      segment_props,
      segment_mapping,
      by.x = "segment_id",
      by.y = "segment_id"
    )
    
    # Add weights
    if ("token_count" %in% names(segment_info)) {
      # Use existing token counts
      segment_info$weight <- segment_info$token_count
    } else {
      # Create weights based on segment_size
      segment_info$weight <- segment_info$segment_size
    }
    
    # Normalize weights within each original document
    segment_info <- segment_info %>%
      dplyr::group_by(original_doc_id) %>%
      dplyr::mutate(
        weight_norm = weight / sum(weight)
      ) %>%
      dplyr::ungroup()
    
    # Get topic columns
    topic_cols <- paste0("V", 1:k)
    
    # Perform weighted aggregation to document level
    doc_topic_props <- segment_info %>%
      dplyr::group_by(original_doc_id) %>%
      dplyr::summarize(
        across(all_of(topic_cols), ~ weighted.mean(., weight_norm)),
        .groups = "drop"
      )
    
    # Rename columns for clarity
    names(doc_topic_props)[names(doc_topic_props) == "original_doc_id"] <- "doc_id"
    names(doc_topic_props)[match(topic_cols, names(doc_topic_props))] <- paste0("Topic_", 1:k)
    
    # Create long format
    doc_topic_long <- doc_topic_props %>%
      tidyr::pivot_longer(
        cols = starts_with("Topic_"),
        names_to = "Topic",
        values_to = "Proportion"
      )
    
    # Join with document metadata if available
    if (!is.null(metadata)) {
      # Find the document ID column in metadata
      meta_id_col <- if ("doc_id" %in% names(metadata)) {
        "doc_id"
      } else if ("original_doc_id" %in% names(metadata)) {
        "original_doc_id"
      } else {
        NULL
      }
      
      if (!is.null(meta_id_col)) {
        # Create a mapping from segment IDs to original doc IDs
        id_mapping <- segment_mapping[, c("segment_id", "original_doc_id")]
        
        # Get unique original document IDs
        orig_doc_ids <- unique(id_mapping$original_doc_id)
        
        # Filter metadata to include only original documents
        doc_metadata <- metadata[metadata[[meta_id_col]] %in% orig_doc_ids, ]
        
        # Rename ID column for consistency
        names(doc_metadata)[names(doc_metadata) == meta_id_col] <- "doc_id"
        
        # Join with topic proportions
        doc_topic_long <- dplyr::left_join(
          doc_topic_long,
          doc_metadata,
          by = "doc_id"
        )
      } else {
        log_message("Could not find document ID column in metadata", "fit_model", "WARNING")
      }
    }
    
  } else {
    # No segmentation, just format the topic proportions
    doc_topic_props <- as.data.frame(topic_proportions)
    
    # Add document ID
    doc_topic_props$doc_id <- rownames(doc_topic_props)
    
    # Rename topic columns
    names(doc_topic_props)[1:k] <- paste0("Topic_", 1:k)
    
    # Create long format
    doc_topic_long <- doc_topic_props %>%
      tidyr::pivot_longer(
        cols = starts_with("Topic_"),
        names_to = "Topic",
        values_to = "Proportion"
      )
    
    # Join with metadata if available
    if (!is.null(metadata)) {
      doc_topic_long <- dplyr::left_join(
        doc_topic_long,
        metadata,
        by = "doc_id"
      )
    }
  }
  
  ## --- Calculate model quality metrics ---------------------------------------
  log_message("Calculating model quality metrics", "fit_model")
  
  # Calculate semantic coherence
  coherence <- tryCatch({
    mean(stm::semanticCoherence(stm_model, documents))
  }, error = function(e) {
    log_message(paste("Error calculating coherence:", e$message), "fit_model", "WARNING")
    NA
  })
  
  # Calculate exclusivity
  exclusivity <- tryCatch({
    mean(stm::exclusivity(stm_model))
  }, error = function(e) {
    log_message(paste("Error calculating exclusivity:", e$message), "fit_model", "WARNING")
    NA
  })
  
  # Store quality metrics
  diagnostics$model_quality$coherence <- coherence
  diagnostics$model_quality$exclusivity <- exclusivity
  diagnostics$model_quality$iterations <- stm_model$convergence$its
  diagnostics$model_quality$convergence <- !stm_model$convergence$converged
  
  ## --- Prepare result ---------------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create result data
  result_data <- list(
    topic_proportions = doc_topic_props,  # Wide format
    topic_data = doc_topic_long,          # Long format with metadata
    topic_labels = all_labels,            # All label types
    label_summary = label_summary,        # Wide format labels
    topic_correlations = stm_model$beta$cor  # Topic correlation matrix
  )
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k = k,
    auto_k = is.null(k),
    prevalence_formula = deparse(prev_formula),
    segmentation_used = use_segmentation,
    iterations = iterations,
    topics = k,
    documents = nrow(doc_topic_props),
    seed = seed
  )
  
  # Create final result (excluding full model)
  final_result <- create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  )
  
  log_message(sprintf("Model fitting complete: k = %d, coherence = %.3f, exclusivity = %.3f", 
                      k, coherence, exclusivity), 
              "fit_model")
  
  return(final_result)
}