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
    log_message("Found metadata in stm_data$meta", "fit_model")
  } else if ("metadata" %in% names(corpus$data)) {
    metadata <- corpus$data$metadata
    log_message("Found metadata in corpus$data$metadata", "fit_model")
  } else {
    metadata <- NULL
    log_message("No metadata found in corpus", "fit_model", "WARNING")
  }
  
  # Check if we have metadata and if doc_id exists
  if (!is.null(metadata)) {
    log_message(paste("Metadata has", nrow(metadata), "rows and", ncol(metadata), "columns"), "fit_model")
    if ("doc_id" %in% names(metadata)) {
      log_message("Found doc_id column in metadata", "fit_model")
      # Ensure doc_id is character type for consistent joining
      metadata$doc_id <- as.character(metadata$doc_id)
      log_message(paste("First few doc_ids:", paste(head(metadata$doc_id, 3), collapse=", ")), "fit_model")
    } else {
      log_message("No doc_id column found in metadata", "fit_model", "WARNING")
      log_message(paste("Available columns:", paste(names(metadata), collapse=", ")), "fit_model")
    }
  }
  
  # Check for segmentation
  use_segmentation <- FALSE
  segment_mapping <- NULL
  
  if ("segments" %in% names(corpus$data)) {
    use_segmentation <- TRUE
    if ("mapping" %in% names(corpus$data$segments)) {
      segment_mapping <- corpus$data$segments$mapping
      log_message(paste("Detected segmented corpus with", nrow(segment_mapping), "segments"), "fit_model")
      
      # Convert segment_id to character
      segment_mapping$segment_id <- as.character(segment_mapping$segment_id)
      segment_mapping$original_doc_id <- as.character(segment_mapping$original_doc_id)
    } else {
      log_message("Segmented corpus but no mapping found", "fit_model", "WARNING")
    }
    
    # Check if we have token counts for weighting
    if ("metadata" %in% names(corpus$data$segments) && 
        "token_count" %in% names(corpus$data$segments$metadata)) {
      segment_weights <- corpus$data$segments$metadata$token_count
      log_message("Using token counts for segment weighting", "fit_model")
    } else {
      # Create equal weights if token counts not available
      if (!is.null(segment_mapping)) {
        segment_weights <- rep(1, nrow(segment_mapping))
        log_message("No token counts found, using equal weights for segments", "fit_model", "WARNING")
      }
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
    
    # Initialize results storage
    model_results <- data.frame(
      k = integer(),
      coherence = numeric(),
      exclusivity = numeric(),
      held_out_likelihood = numeric(),
      complexity_penalty = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Test each k value individually
    for (k_val in k_values) {
      log_message(paste("Testing model with k =", k_val), "fit_model")
      
      # Set seed for reproducibility
      set.seed(seed)
      
      # Fit model with current k
      model_k <- tryCatch({
        stm::stm(
          documents = documents,
          vocab = vocab,
          K = k_val,
          prevalence = prev_formula,
          data = metadata,
          max.em.its = iterations,
          init.type = "Spectral",
          verbose = FALSE,
          seed = seed
        )
      }, error = function(e) {
        log_message(paste("Error fitting model with k =", k_val, ":", e$message), 
                    "fit_model", "WARNING")
        return(NULL)
      })
      
      if (!is.null(model_k)) {
        # Calculate metrics
        coherence_val <- tryCatch({
          mean(stm::semanticCoherence(model_k, documents))
        }, error = function(e) {
          log_message(paste("Coherence calculation failed:", e$message), 
                      "fit_model", "WARNING")
          NA_real_
        })
        
        exclusivity_val <- tryCatch({
          mean(stm::exclusivity(model_k))
        }, error = function(e) {
          log_message(paste("Exclusivity calculation failed:", e$message), 
                      "fit_model", "WARNING")
          NA_real_
        })
        
        # Simple held-out likelihood calculation
        heldout_val <- tryCatch({
          as.numeric(logLik(model_k))
        }, error = function(e) {
          log_message(paste("Held-out likelihood calculation failed:", e$message), 
                      "fit_model", "WARNING")
          NA_real_
        })
        
        # Apply complexity penalty
        complexity_penalty <- 0.01 * k_val
        
        # Add to results
        model_results <- rbind(
          model_results,
          data.frame(
            k = k_val,
            coherence = coherence_val,
            exclusivity = exclusivity_val,
            held_out_likelihood = heldout_val,
            complexity_penalty = complexity_penalty,
            stringsAsFactors = FALSE
          )
        )
        
        # Clean up to save memory
        rm(model_k)
        gc()
      }
    }
    
    # Calculate normalized scores for comparison
    if (nrow(model_results) > 0) {
      # Normalize metrics
      normalize <- function(x) {
        if (length(x) <= 1 || all(is.na(x)) || (max(x, na.rm = TRUE) == min(x, na.rm = TRUE))) {
          return(rep(0, length(x)))
        }
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      }
      
      model_results$coherence_norm <- normalize(model_results$coherence)
      model_results$exclusivity_norm <- normalize(model_results$exclusivity)
      if (any(!is.na(model_results$held_out_likelihood))) {
        model_results$likelihood_norm <- normalize(model_results$held_out_likelihood)
      } else {
        model_results$likelihood_norm <- 0
      }
      
      # Calculate combined score
      model_results$score <- (model_results$coherence_norm + 
                                model_results$exclusivity_norm + 
                                model_results$likelihood_norm) - 
        model_results$complexity_penalty
      
      # Find best k
      best_row <- model_results[which.max(model_results$score), ]
      best_k <- best_row$k
      
      log_message(paste("Selected optimal k =", best_k), "fit_model")
      
      # Store selection results
      diagnostics$k_selection$k_values <- k_values
      diagnostics$k_selection$metrics <- model_results
      diagnostics$k_selection$best_k <- best_k
      
      # Use best k for final model
      k <- best_k
    } else {
      log_message("No valid models found during k selection, using default k = 30", "fit_model", "WARNING")
      k <- 30
    }
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
  
  # Check model structure
  log_message("Model fitted successfully - inspecting structure", "fit_model")
  log_message(paste("Model has components:", paste(names(stm_model), collapse=", ")), "fit_model")
  
  ## --- Extract topic information --------------------------------------------
  log_message("Extracting topic information", "fit_model")
  
  # Extract document-topic proportions
  topic_proportions <- stm_model$theta
  log_message(paste("topic_proportions dimensions:", nrow(topic_proportions), "x", ncol(topic_proportions)), "fit_model")
  log_message(paste("topic_proportions has rownames:", !is.null(rownames(topic_proportions))), "fit_model")
  
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
  
  ## --- Process topic proportions based on segmentation -----------------------
  log_message("Processing topic proportions", "fit_model")
  
  if (!use_segmentation) {
    # No segmentation - direct processing
    log_message("Processing non-segmented topic proportions", "fit_model")
    
    # Extract topic proportions
    doc_topic_props <- as.data.frame(topic_proportions)
    
    # Get document IDs - these might be numeric in STM output
    if (!is.null(rownames(topic_proportions))) {
      # Use rownames if they exist
      doc_ids <- rownames(topic_proportions)
    } else {
      # Create sequential IDs if no rownames
      doc_ids <- as.character(1:nrow(topic_proportions))
      log_message("No rownames found in topic proportions, using sequential IDs", "fit_model", "WARNING")
    }
    
    # Always convert to character for consistent joining
    doc_topic_props$doc_id <- as.character(doc_ids)
    
    # Ensure metadata has character doc_ids for consistent joining
    if (!is.null(metadata) && "doc_id" %in% names(metadata)) {
      metadata$doc_id <- as.character(metadata$doc_id)
    }
    
    # Rename topic columns
    names(doc_topic_props)[1:k] <- paste0("Topic_", 1:k)
    
    # Create long format
    doc_topic_long <- doc_topic_props %>%
      tidyr::pivot_longer(
        cols = starts_with("Topic_"),
        names_to = "Topic",
        values_to = "Proportion"
      )
    
    # Join with metadata if available - use left_join to preserve all topic data
    if (!is.null(metadata) && "doc_id" %in% names(metadata)) {
      # Check ID overlap first
      overlap_ids <- intersect(doc_topic_long$doc_id, metadata$doc_id)
      log_message(paste("Found", length(overlap_ids), "overlapping document IDs for joining"), "fit_model")
      
      if (length(overlap_ids) == 0) {
        log_message("WARNING: No document ID overlap for joining!", "fit_model", "WARNING")
        log_message(paste("First few doc_ids in topics:", paste(head(unique(doc_topic_long$doc_id)), collapse=", ")), "fit_model")
        log_message(paste("First few doc_ids in metadata:", paste(head(metadata$doc_id), collapse=", ")), "fit_model")
        
        # Return doc_topic_long without attempting the join
      } else {
        # Proceed with join
        doc_topic_long <- dplyr::left_join(
          doc_topic_long,
          metadata,
          by = "doc_id"
        )
      }
    }
    
  } else {
    # Segmentation case - need to aggregate
    log_message("Processing segmented topic proportions", "fit_model")
    
    # Ensure we have valid segment mapping
    if (is.null(segment_mapping) || nrow(segment_mapping) == 0) {
      log_message("No valid segment mapping found, cannot aggregate", "fit_model", "ERROR")
      stop("No valid segment mapping found, cannot aggregate")
    }
    
    # Prepare data frame of topic proportions
    segment_props <- as.data.frame(topic_proportions)
    
    # Add segment ID
    if (!is.null(rownames(topic_proportions))) {
      segment_props$segment_id <- rownames(topic_proportions)
    } else {
      # Create sequential IDs if no rownames
      segment_props$segment_id <- as.character(1:nrow(topic_proportions))
      log_message("No rownames found in topic proportions, using sequential IDs", "fit_model", "WARNING")
    }
    
    # Ensure segment IDs are character type
    segment_props$segment_id <- as.character(segment_props$segment_id)
    
    # Debug segment IDs
    log_message(paste("segment_props has", nrow(segment_props), "rows"), "fit_model")
    log_message(paste("First few segment_props IDs:", paste(head(segment_props$segment_id, 3), collapse=", ")), "fit_model")
    log_message(paste("segment_mapping has", nrow(segment_mapping), "rows"), "fit_model")
    log_message(paste("First few segment_mapping IDs:", paste(head(segment_mapping$segment_id, 3), collapse=", ")), "fit_model")
    
    # Check for overlap between segment IDs
    overlap_segments <- intersect(segment_props$segment_id, segment_mapping$segment_id)
    log_message(paste("Found", length(overlap_segments), "overlapping segment IDs out of", 
                      nrow(segment_props), "segments for mapping"), "fit_model")
    
    if (length(overlap_segments) == 0) {
      log_message("WARNING: No segment ID overlap for mapping!", "fit_model", "WARNING")
      
      # Create a basic mapping to avoid empty results
      log_message("Creating a basic mapping to avoid empty results", "fit_model", "WARNING")
      segment_info <- segment_props
      segment_info$original_doc_id <- paste0("doc_", 1:nrow(segment_props))
      segment_info$weight <- 1
      segment_info$weight_norm <- 1
    } else {
      # Join with segment mapping to get original document IDs
      segment_info <- merge(
        segment_props,
        segment_mapping,
        by.x = "segment_id",
        by.y = "segment_id"
      )
      
      log_message(paste("Joined data has", nrow(segment_info), "rows"), "fit_model")
      
      # Add weights
      if (exists("segment_weights") && length(segment_weights) == nrow(segment_mapping)) {
        # Use existing token counts
        segment_info$weight <- segment_weights[match(segment_info$segment_id, segment_mapping$segment_id)]
      } else if ("token_count" %in% names(segment_info)) {
        # Use token counts from join
        segment_info$weight <- segment_info$token_count
      } else if ("segment_size" %in% names(segment_info)) {
        # Create weights based on segment_size
        segment_info$weight <- segment_info$segment_size
      } else {
        # Equal weights as fallback
        segment_info$weight <- 1
      }
      
      # Normalize weights within each original document
      segment_info <- segment_info %>%
        dplyr::group_by(original_doc_id) %>%
        dplyr::mutate(
          weight_norm = weight / sum(weight)
        ) %>%
        dplyr::ungroup()
    }
    
    # Get topic columns
    topic_cols <- paste0("V", 1:k)
    
    # Perform weighted aggregation to document level
    doc_topic_props <- segment_info %>%
      dplyr::group_by(original_doc_id) %>%
      dplyr::summarize(
        across(all_of(topic_cols), ~ weighted.mean(., weight_norm)),
        .groups = "drop"
      )
    
    log_message(paste("Aggregated to", nrow(doc_topic_props), "documents"), "fit_model")
    
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
        original_metadata <- metadata
        names(original_metadata)[names(original_metadata) == meta_id_col] <- "doc_id"
        
        # Check for overlap
        overlap_docs <- intersect(doc_topic_long$doc_id, original_metadata$doc_id)
        log_message(paste("Found", length(overlap_docs), "overlapping document IDs for metadata joining"), "fit_model")
        
        if (length(overlap_docs) > 0) {
          # Join with topic proportions
          doc_topic_long <- dplyr::left_join(
            doc_topic_long,
            original_metadata,
            by = "doc_id"
          )
        } else {
          log_message("WARNING: No document ID overlap for metadata joining", "fit_model", "WARNING")
        }
      } else {
        log_message("Could not find document ID column in metadata", "fit_model", "WARNING")
      }
    }
  }
  
  # Debug output for final data structures
  log_message(paste("Final doc_topic_props has", nrow(doc_topic_props), "rows and", 
                    ncol(doc_topic_props), "columns"), "fit_model")
  log_message(paste("Final doc_topic_long has", nrow(doc_topic_long), "rows and", 
                    ncol(doc_topic_long), "columns"), "fit_model")
  
  ## --- Calculate topic correlations -----------------------------------------
  log_message("Calculating topic correlations", "fit_model")
  
  # Calculate topic correlations
  topic_correlations <- tryCatch({
    # Use topicCorr function
    corr_result <- stm::topicCorr(stm_model)
    
    # Check if result has correlation matrix
    if (!is.null(corr_result) && !is.null(corr_result$cor)) {
      corr_result$cor
    } else {
      log_message("Topic correlation calculation returned NULL or missing correlation matrix", 
                  "fit_model", "WARNING")
      # Create empty correlation matrix with proper dimensions
      matrix(0, nrow = k, ncol = k)
    }
  }, error = function(e) {
    log_message(paste("Error calculating topic correlations:", e$message), 
                "fit_model", "WARNING")
    # Create empty correlation matrix with proper dimensions
    matrix(0, nrow = k, ncol = k)
  })
  
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
    topic_correlations = topic_correlations  # Topic correlation matrix
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