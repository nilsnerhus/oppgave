#' @title Fit structural topic model with optional segmentation
#' @description Takes a preprocessed corpus, optionally segments documents by token count,
#'   creates a document-feature matrix, and fits a structural topic model (STM).
#'   Includes automatic topic number determination if k is not specified.
#'
#' @param corpus Output from prepare_corpus() function
#' @param k Number of topics to use (default: NULL, auto-determines optimal k)
#' @param k_min Minimum number of topics to try if auto-determining (default: 20)
#' @param k_max Maximum number of topics to try if auto-determining (default: 80)
#' @param k_step Step size for k search (default: 10)
#' @param segment Number of tokens per segment (default: 1000, 0 for no segmentation)
#' @param prevalence Formula for metadata covariates (default: NULL)
#' @param iterations Maximum EM algorithm iterations (default: 150)
#' @param output_path Path to save results (default: "data/topic_model.rds")
#'
#' @return A list containing:
#'   \item{data}{Document-topic proportions, topic labels, and other model outputs}
#'   \item{metadata}{Processing information and model parameters}
#'   \item{diagnostics}{Model quality metrics and processing information}
#'
#' @examples
#' \dontrun{
#' # Fit model with automatic k determination, no segmentation
#' model <- fit_model(corpus, segment = 0)
#' 
#' # Fit model with 30 topics and 500-token segments
#' model <- fit_model(corpus, k = 30, segment = 500)
#' 
#' # Fit model with metadata covariates
#' model <- fit_model(corpus, 
#'                   prevalence = ~ region + wb_income_level)
#' }

fit_model <- function(
    corpus,
    k = NULL,
    k_min = 20,
    k_max = 80, 
    k_step = 10,
    segment = 1000,
    prevalence = NULL,
    iterations = 150,
    output_path = "data/topic_model.rds"
) {
  ## --- Housekeeping ----------------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Set seed for reproducibility
  seed <- 1234
  set.seed(seed)
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    model_quality = list(),
    k_selection = list(),
    segmentation = list(),
    processing_issues = character()
  )
  
  ## --- Input validation ------------------------------------------------------
  log_message("Validating input corpus", "fit_model")
  
  tryCatch({
    # Check that corpus has the expected structure
    if (!is.list(corpus) || !"data" %in% names(corpus)) {
      stop("Input must be the output from prepare_corpus()")
    }
    
    # Validate corpus data components
    required_components <- c("tokens", "vocab", "metadata")
    missing_components <- setdiff(required_components, names(corpus$data))
    if (length(missing_components) > 0) {
      stop("Corpus missing required components: ", paste(missing_components, collapse = ", "))
    }
    
    # Validate k parameters
    if (!is.null(k)) {
      if (!is.numeric(k) || k < 2) {
        stop("k must be a number greater than 1")
      }
    } else {
      if (!is.numeric(k_min) || !is.numeric(k_max) || !is.numeric(k_step)) {
        stop("k_min, k_max, and k_step must be numbers")
      }
      if (k_min < 2) {
        stop("k_min must be a number greater than 1")
      }
      if (k_max <= k_min) {
        stop("k_max must be greater than k_min")
      }
      if (k_step <= 0) {
        stop("k_step must must be a number greater than 0")
      }
    }
    
    # Validate segmentation parameter
    if (!is.numeric(segment) || segment < 0) {
      stop("segment must be a non-negative integer")
    }
    
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "fit_model", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, e$message)
    stop(e$message)
  })
  
  ## --- Extract corpus components ---------------------------------------------
  log_message("Extracting corpus components", "fit_model")
  
  # Extract tokens
  tokens <- corpus$data$tokens
  
  # Extract vocabulary if available
  if ("vocab" %in% names(corpus$data)) {
    vocabulary <- corpus$data$vocab
  } else {
    vocabulary <- sort(unique(tokens$word))
    log_message("Created vocabulary from token data", "fit_model")
  }
  
  # Extract metadata
  metadata <- corpus$data$metadata
  
  # Check for required columns in tokens
  if (!all(c("doc_id", "word") %in% names(tokens))) {
    log_message("Tokens dataframe missing required columns (doc_id, word)", "fit_model", "ERROR")
    stop("Tokens dataframe must contain doc_id and word columns")
  }
  
  # Count original documents
  original_docs <- length(unique(tokens$doc_id))
  log_message(paste("Corpus has", original_docs, "documents and", 
                    length(vocabulary), "unique terms"), "fit_model")
  
  ## --- Apply segmentation ----------------------------------------------------
  use_segmentation <- segment > 0
  segment_mapping <- NULL
  
  if (use_segmentation) {
    log_message(paste("Segmenting documents into chunks of", segment, "tokens"), "fit_model")
    
    # Initialize segment storage
    segmented_tokens <- list()
    segment_mapping <- data.frame(
      segment_id = character(0),
      original_doc_id = character(0),
      segment_num = integer(0),
      token_count = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Get unique document IDs
    doc_ids <- unique(tokens$doc_id)
    
    # Process each document for segmentation
    for (doc_id in doc_ids) {
      # Extract tokens for this document
      doc_tokens <- tokens[tokens$doc_id == doc_id, ]
      total_tokens <- nrow(doc_tokens)
      
      # Skip empty documents
      if (total_tokens == 0) {
        log_message(paste("Skipping empty document:", doc_id), "fit_model", "WARNING")
        next
      }
      
      # Calculate number of segments needed
      n_segments <- ceiling(total_tokens / segment)
      
      # If document is smaller than segmentation threshold, keep as one segment
      if (total_tokens <= segment) {
        segment_id <- paste0(doc_id, "_1")
        
        # Create segment with original tokens
        doc_tokens$segment_id <- segment_id
        segmented_tokens[[segment_id]] <- doc_tokens
        
        # Add to segment mapping
        segment_mapping <- rbind(segment_mapping, data.frame(
          segment_id = segment_id,
          original_doc_id = doc_id,
          segment_num = 1,
          token_count = total_tokens,
          stringsAsFactors = FALSE
        ))
      } else {
        # Split into segments of specified size
        for (j in 1:n_segments) {
          start_idx <- (j - 1) * segment + 1
          end_idx <- min(j * segment, total_tokens)
          
          if (start_idx <= total_tokens) {
            # Create segment ID
            segment_id <- paste0(doc_id, "_", j)
            
            # Extract tokens for this segment
            segment_tokens <- doc_tokens[start_idx:end_idx, ]
            segment_tokens$segment_id <- segment_id
            segmented_tokens[[segment_id]] <- segment_tokens
            
            # Add to segment mapping
            segment_mapping <- rbind(segment_mapping, data.frame(
              segment_id = segment_id,
              original_doc_id = doc_id,
              segment_num = j,
              token_count = end_idx - start_idx + 1,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
    
    # Combine all segmented tokens
    tokens <- do.call(rbind, segmented_tokens)
    
    # Update document IDs to use segment IDs
    tokens$doc_id <- tokens$segment_id
    
    log_message(paste("Created", nrow(segment_mapping), "segments from", 
                      length(doc_ids), "documents"), "fit_model")
    
    # Store segmentation info in diagnostics
    diagnostics$segmentation$segment <- segment
    diagnostics$segmentation$original_docs <- original_docs
    diagnostics$segmentation$segment_count <- nrow(segment_mapping)
    
    # Add metadata to segments
    if (nrow(metadata) > 0) {
      # Create lookup table for original document metadata
      segment_metadata <- merge(segment_mapping, metadata,
                                by.x = "original_doc_id", by.y = "doc_id")
      
      # Rename segment_id to doc_id for consistency
      names(segment_metadata)[names(segment_metadata) == "segment_id"] <- "doc_id"
      
      # Use segment metadata for modeling
      metadata <- segment_metadata
    }
  } else {
    log_message("Using documents without segmentation", "fit_model")
  }
  
  ## --- Create document-term matrix and convert to STM format ----------------
  log_message("Creating document-term matrix", "fit_model")
  
  # Count word frequencies
  word_counts <- tokens %>%
    dplyr::count(doc_id, word)
  
  # Create document-feature matrix using tidytext
  dfm_object <- tryCatch({
    word_counts %>%
      tidytext::cast_dfm(doc_id, word, n)
  }, error = function(e) {
    log_message(paste("DFM creation error:", e$message), "fit_model", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("DFM creation failed:", e$message))
    stop(e$message)
  })
  
  # Convert to STM format
  stm_data <- tryCatch({
    # Convert DFM to STM format
    stm_result <- quanteda::convert(dfm_object, to = "stm")
    
    # Set document names to match IDs
    stm_result$documents.dimnames <- rownames(dfm_object)
    
    stm_result
  }, error = function(e) {
    log_message(paste("STM conversion error:", e$message), "fit_model", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("STM conversion failed:", e$message))
    stop(e$message)
  })
  
  ## --- Process prevalence formula --------------------------------------------
  log_message("Processing prevalence formula", "fit_model")
  
  # Handle prevalence formula
  if (is.null(prevalence)) {
    # Default to no covariates
    prev_formula <- stats::as.formula("~ 1")
    log_message("Using default prevalence formula (no covariates)", "fit_model")
  } else if (is.character(prevalence)) {
    # Convert string to formula
    tryCatch({
      prev_formula <- stats::as.formula(prevalence)
      log_message(paste("Using prevalence formula:", prevalence), "fit_model")
    }, error = function(e) {
      log_message(paste("Error parsing prevalence formula:", e$message), "fit_model", "ERROR")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                         paste("Formula error:", e$message))
      prev_formula <- stats::as.formula("~ 1")
      log_message("Falling back to default prevalence formula (no covariates)", "fit_model", "WARNING")
    })
  } else if (inherits(prevalence, "formula")) {
    # Use provided formula
    prev_formula <- prevalence
    log_message(paste("Using provided prevalence formula:", deparse(prev_formula)), "fit_model")
  } else {
    # Invalid input, use default
    log_message("Invalid prevalence formula, using default (no covariates)", "fit_model", "WARNING")
    prev_formula <- stats::as.formula("~ 1")
  }
  
  ## --- Prepare metadata for STM ----------------------------------------------
  # Ensure metadata is properly formatted for STM
  if (!is.null(metadata) && nrow(metadata) > 0) {
    # Check if metadata has doc_id
    if ("doc_id" %in% names(metadata)) {
      # Match metadata to documents in the correct order
      doc_ids <- rownames(dfm_object)
      
      # Filter metadata to include only documents in the dfm
      metadata_filtered <- metadata[metadata$doc_id %in% doc_ids, ]
      
      # Sort metadata to match dfm order
      metadata_ordered <- metadata_filtered[match(doc_ids, metadata_filtered$doc_id), ]
      
      # Remove doc_id column as it's not needed for STM
      metadata_final <- metadata_ordered[, !names(metadata_ordered) %in% c("doc_id")]
      
      # Check if there are actually any metadata columns left
      if (ncol(metadata_final) == 0) {
        metadata_final <- NULL
        log_message("No usable metadata columns found after filtering", "fit_model", "WARNING")
      }
    } else {
      metadata_final <- NULL
      log_message("Metadata missing doc_id column, cannot use for modeling", "fit_model", "WARNING")
    }
  } else {
    metadata_final <- NULL
  }
  
  ## --- Determine optimal k ---------------------------------------------------
  if (is.null(k)) {
    log_message(paste("Determining optimal number of topics (k_min =", k_min, 
                      ", k_max =", k_max, ", k_step =", k_step, ")"), "fit_model")
    
    # Generate sequence of k values to test
    k_values <- seq(k_min, k_max, by = k_step)
    
    # Search for optimal k
    k_search <- tryCatch({
      stm::searchK(
        documents = stm_data$documents,
        vocab = stm_data$vocab,
        K = k_values,
        prevalence = prev_formula,
        data = metadata_final,
        max.em.its = iterations / 2,  # Use fewer iterations for search
        init.type = "Spectral",
        verbose = FALSE,
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
  
  # Fit model
  stm_model <- tryCatch({
    stm::stm(
      documents = stm_data$documents,
      vocab = stm_data$vocab,
      K = k,
      prevalence = prev_formula,
      data = metadata_final,
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
  
  # Add document IDs
  doc_ids <- rownames(dfm_object)
  topic_proportions_df <- as.data.frame(topic_proportions)
  topic_proportions_df$doc_id <- doc_ids
  
  # Rename topic columns
  names(topic_proportions_df)[1:k] <- paste0("Topic_", 1:k)
  
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
  
  ## --- Aggregate results -----------------------------------------------------
  if (use_segmentation) {
    log_message("Aggregating segment-level results to document level", "fit_model")
    
    # Merge topic proportions with segment mapping
    segment_props <- merge(
      topic_proportions_df,
      segment_mapping,
      by.x = "doc_id",
      by.y = "segment_id"
    )
    
    # Aggregate to document level - simple average across segments
    doc_topic_cols <- paste0("Topic_", 1:k)
    
    doc_topic_props <- segment_props %>%
      dplyr::group_by(original_doc_id) %>%
      dplyr::summarize(
        across(all_of(doc_topic_cols), mean),
        .groups = "drop"
      )
    
    # Rename for consistency
    names(doc_topic_props)[names(doc_topic_props) == "original_doc_id"] <- "doc_id"
    
    # Get document-level metadata (before segmentation)
    if (!is.null(corpus$data$metadata)) {
      doc_metadata <- corpus$data$metadata
      
      # Merge with aggregated topic proportions
      doc_topic_props <- merge(doc_topic_props, doc_metadata, by = "doc_id", all.x = TRUE)
    }
    
    # Store both segment-level and document-level results
    topic_results <- list(
      segment_level = topic_proportions_df,
      document_level = doc_topic_props,
      segment_mapping = segment_mapping
    )
  } else {
    # No segmentation - just use the topic proportions directly
    if (!is.null(metadata) && nrow(metadata) > 0) {
      # Add metadata back to topic proportions
      doc_topic_props <- merge(topic_proportions_df, metadata, by = "doc_id", all.x = TRUE)
    } else {
      doc_topic_props <- topic_proportions_df
    }
    
    # Store only document-level results
    topic_results <- list(
      document_level = doc_topic_props
    )
  }
  
  ## --- Calculate model quality metrics ---------------------------------------
  log_message("Calculating model quality metrics", "fit_model")
  
  # Calculate semantic coherence
  coherence <- tryCatch({
    mean(stm::semanticCoherence(stm_model, stm_data$documents))
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
  
  ## --- Create long format for topic data ------------------------------------
  # Convert to long format for easier analysis
  doc_topic_long <- topic_results$document_level %>%
    tidyr::pivot_longer(
      cols = starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  ## --- Prepare result ---------------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create result data
  result_data <- list(
    # Core model results
    topic_proportions = topic_results$document_level,  # Document-level results
    topic_data = doc_topic_long,                      # Long format for easy analysis
    topic_labels = all_labels,                        # All label types
    label_summary = label_summary,                    # Wide format labels
    topic_correlations = stm_model$beta$cor,          # Topic correlation matrix
    
    # If segmentation was used, include segment information
    segmentation = if(use_segmentation) {
      list(
        segment_proportions = topic_results$segment_level,
        segment_mapping = topic_results$segment_mapping
      )
    } else {
      NULL
    },
    
    # Include model object for potential further analysis
    model = stm_model
  )
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k = k,
    auto_k = is.null(k),
    prevalence_formula = deparse(prev_formula),
    segmentation_used = use_segmentation,
    segment = segment,
    iterations = iterations,
    documents = if(use_segmentation) length(unique(segment_mapping$original_doc_id)) else nrow(topic_proportions_df),
    segments = if(use_segmentation) nrow(segment_mapping) else NA,
    seed = seed
  )
  
  # Create final result
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