#' @title Fit optimal structural topic model with effects analysis
#' @description Fits a structural topic model to tokenized text data, automatically
#'   determining the optimal number of topics or using a specified value. Supports 
#'   document segmentation, prevalence modeling with metadata, content covariates, 
#'   and pre-calculates topic effects for downstream analysis.
#'   
#' @param tokens Result from validate_tokens() function containing tokens and vocabulary
#' @param config Configuration from add_metadata including metadata and category_map
#' @param k Number of topics to use (default: NULL, auto-determines optimal k)
#' @param k_search_config Topic number search configuration:
#'   \itemize{
#'     \item min - Minimum number of topics to try (default: 5)
#'     \item max - Maximum number of topics to try (default: 40)
#'     \item step - Step size for k search (default: 5)
#'   }
#' @param modeling_config Model fitting configuration:
#'   \itemize{
#'     \item segment - Number of tokens per segment (default: 0, no segmentation)
#'     \item prevalence_formula - Custom formula for metadata covariates (default: NULL, auto-generated)
#'     \item content_covariates - Variables affecting word distribution within topics (default: NULL)
#'     \item iterations - Maximum EM algorithm iterations (default: 200)
#'     \item seed - Random seed for reproducibility (default: 1234)
#'     \item include_model - Whether to include full model in output (default: TRUE)
#'   }
#' @param model_weights Advanced STM model weights and parameters:
#'   \itemize{
#'     \item gamma.prior - Prior on document-topic proportions (default: NULL)
#'     \item sigma.prior - Prior on topic-word distributions (default: NULL)
#'     \item alpha - Parameter for exclusivity calculation (default: NULL)
#'     \item kappa - Regularization parameter (default: NULL)
#'   }
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item model - Fitted STM model object (optional, can be excluded to save memory)
#'       \item topic_proportions - Document-topic proportion matrix (wide format)
#'       \item topic_data - Long-format topic data with metadata
#'       \item topic_terms - Top terms for each topic using different metrics
#'       \item topic_correlations - Topic correlation matrix
#'       \item segments - Segmentation information (if used)
#'       \item effects - Pre-calculated estimateEffect results for each dimension
#'       \item variance_explained - Percentage of variance explained by each dimension
#'     }
#'   }
#'   \item{metadata}{Processing information and model parameters}
#'   \item{diagnostics}{Model quality metrics and k-selection information}
#'
#' @examples
#' \dontrun{
#' # Fit model with automatic k determination and default effect calculation
#' model <- fit_model(tokens$data, nap_data$data$config)
#' 
#' # Fit model with content covariates and custom weights
#' modeling_config <- list(
#'   segment = 500,  # Use 500-token segments
#'   content_covariates = c("wb_income_level")  # Word distributions vary by income level
#' )
#' model_weights <- list(
#'   kappa = 0.1,  # Lower regularization
#'   alpha = 50    # Higher exclusivity weight
#' )
#' model <- fit_model(tokens$data, nap_data$data$config, 
#'                  k = 20, 
#'                  modeling_config = modeling_config,
#'                  model_weights = model_weights)
#' }
fit_model <- function(
    tokens, 
    config,
    k = NULL,
    k_search_config = list(
      min = 5,
      max = 40,
      step = 5
    ),
    modeling_config = list(
      segment = 0,                # Default: no segmentation
      prevalence_formula = NULL,  # Default: auto-generated from category_map
      content_covariates = NULL,  # Default: no content covariates
      iterations = 200,
      seed = 1234,
      include_model = FALSE        # Whether to include full model in output
    ),
    model_weights = list(
      gamma.prior = NULL,         # Prior on document-topic proportions
      sigma.prior = NULL,         # Prior on topic-word distributions
      alpha = NULL,               # Parameter for exclusivity calculation
      kappa = NULL                # Regularization parameter
    )
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Set default parameters for any missing config options
  k_search_config$min <- k_search_config$min %||% 5
  k_search_config$max <- k_search_config$max %||% 40
  k_search_config$step <- k_search_config$step %||% 5
  
  modeling_config$segment <- modeling_config$segment %||% 0
  modeling_config$iterations <- modeling_config$iterations %||% 200
  modeling_config$seed <- modeling_config$seed %||% 1234
  modeling_config$include_model <- modeling_config$include_model %||% TRUE
  
  # Set seed for reproducibility
  set.seed(modeling_config$seed)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    model_quality = list(),
    k_selection = list(),
    segmentation = list(),
    effects_analysis = list(),
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "fit_model")
  
  # Validate tokens structure
  if (!is.list(tokens) || 
      !all(c("tokens", "vocab", "metadata") %in% names(tokens))) {
    error_msg <- "tokens must be a list with 'tokens', 'vocab', and 'metadata' components"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate config
  if (!is.list(config) || 
      !all(c("metadata", "category_map") %in% names(config))) {
    error_msg <- "config must be a list with 'metadata' and 'category_map' components"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate tokens dataframe
  tokens_df <- tokens$tokens
  if (!"doc_id" %in% names(tokens_df) || !"word" %in% names(tokens_df)) {
    error_msg <- "tokens dataframe must contain 'doc_id' and 'word' columns"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate vocabulary
  vocabulary <- tokens$vocab
  if (length(vocabulary) == 0) {
    error_msg <- "vocabulary is empty"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Count original documents
  original_docs <- length(unique(tokens_df$doc_id))
  log_message(paste("Processing", original_docs, "documents with", 
                    length(vocabulary), "unique terms"), "fit_model")
  
  ## --- STM data preparation ---------------------------------------------------
  log_message("Preparing data for STM", "fit_model")
  
  # Count word frequencies
  word_counts <- tokens_df %>%
    dplyr::count(doc_id, word)
  
  # Create document-feature matrix using tidytext
  dfm_object <- tryCatch({
    word_counts %>%
      tidytext::cast_dfm(doc_id, word, n)
  }, error = function(e) {
    error_msg <- paste("DFM creation error:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    stop(error_msg)
  })
  
  # Convert to STM format
  stm_data <- tryCatch({
    # Convert DFM to STM format
    stm_result <- quanteda::convert(dfm_object, to = "stm")
    
    # Set document names to match IDs
    stm_result$documents.dimnames <- rownames(dfm_object)
    
    stm_result
  }, error = function(e) {
    error_msg <- paste("STM conversion error:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    stop(error_msg)
  })
  
  ## --- Segmentation (if requested) --------------------------------------------
  use_segmentation <- modeling_config$segment > 0
  segment_mapping <- NULL
  
  if (use_segmentation) {
    log_message(paste("Segmenting documents into chunks of", 
                      modeling_config$segment, "tokens"), "fit_model")
    
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
    doc_ids <- unique(tokens_df$doc_id)
    
    # Process each document for segmentation
    for (doc_id in doc_ids) {
      # Extract tokens for this document
      doc_tokens <- tokens_df[tokens_df$doc_id == doc_id, ]
      total_tokens <- nrow(doc_tokens)
      
      # Skip empty documents
      if (total_tokens == 0) {
        log_message(paste("Skipping empty document:", doc_id), "fit_model", "WARNING")
        next
      }
      
      # Calculate number of segments needed
      n_segments <- ceiling(total_tokens / modeling_config$segment)
      
      # If document is smaller than segmentation threshold, keep as one segment
      if (total_tokens <= modeling_config$segment) {
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
          start_idx <- (j - 1) * modeling_config$segment + 1
          end_idx <- min(j * modeling_config$segment, total_tokens)
          
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
    tokens_df <- do.call(rbind, segmented_tokens)
    
    # Update document IDs to use segment IDs
    tokens_df$doc_id <- tokens_df$segment_id
    
    log_message(paste("Created", nrow(segment_mapping), "segments from", 
                      length(doc_ids), "documents"), "fit_model")
    
    # Store segmentation info in diagnostics
    diagnostics$segmentation$segment_size <- modeling_config$segment
    diagnostics$segmentation$original_docs <- original_docs
    diagnostics$segmentation$segment_count <- nrow(segment_mapping)
    
    # We need to redo the document-term matrix creation for segments
    # Count word frequencies
    word_counts <- tokens_df %>%
      dplyr::count(doc_id, word)
    
    # Create document-feature matrix using tidytext
    dfm_object <- word_counts %>%
      tidytext::cast_dfm(doc_id, word, n)
    
    # Convert to STM format
    stm_data <- quanteda::convert(dfm_object, to = "stm")
    stm_data$documents.dimnames <- rownames(dfm_object)
  }
  
  ## --- Metadata preparation ---------------------------------------------------
  log_message("Preparing metadata for modeling", "fit_model")
  
  # Get metadata from config
  metadata_df <- config$metadata
  
  # If segmentation was used, we need to match metadata to segments
  if (use_segmentation) {
    # Create segment metadata by joining with original metadata
    segment_metadata <- dplyr::left_join(
      segment_mapping,
      metadata_df,
      by = c("original_doc_id" = "doc_id")
    )
    
    # Rename segment_id to doc_id for consistency
    segment_metadata$doc_id <- segment_metadata$segment_id
    segment_metadata$segment_id <- NULL
    
    # Use segment metadata for modeling
    model_metadata <- segment_metadata
  } else {
    # Use original metadata
    model_metadata <- metadata_df
  }
  
  # Ensure metadata order matches dfm order
  doc_ids <- rownames(dfm_object)
  
  # Filter metadata to include only documents in the dfm
  model_metadata <- model_metadata[model_metadata$doc_id %in% doc_ids, ]
  
  # Sort metadata to match dfm order
  model_metadata <- model_metadata[match(doc_ids, model_metadata$doc_id), ]
  
  # Check for any mismatches
  if (any(is.na(match(doc_ids, model_metadata$doc_id)))) {
    warning_msg <- "Some documents don't have matching metadata"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
  }
  
  # Prepare metadata for STM - remove doc_id and convert types
  stm_metadata <- model_metadata[, !names(model_metadata) %in% c("doc_id")]
  
  # Convert logical and character columns to factors
  for (col in names(stm_metadata)) {
    if (is.logical(stm_metadata[[col]])) {
      # Handle NA values in logical columns
      if (any(is.na(stm_metadata[[col]]))) {
        stm_metadata[[col]] <- factor(stm_metadata[[col]], 
                                      levels = c(FALSE, TRUE, NA), 
                                      exclude = NULL)
        levels(stm_metadata[[col]])[3] <- "Missing"
      } else {
        stm_metadata[[col]] <- factor(stm_metadata[[col]], 
                                      levels = c(FALSE, TRUE))
      }
    } else if (is.character(stm_metadata[[col]])) {
      stm_metadata[[col]] <- factor(stm_metadata[[col]])
    }
  }
  
  ## --- Prevalence formula preparation -----------------------------------------
  log_message("Preparing prevalence formula", "fit_model")
  
  # Initialize formula variables
  search_prev_formula <- NULL
  prev_formula <- NULL
  
  # Prepare prevalence formula based on provided value or auto-generate
  if (is.null(modeling_config$prevalence_formula)) {
    # Extract all dimensions from category_map
    all_dimensions <- unlist(config$category_map)
    
    # Check which dimensions are available in metadata
    available_dims <- all_dimensions[all_dimensions %in% names(stm_metadata)]
    
    if (length(available_dims) > 0) {
      # For search phase, just use the first dimension to keep it simple
      search_formula_str <- paste("~", available_dims[1])
      search_prev_formula <- stats::as.formula(search_formula_str)
      log_message(paste("Using simplified formula for searchK:", 
                        search_formula_str), "fit_model")
      
      # For final model, use all dimensions
      final_formula_str <- paste("~", paste(available_dims, collapse = " + "))
      prev_formula <- stats::as.formula(final_formula_str)
      log_message(paste("Will use full formula for final model:", 
                        final_formula_str), "fit_model")
    } else {
      # No usable dimensions, use intercept-only model
      search_prev_formula <- stats::as.formula("~ 1")
      prev_formula <- stats::as.formula("~ 1")
      log_message("No usable dimensions for prevalence formula, using intercept-only", "fit_model")
    }
  } else if (is.character(modeling_config$prevalence_formula)) {
    # Convert string to formula
    tryCatch({
      prev_formula <- stats::as.formula(modeling_config$prevalence_formula)
      search_prev_formula <- prev_formula  # Use same formula for both
      log_message(paste("Using prevalence formula:", modeling_config$prevalence_formula), "fit_model")
    }, error = function(e) {
      warning_msg <- paste("Error parsing prevalence formula:", e$message)
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      prev_formula <- stats::as.formula("~ 1")
      search_prev_formula <- prev_formula
      log_message("Falling back to intercept-only prevalence formula", "fit_model", "WARNING")
    })
  } else if (inherits(modeling_config$prevalence_formula, "formula")) {
    # Use the provided formula
    prev_formula <- modeling_config$prevalence_formula
    search_prev_formula <- prev_formula  # Use same formula for both
    log_message(paste("Using provided prevalence formula:", 
                      deparse(prev_formula)), "fit_model")
  } else {
    # Invalid input, use default
    prev_formula <- stats::as.formula("~ 1")
    search_prev_formula <- prev_formula
    log_message("Invalid prevalence formula, using intercept-only", "fit_model", "WARNING")
  }
  
  ## --- Content covariates preparation -----------------------------------------
  log_message("Preparing content covariates", "fit_model")
  
  # Initialize content covariates
  content_covariates <- NULL
  
  # Process content covariates if provided
  if (!is.null(modeling_config$content_covariates)) {
    # Check which content covariates are available in metadata
    available_content <- modeling_config$content_covariates[
      modeling_config$content_covariates %in% names(stm_metadata)]
    
    if (length(available_content) > 0) {
      # Create a formula for content covariates
      content_formula <- stats::as.formula(
        paste("~", paste(available_content, collapse = " + ")))
      
      # Create design matrix (without intercept)
      content_covariates <- stats::model.matrix(content_formula, data = stm_metadata)[, -1, drop = FALSE]
      
      log_message(paste("Using content covariates:", 
                        paste(available_content, collapse = ", ")), "fit_model")
    } else {
      warning_msg <- "Specified content covariates not available in metadata"
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
    }
  }
  
  ## --- K selection (if not provided) ------------------------------------------
  if (is.null(k)) {
    log_message(paste("Determining optimal number of topics (min =", k_search_config$min, 
                      ", max =", k_search_config$max, 
                      ", step =", k_search_config$step, ")"), "fit_model")
    
    # Generate sequence of k values to test
    k_values <- seq(k_search_config$min, k_search_config$max, by = k_search_config$step)
    log_message(paste("Testing K values:", paste(k_values, collapse = ", ")), "fit_model")
    
    # Run searchK
    k_search <- tryCatch({
      stm::searchK(
        documents = stm_data$documents,
        vocab = stm_data$vocab,
        K = k_values,
        prevalence = search_prev_formula,  # Use simplified formula
        content = NULL,  # Always NULL for searchK to avoid exclusivity errors
        data = stm_metadata,
        max.em.its = round(modeling_config$iterations / 2),
        init.type = "Spectral",
        verbose = FALSE,
        seed = modeling_config$seed
      )
    }, error = function(e) {
      error_msg <- paste("Error in searchK:", e$message)
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
      log_message(error_msg, "fit_model", "ERROR")
      stop(error_msg)
    })
    
    # Extract results
    k_results <- k_search$results
    
    # Store searchK results in diagnostics
    diagnostics$k_selection$k_values <- k_values
    diagnostics$k_selection$raw_results <- k_results
    
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
    combined_score <- 0.35 * semcoh_norm + 
      0.35 * exclus_norm + 
      0.30 * residual_norm - 
      0.05 * normalize(k_results$K)  # Small complexity penalty
    
    # Find best K
    best_idx <- which.max(combined_score)
    best_k <- k_values[best_idx]
    
    log_message(paste("Selected optimal K =", best_k), "fit_model")
    
    # Store selection results in diagnostics
    diagnostics$k_selection$combined_score <- combined_score
    diagnostics$k_selection$best_k <- best_k
    diagnostics$k_selection$best_idx <- best_idx
    
    # Use best k for final model
    k <- best_k
  }
  
  ## --- Fit STM model ----------------------------------------------------------
  log_message(paste("Fitting STM model with k =", k), "fit_model")
  
  # Set seed for reproducibility 
  set.seed(modeling_config$seed)
  
  # Fit model with configured options
  stm_model <- tryCatch({
    stm::stm(
      documents = stm_data$documents,
      vocab = stm_data$vocab,
      K = k,
      prevalence = prev_formula,
      content = content_covariates,
      data = stm_metadata,
      max.em.its = modeling_config$iterations,
      init.type = "Spectral",
      verbose = FALSE,
      gamma.prior = model_weights$gamma.prior,
      sigma.prior = model_weights$sigma.prior,
      kappa = model_weights$kappa,
      seed = modeling_config$seed
    )
  }, error = function(e) {
    error_msg <- paste("Error fitting STM model:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "fit_model", "ERROR")
    stop(error_msg)
  })
  
  ## --- Extract topic information ----------------------------------------------
  log_message("Extracting topic information", "fit_model")
  
  # Extract document-topic proportions
  topic_proportions <- stm_model$theta
  
  # Add document IDs
  topic_props_df <- as.data.frame(topic_proportions)
  topic_props_df$doc_id <- rownames(dfm_object)
  
  # Rename topic columns
  names(topic_props_df)[1:k] <- paste0("Topic_", 1:k)
  
  # Get topic labels and top terms
  topic_terms <- tryCatch({
    labels <- stm::labelTopics(stm_model, n = 10)
    
    # Create data frames for different label types
    prob_labels <- data.frame(
      topic_id = 1:k,
      label_type = "probability",
      words = apply(labels$prob, 1, paste, collapse = ", "),
      stringsAsFactors = FALSE
    )
    
    frex_labels <- data.frame(
      topic_id = 1:k,
      label_type = "frex",
      words = apply(labels$frex, 1, paste, collapse = ", "),
      stringsAsFactors = FALSE
    )
    
    lift_labels <- data.frame(
      topic_id = 1:k,
      label_type = "lift",
      words = apply(labels$lift, 1, paste, collapse = ", "),
      stringsAsFactors = FALSE
    )
    
    score_labels <- data.frame(
      topic_id = 1:k,
      label_type = "score",
      words = apply(labels$score, 1, paste, collapse = ", "),
      stringsAsFactors = FALSE
    )
    
    # Combine all label types
    all_labels <- rbind(prob_labels, frex_labels, lift_labels, score_labels)
    
    # Also create a wide format for easier use
    label_summary <- data.frame(
      topic_id = 1:k,
      prob_words = prob_labels$words,
      frex_words = frex_labels$words,
      lift_words = lift_labels$words,
      score_words = score_labels$words,
      stringsAsFactors = FALSE
    )
    
    list(
      all_labels = all_labels,
      label_summary = label_summary
    )
  }, error = function(e) {
    warning_msg <- paste("Error extracting topic labels:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    
    # Return empty label data frames
    list(
      all_labels = data.frame(
        topic_id = integer(0), 
        label_type = character(0), 
        words = character(0),
        stringsAsFactors = FALSE
      ),
      label_summary = data.frame(
        topic_id = 1:k,
        stringsAsFactors = FALSE
      )
    )
  })
  
  ## --- Aggregate results (if segmentation used) -------------------------------
  if (use_segmentation) {
    log_message("Aggregating segment-level results to document level", "fit_model")
    
    # Merge topic proportions with segment mapping
    segment_props <- dplyr::left_join(
      topic_props_df,
      segment_mapping,
      by = c("doc_id" = "segment_id")
    )
    
    # Get topic columns
    topic_cols <- paste0("Topic_", 1:k)
    
    # Aggregate to document level using weighted average
    doc_topic_props <- segment_props %>%
      dplyr::group_by(original_doc_id) %>%
      dplyr::summarize(
        across(all_of(topic_cols), ~ weighted.mean(., token_count, na.rm = TRUE)),
        .groups = "drop"
      )
    
    # Rename for consistency
    names(doc_topic_props)[names(doc_topic_props) == "original_doc_id"] <- "doc_id"
    
    # Join with original metadata (pre-segmentation)
    doc_topic_props <- dplyr::left_join(
      doc_topic_props,
      metadata_df,
      by = "doc_id"
    )
  } else {
    # No segmentation - just join with metadata
    doc_topic_props <- dplyr::left_join(
      topic_props_df,
      metadata_df,
      by = "doc_id"
    )
  }
  
  # Create long format for easier analysis
  topic_data <- doc_topic_props %>%
    tidyr::pivot_longer(
      cols = starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  ## --- Calculate model quality metrics ----------------------------------------
  log_message("Calculating model quality metrics", "fit_model")
  
  # Calculate semantic coherence
  coherence <- tryCatch({
    mean(stm::semanticCoherence(stm_model, stm_data$documents))
  }, error = function(e) {
    warning_msg <- paste("Error calculating coherence:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    NA
  })
  
  # Calculate exclusivity
  exclusivity <- tryCatch({
    mean(stm::exclusivity(stm_model))
  }, error = function(e) {
    warning_msg <- paste("Error calculating exclusivity:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "fit_model", "WARNING")
    NA
  })
  
  # Calculate held-out likelihood if available
  heldout <- tryCatch({
    if (!is.null(stm_model$heldout)) {
      stm_model$heldout$bound
    } else {
      NA
    }
  }, error = function(e) {
    NA
  })
  
  # Store quality metrics
  model_quality <- list(
    coherence = coherence,
    exclusivity = exclusivity,
    heldout = heldout,
    iterations = stm_model$convergence$its,
    converged = stm_model$convergence$converged
  )
  
  diagnostics$model_quality <- model_quality
  
  ## --- Run effects analysis ---------------------------------------------------
  log_message("Running effects analysis", "fit_model")
  
  # Initialize lists for effects and variance explained
  effects_list <- list()
  variance_explained <- list()
  
  # Determine dimensions to analyze from category_map
  all_dimensions <- unlist(config$category_map)
  available_dims <- all_dimensions[all_dimensions %in% names(doc_topic_props)]
  
  # Helper function to calculate variance explained
  calculate_variance <- function(effects_obj, topic_count) {
    total_variance <- 0
    
    # Loop through topics
    for (i in 1:topic_count) {
      # Get summary for this topic
      s <- summary(effects_obj, topics = i)
      
      # Add this topic's explained variance
      if (!is.null(s$r2)) {
        total_variance <- total_variance + s$r2
      }
    }
    
    # Return average across topics
    return(total_variance / topic_count)
  }
  
  # Process each dimension
  for (dim in available_dims) {
    log_message(paste("Analyzing effects of", dim), "fit_model")
    
    # Skip if dimension has insufficient variation
    if (is.factor(doc_topic_props[[dim]]) && length(levels(doc_topic_props[[dim]])) < 2) {
      warning_msg <- paste("Dimension", dim, "has insufficient variation for effects analysis")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      next
    }
    
    if (is.logical(doc_topic_props[[dim]]) && length(unique(doc_topic_props[[dim]])) < 2) {
      warning_msg <- paste("Dimension", dim, "has insufficient variation for effects analysis")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      next
    }
    
    # Create formula for this dimension
    formula <- stats::as.formula(paste("~", dim))
    
    # Try to estimate effects
    effects_result <- tryCatch({
      # Estimate effects for all topics
      effects <- stm::estimateEffect(
        formula, 
        stmobj = stm_model, 
        metadata = doc_topic_props,
        uncertainty = 20  # Default value
      )
      
      # Calculate variance explained
      var_explained <- calculate_variance(effects, k)
      
      list(
        effects = effects,
        variance = var_explained
      )
    }, error = function(e) {
      warning_msg <- paste("Error estimating effects for", dim, ":", e$message)
      diagnostics$processing_issues <<- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "fit_model", "WARNING")
      NULL
    })
    
    # Store results if successful
    if (!is.null(effects_result)) {
      effects_list[[dim]] <- effects_result$effects
      variance_explained[[dim]] <- effects_result$variance
      
      log_message(paste(dim, "explains approximately", 
                        round(effects_result$variance * 100, 1), "% of topic variance"), 
                  "fit_model")
    }
  }
  
  # Store effect analysis summary in diagnostics
  diagnostics$effects_analysis$dimensions <- names(effects_list)
  diagnostics$effects_analysis$variance_explained <- variance_explained
  
  ## --- Create and format final results ----------------------------------------
  log_message("Preparing final results", "fit_model")
  
  # Create result data
  result_data <- list(
    # Include model only if requested
    model = if(modeling_config$include_model) stm_model else NULL,
    
    # Document-topic proportions (wide format)
    topic_proportions = doc_topic_props,
    
    # Topic data (long format with metadata)
    topic_data = topic_data,
    
    # Topic terms
    topic_terms = topic_terms,
    
    # Topic correlations
    topic_correlations = stm_model$beta$cor,
    
    # Segmentation information (if used)
    segments = if(use_segmentation) list(
      mapping = segment_mapping,
      segment_size = modeling_config$segment
    ) else NULL,
    
    # Effects analysis results
    effects = effects_list,
    
    # Variance explained by each dimension
    variance_explained = variance_explained
  )
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k = k,
    auto_k = is.null(k),
    prevalence_formula = deparse(prev_formula),
    content_covariates = if(!is.null(modeling_config$content_covariates)) 
      modeling_config$content_covariates else NULL,
    segmentation_used = use_segmentation,
    segment_size = if(use_segmentation) modeling_config$segment else NA,
    iterations = modeling_config$iterations,
    documents = if(use_segmentation) 
      length(unique(segment_mapping$original_doc_id)) 
    else nrow(topic_proportions),
    segments = if(use_segmentation) nrow(segment_mapping) else NA,
    seed = modeling_config$seed,
    model_included = modeling_config$include_model
  )
  
  # Add diagnostics summary 
  diagnostics$summary <- list(
    model_converged = model_quality$converged,
    coherence = model_quality$coherence,
    exclusivity = model_quality$exclusivity,
    dimensions_analyzed = length(effects_list),
    issues_count = length(diagnostics$processing_issues)
  )
  
  log_message(sprintf("Model fitting complete: k = %d, coherence = %.3f, exclusivity = %.3f", 
                      k, coherence, exclusivity), 
              "fit_model")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}