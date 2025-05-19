#' @title Prepare data for Structural Topic Modeling
#' @description Joins token data and metadata, prepares document-term matrices, handles
#'   segmentation, and creates the format required by the STM package. This function
#'   combines outputs from both text and metadata processing paths.
#'
#' @param tokens_result Result from validate_tokens() containing tokens and vocabulary
#' @param metadata_result Result from add_metadata() containing metadata and configuration
#' @param segment_size Number of tokens per segment (default: 0, no segmentation)
#' @param stm_config Additional STM preparation options:
#'   \itemize{
#'     \item handle_missing - How to handle documents missing from either path (default: "warn")
#'     \item keep_tokens - Whether to include the token dataframe in output (default: FALSE)
#'     \item preserve_vars - Variables to preserve in metadata (default: NULL, keep all)
#'   }
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item stm_input - STM input format required for modeling
#'       \item metadata - Metadata dataframe ready for STM with matched document IDs
#'       \item segments - Segment mapping information (if segmentation used)
#'       \item vocab - Vocabulary list
#'       \item config - Configuration options and category mappings
#'     }
#'   }
#'   \item{metadata}{Processing information and statistics}
#'   \item{diagnostics}{Join statistics and processing issues}
#'
#' @examples
#' \dontrun{
#' # Process both data paths
#' web_data <- scrape_web()
#' text_data <- extract_pdfs(web_data$data$tokens)
#' tokens <- validate_tokens(text_data$data$tokens)
#' metadata <- add_metadata(web_data$data$metadata)
#' 
#' # Join paths and prepare for STM
#' stm_data <- prepare_stm(tokens, metadata)
#' 
#' # Use segmentation
#' stm_data_segmented <- prepare_stm(tokens, metadata, segment_size = 500)
#' }
prepare_stm <- function(
    tokens_result,
    metadata_result,
    segment_size = 0,
    stm_config = list(
      handle_missing = "warn",  # Options: "warn", "drop", "error"
      keep_tokens = FALSE,      # Whether to include tokens in output
      preserve_vars = NULL      # Variables to preserve in metadata (NULL = all)
    )
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    join_stats = list(),
    segmentation = list(),
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "prepare_stm")
  
  # Validate tokens_result
  if (!is.list(tokens_result) || 
      !"data" %in% names(tokens_result) ||
      !all(c("tokens", "vocab") %in% names(tokens_result$data))) {
    error_msg <- "tokens_result must be the output from validate_tokens() with tokens and vocab"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prepare_stm", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate metadata_result
  if (!is.list(metadata_result) || 
      !"data" %in% names(metadata_result) ||
      !all(c("metadata", "config") %in% names(metadata_result$data))) {
    error_msg <- "metadata_result must be the output from add_metadata() with metadata and config"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prepare_stm", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate stm_config
  if (!is.list(stm_config)) {
    warning_msg <- "stm_config must be a list, using defaults"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "prepare_stm", "WARNING")
    stm_config <- list(handle_missing = "warn", keep_tokens = FALSE, preserve_vars = NULL)
  }
  
  # Set defaults for any missing config options
  stm_config$handle_missing <- stm_config$handle_missing %||% "warn"
  stm_config$keep_tokens <- stm_config$keep_tokens %||% FALSE
  stm_config$preserve_vars <- stm_config$preserve_vars %||% NULL
  
  # Validate segment_size
  if (!is.numeric(segment_size) || segment_size < 0) {
    warning_msg <- "segment_size must be a non-negative number, using 0 (no segmentation)"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "prepare_stm", "WARNING")
    segment_size <- 0
  }
  
  # Extract data
  tokens_df <- tokens_result$data$tokens
  vocab <- tokens_result$data$vocab
  metadata_df <- metadata_result$data$metadata
  config <- metadata_result$data$config
  
  ## --- Join document sets -----------------------------------------------------
  log_message("Matching document sets from tokens and metadata", "prepare_stm")
  
  # Get document IDs from both paths
  token_doc_ids <- unique(tokens_df$doc_id)
  metadata_doc_ids <- metadata_df$doc_id
  
  log_message(paste("Found", length(token_doc_ids), "documents in tokens and", 
                    length(metadata_doc_ids), "documents in metadata"), "prepare_stm")
  
  # Check for missing documents
  missing_in_tokens <- setdiff(metadata_doc_ids, token_doc_ids)
  missing_in_metadata <- setdiff(token_doc_ids, metadata_doc_ids)
  
  # Track join statistics
  diagnostics$join_stats$token_docs <- length(token_doc_ids)
  diagnostics$join_stats$metadata_docs <- length(metadata_doc_ids)
  diagnostics$join_stats$missing_in_tokens <- length(missing_in_tokens)
  diagnostics$join_stats$missing_in_metadata <- length(missing_in_metadata)
  diagnostics$join_stats$common_docs <- length(intersect(token_doc_ids, metadata_doc_ids))
  
  # Handle missing documents based on config
  if (length(missing_in_tokens) > 0 || length(missing_in_metadata) > 0) {
    if (stm_config$handle_missing == "error") {
      error_msg <- paste(
        "Document ID mismatch:", 
        length(missing_in_tokens), "documents missing in tokens,",
        length(missing_in_metadata), "documents missing in metadata"
      )
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
      log_message(error_msg, "prepare_stm", "ERROR")
      
      return(create_result(
        data = NULL,
        metadata = list(
          timestamp = Sys.time(),
          success = FALSE
        ),
        diagnostics = diagnostics
      ))
    } else if (stm_config$handle_missing == "warn") {
      warning_msg <- paste(
        "Document ID mismatch:", 
        length(missing_in_tokens), "documents missing in tokens,",
        length(missing_in_metadata), "documents missing in metadata.",
        "Using intersection of document sets."
      )
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "prepare_stm", "WARNING")
    }
    
    # Filter to common document IDs
    common_doc_ids <- intersect(token_doc_ids, metadata_doc_ids)
    tokens_df <- tokens_df %>% dplyr::filter(doc_id %in% common_doc_ids)
    metadata_df <- metadata_df %>% dplyr::filter(doc_id %in% common_doc_ids)
    
    log_message(paste("Using", length(common_doc_ids), "documents common to both paths"), 
                "prepare_stm")
  }
  
  ## --- Segmentation (if requested) --------------------------------------------
  use_segmentation <- segment_size > 0
  segment_mapping <- NULL
  tokens_segmented <- NULL
  
  if (use_segmentation) {
    log_message(paste("Segmenting documents into chunks of", segment_size, "tokens"), 
                "prepare_stm")
    
    # Initialize segment storage
    tokens_segmented <- tibble::tibble(
      doc_id = character(),
      segment_id = character(),
      word = character()
    )
    
    segment_mapping <- tibble::tibble(
      segment_id = character(),
      original_doc_id = character(),
      segment_num = integer(),
      token_count = integer()
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
        log_message(paste("Skipping empty document:", doc_id), "prepare_stm", "WARNING")
        next
      }
      
      # Calculate number of segments needed
      n_segments <- ceiling(total_tokens / segment_size)
      
      # If document is smaller than segmentation threshold, keep as one segment
      if (total_tokens <= segment_size) {
        segment_id <- paste0(doc_id, "_1")
        
        # Create segment with original tokens
        doc_tokens$segment_id <- segment_id
        tokens_segmented <- dplyr::bind_rows(tokens_segmented, doc_tokens)
        
        # Add to segment mapping
        segment_mapping <- dplyr::bind_rows(
          segment_mapping,
          tibble::tibble(
            segment_id = segment_id,
            original_doc_id = doc_id,
            segment_num = 1,
            token_count = total_tokens
          )
        )
      } else {
        # Split into segments of specified size
        for (j in 1:n_segments) {
          start_idx <- (j - 1) * segment_size + 1
          end_idx <- min(j * segment_size, total_tokens)
          
          if (start_idx <= total_tokens) {
            # Create segment ID
            segment_id <- paste0(doc_id, "_", j)
            
            # Extract tokens for this segment
            segment_tokens <- doc_tokens[start_idx:end_idx, ]
            segment_tokens$segment_id <- segment_id
            tokens_segmented <- dplyr::bind_rows(tokens_segmented, segment_tokens)
            
            # Add to segment mapping
            segment_mapping <- dplyr::bind_rows(
              segment_mapping,
              tibble::tibble(
                segment_id = segment_id,
                original_doc_id = doc_id,
                segment_num = j,
                token_count = end_idx - start_idx + 1
              )
            )
          }
        }
      }
    }
    
    # Use segmented tokens for further processing
    tokens_df <- tokens_segmented
    
    # Update document ID column to use segment IDs
    tokens_df$doc_id <- tokens_df$segment_id
    
    # Store segmentation info in diagnostics
    diagnostics$segmentation$segment_size <- segment_size
    diagnostics$segmentation$original_docs <- length(doc_ids)
    diagnostics$segmentation$segment_count <- nrow(segment_mapping)
    
    log_message(paste("Created", nrow(segment_mapping), "segments from", 
                      length(doc_ids), "documents"), "prepare_stm")
  }
  
  ## --- Create document-term matrix --------------------------------------------
  log_message("Creating document-term matrix", "prepare_stm")
  
  # Count word frequencies
  word_counts <- tokens_df %>%
    dplyr::count(doc_id, word)
  
  # Create document-feature matrix using tidytext
  dfm_object <- tryCatch({
    word_counts %>%
      tidytext::cast_dfm(doc_id, word, n)
  }, error = function(e) {
    error_msg <- paste("DFM creation error:", e$message)
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prepare_stm", "ERROR")
    stop(error_msg)
  })
  
  ## --- Convert to STM format with proper matrix structure ---------------------
  log_message("Converting to STM format", "prepare_stm")
  stm_input <- tryCatch({
    # Convert DFM to STM format
    stm_result <- quanteda::convert(dfm_object, to = "stm")
    
    # Set document names to match IDs
    stm_result$documents.dimnames <- rownames(dfm_object)
    
    # IMPORTANT: Validate and ensure proper matrix format for each document
    log_message("Validating STM document format", "prepare_stm")
    for (i in seq_along(stm_result$documents)) {
      # Get the document
      doc <- stm_result$documents[[i]]
      
      # Handle different document formats
      if (is.matrix(doc) && ncol(doc) == 2) {
        # Already in correct matrix format, just ensure column names
        colnames(doc) <- c("indices", "counts")
        stm_result$documents[[i]] <- doc
      } else if (is.list(doc) && all(c("indices", "counts") %in% names(doc))) {
        # Convert list format to matrix format
        log_message(paste("Converting list document", i, "to matrix format"), "prepare_stm")
        mat <- cbind(doc$indices, doc$counts)
        colnames(mat) <- c("indices", "counts")
        stm_result$documents[[i]] <- mat
      } else if (is.atomic(doc)) {
        # Handle atomic vector format (single vector of indices)
        log_message(paste("Converting atomic vector document", i, "to matrix format"), "prepare_stm")
        
        # Count occurrences of each index
        if (length(doc) > 0) {
          index_table <- table(doc)
          indices <- as.integer(names(index_table))
          counts <- as.integer(index_table)
          
          # Create matrix with indices and counts
          mat <- cbind(indices, counts)
          colnames(mat) <- c("indices", "counts")
          stm_result$documents[[i]] <- mat
        } else {
          # Empty document
          stm_result$documents[[i]] <- matrix(integer(0), ncol = 2)
          colnames(stm_result$documents[[i]]) <- c("indices", "counts")
        }
      } else {
        # Fallback for undefined format - create empty document
        log_message(paste("Unknown document format for document", i, "- creating empty document"), 
                    "prepare_stm", "WARNING")
        stm_result$documents[[i]] <- matrix(integer(0), ncol = 2)
        colnames(stm_result$documents[[i]]) <- c("indices", "counts")
      }
      
      # Verify indices are within vocabulary bounds
      if (nrow(stm_result$documents[[i]]) > 0) {
        vocab_length <- length(stm_result$vocab)
        invalid_indices <- stm_result$documents[[i]][, 1] > vocab_length
        
        if (any(invalid_indices)) {
          log_message(paste("Removing", sum(invalid_indices), "invalid indices in document", i), 
                      "prepare_stm", "WARNING")
          stm_result$documents[[i]] <- stm_result$documents[[i]][!invalid_indices, , drop = FALSE]
        }
        
        # Ensure there are still indices after filtering
        if (nrow(stm_result$documents[[i]]) == 0) {
          log_message(paste("Document", i, "has no valid indices left after filtering"), 
                      "prepare_stm", "WARNING")
        }
      }
    }
    
    # Return the properly formatted STM input
    stm_result
  }, error = function(e) {
    error_msg <- paste("STM conversion error:", e$message)
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prepare_stm", "ERROR")
    stop(error_msg)
  })
  
  # Additional validation - check for empty documents
  empty_docs <- sapply(stm_input$documents, function(d) nrow(d) == 0)
  if (any(empty_docs)) {
    log_message(paste(sum(empty_docs), "documents are empty after conversion"), 
                "prepare_stm", "WARNING")
  }
  
  log_message(paste("Successfully converted", length(stm_input$documents), 
                    "documents to STM format"), "prepare_stm")
  
  ## --- Prepare metadata for STM -----------------------------------------------
  log_message("Preparing metadata for STM", "prepare_stm")
  
  # Get the document IDs from the STM input
  doc_ids <- rownames(dfm_object)
  
  # Start with original metadata
  model_metadata <- metadata_df
  
  # Check metadata coverage
  metadata_coverage <- sum(doc_ids %in% model_metadata$doc_id) / length(doc_ids) * 100
  log_message(paste("Metadata coverage:", round(metadata_coverage, 1), "%"), "prepare_stm")
  
  # Filter metadata to include only documents in the dfm
  model_metadata <- model_metadata[model_metadata$doc_id %in% doc_ids, ]
  
  # Check for documents missing from metadata after filtering
  missing_docs <- setdiff(doc_ids, model_metadata$doc_id)
  if (length(missing_docs) > 0) {
    warning_msg <- paste(length(missing_docs), "documents don't have matching metadata")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "prepare_stm", "WARNING")
    
    # If too many documents are missing metadata, create placeholder metadata
    if (length(missing_docs) > 0.1 * length(doc_ids)) {
      log_message("Creating placeholder metadata for missing documents", "prepare_stm", "WARNING")
      
      # Create placeholder entries
      placeholder_metadata <- data.frame(
        doc_id = missing_docs,
        stringsAsFactors = FALSE
      )
      
      # Add placeholder columns matching model_metadata
      for (col in setdiff(names(model_metadata), "doc_id")) {
        placeholder_metadata[[col]] <- NA
      }
      
      # Combine with existing metadata
      model_metadata <- rbind(model_metadata, placeholder_metadata)
    }
  }
  
  # Sort metadata to match document order in dfm
  model_metadata <- model_metadata[match(doc_ids, model_metadata$doc_id), ]
  
  # Now check for NA values that might cause problems
  if (nrow(model_metadata) > 0) {
    log_message("Checking for NA values in metadata", "prepare_stm")
    na_counts <- colSums(is.na(model_metadata))
    
    # Identify columns with high NA percentages
    high_na_cols <- names(na_counts[na_counts > 0.1 * nrow(model_metadata)])
    
    if (length(high_na_cols) > 0) {
      warning_msg <- paste("High NA percentage in metadata columns:", 
                           paste(high_na_cols, collapse = ", "))
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "prepare_stm", "WARNING")
      
      # For categorical variables, replace NAs with "Unknown" category
      for (col in high_na_cols) {
        if (is.character(model_metadata[[col]]) || is.factor(model_metadata[[col]])) {
          model_metadata[[col]][is.na(model_metadata[[col]])] <- "Unknown"
          log_message(paste("Replaced NAs in", col, "with 'Unknown' category"), "prepare_stm")
        } else if (is.numeric(model_metadata[[col]])) {
          # For numeric variables, consider replacing with mean or median
          if (sum(!is.na(model_metadata[[col]])) > 0) {
            replacement_value <- median(model_metadata[[col]], na.rm = TRUE)
            model_metadata[[col]][is.na(model_metadata[[col]])] <- replacement_value
            log_message(paste("Replaced NAs in", col, "with median value"), "prepare_stm")
          }
        }
      }
    }
  }
  
  # Verify metadata has right dimensions for STM
  if (nrow(model_metadata) != length(doc_ids)) {
    error_msg <- paste("Metadata row count (", nrow(model_metadata), 
                       ") doesn't match document count (", length(doc_ids), ")")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prepare_stm", "ERROR")
    
    # Force alignment as last resort
    model_metadata <- model_metadata[1:min(nrow(model_metadata), length(doc_ids)), ]
    if (nrow(model_metadata) < length(doc_ids)) {
      # Pad with empty rows if needed
      empty_rows <- data.frame(doc_id = doc_ids[(nrow(model_metadata) + 1):length(doc_ids)])
      for (col in setdiff(names(model_metadata), "doc_id")) {
        empty_rows[[col]] <- NA
      }
      model_metadata <- rbind(model_metadata, empty_rows)
    }
  }
  
  log_message("Metadata preparation complete", "prepare_stm")
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    stm_input = stm_input,
    metadata = model_metadata,
    segments = if(exists("segment_mapping")) segment_mapping else NULL,
    vocab = vocab,
    config = config
  )
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    document_count = length(doc_ids),
    segment_count = if(exists("segment_mapping")) nrow(segment_mapping) else NA,
    vocabulary_size = length(vocab),
    segmentation_used = exists("segment_mapping"),
    segment_size = if(exists("segment_size")) segment_size else 0,
    metadata_vars = names(model_metadata),
    success = TRUE
  )
  
  # Update diagnostics
  if (exists("join_stats")) {
    diagnostics$join_stats$final_docs <- length(doc_ids)
    diagnostics$join_stats$success_rate <- round(
      length(doc_ids) / max(length(token_doc_ids), length(metadata_doc_ids)) * 100, 1
    )
  }
  
  log_message(paste("STM preparation complete:", length(doc_ids), "documents,",
                    length(vocab), "vocabulary terms"), "prepare_stm")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}