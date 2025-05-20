#' @title Standardized Text Processor for STM
#' @description Processes text data from tokens_result and joins with metadata in preparation
#'   for Structural Topic Modeling. Ensures output is in standardized format.
#'   
#' @param tokens_result Result from validate_tokens() containing doc_id and text
#' @param metadata_result Result from add_metadata() containing metadata
#' @param removestopwords Whether to remove common words (default: TRUE)
#' @param stem Whether to apply stemming to words (default: TRUE)
#' @param verbose Whether to display detailed output (default: FALSE)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item processed - Standardized output with documents, vocab, and meta
#'       \item config - Original configuration
#'     }
#'   }
#'   \item{metadata}{Processing information and statistics}
#'   \item{diagnostics}{Processing details and issues encountered}
standardize_text_processor <- function(
    tokens_result,
    metadata_result,
    removestopwords = TRUE,
    stem = TRUE,
    verbose = FALSE
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "standardize_text_processor")
  
  # Validate tokens_result
  if (!is.list(tokens_result) || 
      !"data" %in% names(tokens_result) ||
      !"documents" %in% names(tokens_result$data)) {
    error_msg <- "tokens_result must be the output from validate_tokens() with documents"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "standardize_text_processor", "ERROR")
    
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
      !"metadata" %in% names(metadata_result$data)) {
    error_msg <- "metadata_result must be the output from add_metadata() with metadata"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "standardize_text_processor", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Extract text and metadata -----------------------------------------
  log_message("Extracting text and metadata", "standardize_text_processor")
  
  # Extract document text data
  tokens_df <- tokens_result$data$documents
  
  # Extract metadata
  metadata_df <- metadata_result$data$metadata
  
  ## --- Join datasets for processing --------------------------------------
  log_message("Joining tokens with metadata", "standardize_text_processor")
  
  # Check for doc_id column in both dataframes
  if (!"doc_id" %in% names(tokens_df) || !"doc_id" %in% names(metadata_df)) {
    error_msg <- "Both tokens and metadata must contain a doc_id column for joining"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "standardize_text_processor", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Ensure documents exist in both datasets
  matched_docs <- dplyr::inner_join(tokens_df, metadata_df, by = "doc_id")
  
  # Check if we have any documents after joining
  if (nrow(matched_docs) == 0) {
    error_msg <- "No documents remain after joining tokens with metadata"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "standardize_text_processor", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  log_message(paste("Successfully matched", nrow(matched_docs), "documents"), "standardize_text_processor")
  
  ## --- Process text with textProcessor ------------------------------------------
  log_message("Processing text with STM's textProcessor", "standardize_text_processor")
  
  # Get column containing document text - default to "text"
  text_col <- "text"
  if (!(text_col %in% names(matched_docs))) {
    # Try to find alternative text column
    possible_text_cols <- c("text", "content", "document", "doc_text")
    found_cols <- intersect(possible_text_cols, names(matched_docs))
    
    if (length(found_cols) > 0) {
      text_col <- found_cols[1]
    } else {
      error_msg <- paste("No text column found in matched documents. Available columns:",
                         paste(names(matched_docs), collapse=", "))
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
      log_message(error_msg, "standardize_text_processor", "ERROR")
      
      return(create_result(
        data = NULL,
        metadata = list(
          timestamp = Sys.time(),
          success = FALSE
        ),
        diagnostics = diagnostics
      ))
    }
  }
  
  # Process text using STM's processor
  processed <- tryCatch({
    stm::textProcessor(
      documents = matched_docs[[text_col]],
      metadata = matched_docs,
      lowercase = TRUE,         
      removestopwords = removestopwords,
      removenumbers = TRUE,     
      removepunctuation = TRUE, 
      stem = stem,
      wordLengths = c(3, Inf),   
      verbose = verbose
    )
  }, error = function(e) {
    error_msg <- paste("Error in STM textProcessor:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "standardize_text_processor", "ERROR")
    NULL
  })
  
  # Check if processing was successful
  if (is.null(processed)) {
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Standardize document format -------------------------------------------
  log_message("Standardizing document format", "standardize_text_processor")
  
  standardized_docs <- list()
  format_counts <- list(
    list_format = 0,
    vector_format = 0,
    matrix_format = 0,
    empty_documents = 0
  )
  
  for (i in seq_along(processed$documents)) {
    doc <- processed$documents[[i]]
    
    # Convert to standard 2-column matrix format
    if (is.list(doc) && all(c("indices", "counts") %in% names(doc))) {
      # Convert list format to matrix
      doc_matrix <- cbind(doc$indices, doc$counts)
      format_counts$list_format <- format_counts$list_format + 1
      
    } else if (is.atomic(doc) && length(doc) > 0) {
      # Convert vector of indices to matrix with counts
      doc_table <- table(doc)
      doc_matrix <- cbind(
        as.numeric(names(doc_table)), 
        as.numeric(doc_table)
      )
      format_counts$vector_format <- format_counts$vector_format + 1
      
    } else if (is.matrix(doc) && ncol(doc) >= 2) {
      # Already a matrix, keep first two columns
      doc_matrix <- as.matrix(doc[, 1:2, drop = FALSE])
      format_counts$matrix_format <- format_counts$matrix_format + 1
      
    } else {
      # Create empty document for unrecognized formats
      doc_matrix <- matrix(integer(0), ncol = 2)
      format_counts$empty_documents <- format_counts$empty_documents + 1
      
      if (!is.null(doc)) {
        warning_msg <- paste("Document", i, "has unrecognized format:", class(doc)[1])
        diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
        log_message(warning_msg, "standardize_text_processor", "WARNING")
      }
    }
    
    # Set column names
    colnames(doc_matrix) <- c("indices", "counts")
    standardized_docs[[i]] <- doc_matrix
  }
  
  # Preserve document names if present
  if (!is.null(names(processed$documents))) {
    names(standardized_docs) <- names(processed$documents)
  }
  
  # Store format statistics in diagnostics
  diagnostics$format_stats <- format_counts
  
  ## --- Create standardized result ---------------------------------------------
  standardized_result <- list(
    documents = standardized_docs,
    vocab = processed$vocab,
    meta = processed$meta
  )
  
  ## --- Extract config if available -------------------------------------------
  config <- NULL
  if ("config" %in% names(metadata_result$data)) {
    config <- metadata_result$data$config
  }
  
  ## --- Calculate processing time and create result ---------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_message(paste("Text processing complete:", length(standardized_docs), "documents,", 
                    length(processed$vocab), "vocabulary terms"), "standardize_text_processor")
  
  # Return in standard format
  return(create_result(
    data = list(
      processed = standardized_result,
      config = config
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      document_count = length(standardized_docs),
      vocabulary_size = length(processed$vocab),
      stopwords_removed = removestopwords,
      stemming_applied = stem,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}