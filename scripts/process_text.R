#' @title Process text for Structural Topic Modeling
#' @description Processes text data from tokens and joins with metadata in preparation
#'   for Structural Topic Modeling. Uses STM's textProcessor with minimal parameters.
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
#'       \item processed - Output from STM's textProcessor
#'       \item config - Original configuration
#'     }
#'   }
#'   \item{metadata}{Processing information and statistics}
#'   \item{diagnostics}{Processing details and issues encountered}
#'
process_text <- function(
    tokens_result,
    metadata_result,
    removestopwords = TRUE,
    stem = TRUE,
    verbose = FALSE
) {
  ## --- Setup & Initialization -------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------
  log_message("Validating input data", "process_text")
  
  # Validate tokens_result
  if (!is.list(tokens_result) || 
      !"data" %in% names(tokens_result) ||
      !"documents" %in% names(tokens_result$data)) {
    error_msg <- "tokens_result must be the output from validate_tokens() with documents"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "process_text", "ERROR")
    
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
    log_message(error_msg, "process_text", "ERROR")
    
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
  log_message("Extracting text and metadata", "process_text")
  
  # Extract document text data
  tokens_df <- tokens_result$data$documents
  
  # Extract metadata
  metadata_df <- metadata_result$data$metadata
  
  ## --- Join datasets for processing --------------------------------------
  log_message("Joining tokens with metadata", "process_text")
  
  # Check for doc_id column in both dataframes
  if (!"doc_id" %in% names(tokens_df) || !"doc_id" %in% names(metadata_df)) {
    error_msg <- "Both tokens and metadata must contain a doc_id column for joining"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "process_text", "ERROR")
    
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
    log_message(error_msg, "process_text", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  log_message(paste("Successfully matched", nrow(matched_docs), "documents"), "process_text")
  
  ## --- Use STM's textProcessor ------------------------------------------
  log_message("Processing text with STM's textProcessor", "process_text")
  
  # Process text using STM's processor
  processed <- tryCatch({
    stm::textProcessor(
      documents = matched_docs$text,
      metadata = matched_docs,
      lowercase = FALSE,         # Already done in validate_tokens
      removestopwords = removestopwords,
      removenumbers = FALSE,     # Already done in validate_tokens
      removepunctuation = FALSE, # Already done in validate_tokens
      stem = stem,
      wordLengths = c(3, Inf),   # Default setting
      verbose = verbose
    )
  }, error = function(e) {
    error_msg <- paste("Error in STM textProcessor:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "process_text", "ERROR")
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
  
  ## --- Calculate processing time and create result -----------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_message(paste("Text processing complete:", length(processed$documents), "documents,", 
                    length(processed$vocab), "vocabulary terms"), "process_text")
  
  # Return in standard format
  return(create_result(
    data = list(
      processed = processed,
      config = metadata_result$data$config
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      document_count = length(processed$documents),
      vocabulary_size = length(processed$vocab),
      stopwords_removed = removestopwords,
      stemming_applied = stem,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}