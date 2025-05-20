#' @title Process Documents for Structural Topic Modeling
#' @description Creates a document-feature matrix (DFM) from text data using the STM package's 
#'   standard workflow. Combines text processing and document preparation into a single function 
#'   that stays close to the STM package's native capabilities.
#'   
#' @param tokens Result from validate_tokens() containing document text data
#' @param metadata Result from add_metadata() containing document metadata
#' @param remove_stopwords Whether to remove stopwords (default: TRUE)
#' @param stem Whether to stem words (default: TRUE)
#' @param min_docs Minimum number of documents a term must appear in (default: 2)
#' @param exclusivity_docs Number of documents a term can be absent from before being filtered (default: 1)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item documents - Document list in STM format
#'       \item vocab - Character vector of vocabulary
#'       \item meta - Data frame of metadata aligned with documents
#'     }
#'   }
#'   \item{metadata}{Processing information and statistics}
#'   \item{diagnostics}{Processing details and issues encountered}
#'
#' @examples
#' \dontrun{
#' # Get tokens and metadata
#' tokens <- auto_cache(extract_pdfs, web$data$tokens)
#' metadata <- auto_cache(add_metadata, web$data$metadata)
#' 
#' # Process data
#' dfm <- auto_cache(process_dfm, tokens, metadata)
#' 
#' # Process with custom thresholds
#' dfm <- auto_cache(process_dfm, tokens, metadata, min_docs = 3, exclusivity_docs = 2)
#' }
process_dfm <- function(
    tokens,
    metadata,
    remove_stopwords = TRUE,
    stem = TRUE,
    min_docs = 2,            # Minimum docs for a term to be kept
    exclusivity_docs = 1     # How many docs a term can be absent from
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    text_stats = list(),
    doc_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "process_dfm")
  
  # Validate tokens structure
  if (!is.list(tokens) || 
      !"data" %in% names(tokens) ||
      !"documents" %in% names(tokens$data)) {
    error_msg <- "tokens must be the output from validate_tokens() with a documents component"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "process_dfm", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate metadata structure
  if (!is.list(metadata) || 
      !"data" %in% names(metadata) ||
      !"metadata" %in% names(metadata$data)) {
    error_msg <- "metadata must be the output from add_metadata() with a metadata component"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "process_dfm", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Extract text and metadata ----------------------------------------------
  log_message("Extracting text and metadata", "process_dfm")
  
  # Extract document text data
  text_data <- tokens$data$documents$text
  
  # Extract metadata
  meta_data <- metadata$data$metadata
  
  # Store document count for upper threshold calculation
  doc_count <- length(text_data)
  
  # Calculate upper threshold (total docs minus exclusivity_docs)
  # This ensures words appearing in all but 'exclusivity_docs' documents are removed
  upper_thresh <- max(doc_count - exclusivity_docs, min_docs + 1)  # Ensure upper > lower
  
  # Validate text and metadata alignment
  if (length(text_data) != nrow(meta_data)) {
    warning_msg <- paste("Text data length (", length(text_data), 
                         ") doesn't match metadata rows (", nrow(meta_data), ")")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    log_message(warning_msg, "process_dfm", "WARNING")
  }
  
  ## --- Text Processing --------------------------------------------------------
  log_message("Processing text with STM's textProcessor", "process_dfm")
  
  # Run textProcessor directly from STM package
  processed <- tryCatch({
    stm::textProcessor(
      documents = text_data,
      metadata = meta_data,
      lowercase = TRUE,
      removestopwords = remove_stopwords,
      removenumbers = TRUE,
      removepunctuation = TRUE,
      stem = stem,
      wordLengths = c(3, Inf),
      verbose = FALSE
    )
  }, error = function(e) {
    error_msg <- paste("Error in STM textProcessor:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "process_dfm", "ERROR")
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
  
  # Store text processing statistics
  diagnostics$text_stats <- list(
    initial_docs = length(text_data),
    processed_docs = length(processed$documents),
    vocab_size = length(processed$vocab)
  )
  
  log_message(paste("Text processing complete:", length(processed$documents), 
                    "documents,", length(processed$vocab), "vocabulary terms"), 
              "process_dfm")
  
  ## --- Document Preparation ---------------------------------------------------
  log_message(paste("Preparing documents with min_docs =", min_docs, 
                    "and max_docs =", upper_thresh, 
                    "(keeping words in", min_docs, "to", upper_thresh-1, "documents)"), 
              "process_dfm")
  
  # Apply prepDocuments with error handling
  prepped <- tryCatch({
    stm::prepDocuments(
      documents = processed$documents,
      vocab = processed$vocab,
      meta = processed$meta,
      lower.thresh = min_docs - 1,  # Convert to STM parameter (exclusive threshold)
      upper.thresh = upper_thresh,  # Maximum number of documents
      verbose = TRUE
    )
  }, error = function(e) {
    error_msg <- paste("Error in STM prepDocuments:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "process_dfm", "ERROR")
    NULL
  })
  
  # Check if document preparation was successful
  if (is.null(prepped)) {
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Store document preparation statistics
  diagnostics$doc_stats <- list(
    final_docs = length(prepped$documents),
    final_vocab = length(prepped$vocab),
    docs_removed = length(processed$documents) - length(prepped$documents),
    vocab_removed = length(processed$vocab) - length(prepped$vocab)
  )
  
  log_message(paste("Document preparation complete:", length(prepped$documents), 
                    "documents,", length(prepped$vocab), "vocabulary terms"), 
              "process_dfm")
  
  ## --- Calculate processing time ----------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  ## --- Create result ----------------------------------------------------------
  
  # Return in standard format
  return(create_result(
    data = list(
      documents = prepped$documents,
      vocab = prepped$vocab,
      meta = prepped$meta
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      document_count = length(prepped$documents),
      vocabulary_size = length(prepped$vocab),
      stopwords_removed = remove_stopwords,
      stemming_applied = stem,
      min_docs = min_docs,
      exclusivity_docs = exclusivity_docs,
      stm_lower_thresh = min_docs - 1,
      stm_upper_thresh = upper_thresh,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}