#' @title Prepare documents for Structural Topic Modeling
#' @description Takes processed text data, fixes document matrix orientation if needed,
#'   and prepares it for STM modeling by filtering vocabulary based on document frequency
#'   thresholds. Uses STM's prepDocuments function to create properly formatted input for 
#'   topic modeling.
#'   
#' @param processed_result Result from process_text() containing processed text data
#' @param lower.thresh Minimum number of documents a term must appear in (default: 2)
#' @param upper.thresh Maximum proportion of documents a term can appear in (default: 0.9)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item stm_input - Properly formatted input for STM (documents, vocab)
#'       \item metadata - Document metadata properly aligned with documents
#'       \item config - Original configuration from metadata
#'     }
#'   }
#'   \item{metadata}{Processing information and statistics}
#'   \item{diagnostics}{Filtering statistics and issues encountered}
#'
prep_documents <- function(
    processed_result,
    lower.thresh = 2,
    upper.thresh = 0.9
) {
  ## --- Setup & Initialization -------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    vocab_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------
  log_message("Validating input data", "prep_documents")
  
  # Validate processed_result structure
  if (!is.list(processed_result) || 
      !"data" %in% names(processed_result) ||
      !"processed" %in% names(processed_result$data)) {
    error_msg <- "processed_result must be the output from process_text() with processed data"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prep_documents", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check for required components in processed data
  processed <- processed_result$data$processed
  required_components <- c("documents", "vocab", "meta")
  missing_components <- setdiff(required_components, names(processed))
  
  if (length(missing_components) > 0) {
    error_msg <- paste("Processed data missing required components:", 
                       paste(missing_components, collapse = ", "))
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prep_documents", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Extract processed data -------------------------------------------
  log_message("Extracting processed text data", "prep_documents")
  
  # Extract config if available
  config <- NULL
  if ("config" %in% names(processed_result$data)) {
    config <- processed_result$data$config
  }
  
  ## --- Fix document matrices by transposing if needed and validate indices ---
  log_message("Checking document matrix orientation and indices", "prep_documents")
  
  # Get vocab size for index validation
  vocab_size <- length(processed$vocab)
  
  # Process each document
  fixed_docs <- list()
  for (i in 1:length(processed$documents)) {
    doc <- processed$documents[[i]]
    
    if (!is.null(doc) && is.matrix(doc) && nrow(doc) > 0 && ncol(doc) > 0) {
      # Check if transposition is needed
      if (ncol(doc) > nrow(doc)) {
        # Transpose the matrix
        doc <- t(doc)
      }
      
      # Ensure it has exactly 2 columns
      if (ncol(doc) != 2) {
        log_message(paste("Document", i, "has incorrect number of columns:", 
                          ncol(doc), "- creating empty document"), "prep_documents", "WARNING")
        doc <- matrix(integer(0), ncol = 2)
      } else {
        # Validate that all indices are within bounds (1 to vocab_size)
        invalid_indices <- which(doc[,1] < 1 | doc[,1] > vocab_size)
        if (length(invalid_indices) > 0) {
          log_message(paste("Document", i, "has", length(invalid_indices), 
                            "invalid indices - removing them"), "prep_documents", "WARNING")
          if (length(invalid_indices) < nrow(doc)) {
            # Remove invalid indices
            doc <- doc[-invalid_indices, , drop = FALSE]
          } else {
            # All indices are invalid, create empty document
            doc <- matrix(integer(0), ncol = 2)
          }
        }
      }
    } else {
      # Create empty document
      doc <- matrix(integer(0), ncol = 2)
    }
    
    # Set column names and add to fixed_docs
    colnames(doc) <- c("indices", "counts")
    fixed_docs[[i]] <- doc
  }
  
  # Preserve document names
  if (!is.null(names(processed$documents))) {
    names(fixed_docs) <- names(processed$documents)
  }
  
  log_message(paste("Processed", length(fixed_docs), "document matrices"), "prep_documents")
  
  ## --- Remove empty documents -------------------------------------------
  # Count non-empty documents
  doc_lengths <- sapply(fixed_docs, function(d) nrow(d))
  empty_docs <- which(doc_lengths == 0)
  
  # Remove empty documents and corresponding metadata
  if (length(empty_docs) > 0) {
    log_message(paste("Removing", length(empty_docs), "empty documents"), "prep_documents", "WARNING")
    fixed_docs <- fixed_docs[-empty_docs]
    
    # Also update metadata if possible
    if (!is.null(processed$meta) && nrow(processed$meta) == length(processed$documents)) {
      processed$meta <- processed$meta[-empty_docs, , drop = FALSE]
    }
  }
  
  # Recount non-empty documents
  non_empty_count <- length(fixed_docs)
  
  ## --- Record vocabulary statistics before filtering --------------------
  initial_vocab_size <- length(processed$vocab)
  initial_doc_count <- length(processed$documents)
  
  log_message(paste("Initial vocabulary size:", initial_vocab_size), "prep_documents")
  log_message(paste("Initial document count:", initial_doc_count, 
                    "(", non_empty_count, "after removing empty documents)"), "prep_documents")
  
  ## --- Apply prepDocuments function -------------------------------------
  log_message(paste("Preparing documents with thresh:", lower.thresh, "to", upper.thresh), 
              "prep_documents")
  
  # Check if we have any documents left
  if (non_empty_count == 0) {
    error_msg <- "No non-empty documents after processing"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prep_documents", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Apply prepDocuments with error handling
  prepped <- tryCatch({
    stm::prepDocuments(
      documents = fixed_docs,
      vocab = processed$vocab,
      meta = processed$meta,
      lower.thresh = lower.thresh,
      upper.thresh = upper.thresh,
      verbose = FALSE
    )
  }, error = function(e) {
    error_msg <- paste("Error in STM prepDocuments:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "prep_documents", "ERROR")
    
    # Try with more lenient thresholds as fallback
    log_message("Trying with minimal thresholds as fallback", "prep_documents", "WARNING")
    tryCatch({
      stm::prepDocuments(
        documents = fixed_docs,
        vocab = processed$vocab,
        meta = processed$meta,
        lower.thresh = 1,
        upper.thresh = 1.0,
        verbose = FALSE
      )
    }, error = function(e2) {
      # If that also fails, return processed data
      log_message(paste("Fallback also failed:", e2$message), "prep_documents", "WARNING")
      NULL
    })
  })
  
  # Check if processing was successful
  if (is.null(prepped)) {
    # Create a fallback minimal result structure
    log_message("Creating fallback result without filtering", "prep_documents", "WARNING")
    prepped <- list(
      documents = fixed_docs,
      vocab = processed$vocab,
      meta = processed$meta
    )
  }
  
  ## --- Calculate filtering statistics -----------------------------------
  final_vocab_size <- length(prepped$vocab)
  final_doc_count <- length(prepped$documents)
  
  vocab_removed <- initial_vocab_size - final_vocab_size
  vocab_removed_pct <- round(vocab_removed / initial_vocab_size * 100, 2)
  
  docs_removed <- initial_doc_count - final_doc_count
  docs_removed_pct <- if(initial_doc_count > 0) round(docs_removed / initial_doc_count * 100, 2) else 0
  
  diagnostics$vocab_stats <- list(
    initial_vocab_size = initial_vocab_size,
    final_vocab_size = final_vocab_size,
    vocab_removed = vocab_removed,
    vocab_removed_pct = vocab_removed_pct,
    initial_doc_count = initial_doc_count,
    final_doc_count = final_doc_count,
    docs_removed = docs_removed,
    docs_removed_pct = docs_removed_pct
  )
  
  log_message(paste("Vocabulary reduced from", initial_vocab_size, "to", final_vocab_size, 
                    "terms (", vocab_removed_pct, "% reduction)"), "prep_documents")
  
  if (docs_removed > 0) {
    log_message(paste(docs_removed, "documents removed due to filtering (",
                      docs_removed_pct, "%)"), "prep_documents", "WARNING")
  }
  
  ## --- Calculate processing time and create result ----------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Return in standard format
  return(create_result(
    data = list(
      stm_input = list(
        documents = prepped$documents,
        vocab = prepped$vocab
      ),
      metadata = prepped$meta,
      config = config
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      document_count = final_doc_count,
      vocabulary_size = final_vocab_size,
      lower_thresh = lower.thresh,
      upper.thresh = upper.thresh,
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}