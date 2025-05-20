#' @title Prepare documents for Structural Topic Modeling
#' @description Takes standardized text data, validates indices, and prepares it for STM modeling
#'   by filtering vocabulary based on document frequency thresholds. Assumes input is already
#'   in standardized format from standardize_text_processor.
#'   
#' @param processed_result Result from standardize_text_processor containing processed text data
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
prep_documents <- function(
    processed_result,
    lower.thresh = 2,
    upper.thresh = 0.9
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    vocab_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "prep_documents")
  
  # Validate processed_result structure
  if (!is.list(processed_result) || 
      !"data" %in% names(processed_result) ||
      !"processed" %in% names(processed_result$data)) {
    error_msg <- "processed_result must be the output from standardize_text_processor() with processed data"
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
  
  ## --- Extract processed data -------------------------------------------------
  log_message("Extracting processed text data", "prep_documents")
  
  # Extract config if available
  config <- NULL
  if ("config" %in% names(processed_result$data)) {
    config <- processed_result$data$config
  }
  
  ## --- Basic validation of document indices -----------------------------------
  log_message("Validating document indices", "prep_documents")
  
  # Get vocabulary size for index validation
  vocab_size <- length(processed$vocab)
  
  # Count non-empty documents
  doc_lengths <- sapply(processed$documents, function(d) nrow(d))
  empty_docs <- which(doc_lengths == 0)
  
  log_message(paste("Found", length(empty_docs), "empty documents out of", 
                    length(processed$documents), "total"), "prep_documents")
  
  # Validate indices are within bounds
  valid_docs <- processed$documents
  fixed_count <- 0
  
  for (i in seq_along(valid_docs)) {
    if (nrow(valid_docs[[i]]) > 0) {
      invalid_indices <- which(valid_docs[[i]][, "indices"] < 1 | 
                                 valid_docs[[i]][, "indices"] > vocab_size)
      
      if (length(invalid_indices) > 0) {
        if (length(invalid_indices) < nrow(valid_docs[[i]])) {
          # Remove invalid indices
          valid_docs[[i]] <- valid_docs[[i]][-invalid_indices, , drop = FALSE]
          fixed_count <- fixed_count + 1
        } else {
          # All indices are invalid, create empty document
          valid_docs[[i]] <- matrix(integer(0), ncol = 2)
          colnames(valid_docs[[i]]) <- c("indices", "counts")
        }
      }
    }
  }
  
  if (fixed_count > 0) {
    log_message(paste("Fixed indices in", fixed_count, "documents"), "prep_documents", "WARNING")
  }
  
  ## --- Record vocabulary statistics before filtering -------------------------
  initial_vocab_size <- length(processed$vocab)
  initial_doc_count <- length(processed$documents)
  non_empty_count <- sum(sapply(valid_docs, function(d) nrow(d) > 0))
  
  log_message(paste("Initial vocabulary size:", initial_vocab_size), "prep_documents")
  log_message(paste("Initial document count:", initial_doc_count, 
                    "(", non_empty_count, "non-empty)"), "prep_documents")
  
  ## --- Apply prepDocuments function ------------------------------------------
  log_message(paste("Preparing documents with thresh:", lower.thresh, "to", upper.thresh), 
              "prep_documents")
  
  # Check if we have any non-empty documents
  if (non_empty_count == 0) {
    error_msg <- "No non-empty documents after validation"
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
      documents = valid_docs,
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
        documents = valid_docs,
        vocab = processed$vocab,
        meta = processed$meta,
        lower.thresh = 1,
        upper.thresh = 1.0,
        verbose = FALSE
      )
    }, error = function(e2) {
      log_message(paste("Fallback also failed:", e2$message), "prep_documents", "WARNING")
      NULL
    })
  })
  
  # Check if processing was successful
  if (is.null(prepped)) {
    error_msg <- "Failed to prepare documents with both primary and fallback settings"
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
  
  ## --- Calculate filtering statistics ----------------------------------------
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
  
  ## --- Validate final document format ----------------------------------------
  log_message("Ensuring consistent document format in output", "prep_documents")
  
  standardized_docs <- list()
  
  for (i in seq_along(prepped$documents)) {
    doc <- prepped$documents[[i]]
    
    # Ensure standard format (2-column matrix with named columns)
    if (is.matrix(doc) && ncol(doc) >= 2) {
      doc_matrix <- doc[, 1:2, drop = FALSE]
    } else if (is.list(doc) && all(c("indices", "counts") %in% names(doc))) {
      doc_matrix <- cbind(doc$indices, doc$counts)
    } else {
      # Create empty document as fallback
      doc_matrix <- matrix(integer(0), ncol = 2)
    }
    
    # Set column names
    colnames(doc_matrix) <- c("indices", "counts")
    standardized_docs[[i]] <- doc_matrix
  }
  
  # Replace with standardized documents
  prepped$documents <- standardized_docs
  
  ## --- Calculate processing time and create result ---------------------------
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