#' @title Prepare text corpus for topic modeling
#' @description Processes raw text documents into a tokenized corpus suitable for
#'   topic modeling analysis. Handles tokenization, stop word removal, and filtering.
#'
#' @param text_data Data frame containing text documents
#' @param text_column Name of column containing document text (default: "pdf_text")
#' @param custom_stopwords Additional stop words to remove (default: NULL)
#' @param lemmatize Whether to apply lemmatization (default: TRUE)
#' @param min_word_length Minimum character length for terms (default: 3)
#' @param min_doc_prop Minimum proportion of documents a term must appear in (default: 0.02)
#' @param max_doc_prop Maximum proportion of docs a term can appear in (default: 0.7)
#' @param min_doc_length Minimum tokens required per document (default: 50)
#' @param output_path Path to save processed corpus (default: "data/corpus.rds")
#'
#' @return A list containing:
#'   \item{data}{Cleaned corpus with tokenized documents and metadata}
#'   \item{metadata}{Processing information including token statistics}
#'   \item{diagnostics}{Information about removed documents and processing issues}
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' corpus <- prepare_corpus(nap_data)
#' 
#' # With custom stopwords
#' corpus <- prepare_corpus(nap_data, custom_stopwords = extra_stopwords)
#' }

prepare_corpus <- function(
    text_data, 
    text_column = "pdf_text",
    custom_stopwords = NULL, 
    lemmatize = TRUE,
    min_word_length = 3,
    min_doc_prop = 0.02,
    max_doc_prop = 0.7,
    min_doc_length = 50,
    output_path = "data/corpus.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    removed_documents = list(),
    processing_issues = character(),
    token_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "prepare_corpus")
  
  tryCatch({
    if (!is.data.frame(text_data)) {
      stop("Input must be a dataframe")
    }
    
    if (nrow(text_data) == 0) {
      stop("Input dataframe has no rows")
    }
    
    if (!text_column %in% names(text_data)) {
      stop(paste("Input is missing required column:", text_column))
    }
    
    if (!is.numeric(min_doc_prop) || min_doc_prop < 0 || min_doc_prop > 1) {
      stop("min_doc_prop must be a value between 0 and 1")
    }
    
    if (!is.numeric(max_doc_prop) || max_doc_prop < 0 || max_doc_prop > 1) {
      stop("max_doc_prop must be a value between 0 and 1")
    }
    
    if (min_doc_prop >= max_doc_prop) {
      stop("min_doc_prop must be less than max_doc_prop")
    }
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "prepare_corpus", "ERROR")
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, e$message)
    stop(e$message)
  })
  
  ## --- Ensure document IDs ----------------------------------------------------
  log_message("Preparing document IDs", "prepare_corpus")
  
  # Ensure we have document IDs
  if (!"doc_id" %in% names(text_data)) {
    text_data$doc_id <- as.character(1:nrow(text_data))
    log_message("Added sequential document IDs", "prepare_corpus")
  } else {
    # Ensure doc_id is character type for consistency
    text_data$doc_id <- as.character(text_data$doc_id)
  }
  
  # Record original document count
  original_doc_count <- nrow(text_data)
  diagnostics$token_stats$original_docs <- original_doc_count
  
  log_message(paste("Processing", original_doc_count, "documents"), "prepare_corpus")
  
  ## --- Prepare corpus dataframe -----------------------------------------------
  # Create corpus dataframe with document text and IDs
  corpus_data <- text_data
  names(corpus_data)[names(corpus_data) == text_column] <- "text"
  
  # Check for empty documents early
  empty_docs <- which(is.na(corpus_data$text) | nchar(corpus_data$text) == 0)
  if (length(empty_docs) > 0) {
    log_message(paste("Removing", length(empty_docs), "empty documents"), "prepare_corpus")
    corpus_data <- corpus_data[-empty_docs, ]
    
    diagnostics$removed_documents$empty_count <- length(empty_docs)
    diagnostics$removed_documents$empty_ids <- corpus_data$doc_id[empty_docs]
  }
  
  ## --- Tokenize documents -----------------------------------------------------
  log_message("Tokenizing documents", "prepare_corpus")
  
  tokenized_text <- tryCatch({
    corpus_data %>%
      tidytext::unnest_tokens(
        word, 
        text,
        to_lower = TRUE,
        strip_punct = TRUE
      )
  }, error = function(e) {
    log_message(paste("Tokenization error:", e$message), "prepare_corpus", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("Tokenization failed:", e$message))
    stop(e$message)
  })
  
  # Track initial token count
  start_tokens <- nrow(tokenized_text)
  diagnostics$token_stats$initial_tokens <- start_tokens
  log_message(paste("Initial tokenization produced", start_tokens, "tokens"), "prepare_corpus")
  
  ## --- Filter numeric content -------------------------------------------------
  log_message("Filtering numeric content", "prepare_corpus")
  
  tokenized_text <- tokenized_text %>%
    # Remove pure numeric tokens (with optional commas and decimals)
    dplyr::filter(!stringr::str_detect(word, "^\\d+([,.]\\d+)*$")) %>%
    # Remove decimal numbers (e.g., .123, 0.123)
    dplyr::filter(!stringr::str_detect(word, "^[0-9]*\\.[0-9]+$")) %>%
    # Remove comma-separated numbers (e.g., 123,456)
    dplyr::filter(!stringr::str_detect(word, "^[0-9]{1,3}(,[0-9]{3})+$"))
  
  ## --- Apply word length filter -----------------------------------------------
  log_message(paste("Filtering words shorter than", min_word_length, "characters"), 
              "prepare_corpus")
  
  tokenized_text <- tokenized_text %>%
    dplyr::filter(stringr::str_length(word) >= min_word_length)
  
  ## --- Remove stopwords ------------------------------------------------------
  log_message("Removing stopwords", "prepare_corpus")
  
  # Remove standard stopwords
  tokenized_text <- tokenized_text %>%
    dplyr::anti_join(tidytext::get_stopwords(), by = "word")
  
  # Remove custom stopwords if provided
  if (!is.null(custom_stopwords) && length(custom_stopwords) > 0) {
    custom_stops <- tibble::tibble(word = custom_stopwords)
    tokenized_text <- tokenized_text %>%
      dplyr::anti_join(custom_stops, by = "word")
    
    log_message(paste("Removed", length(custom_stopwords), "custom stopwords"), "prepare_corpus")
  }
  
  ## --- Apply lemmatization if requested ---------------------------------------
  if (lemmatize) {
    log_message("Applying lemmatization", "prepare_corpus")
    
    tokenized_text <- tokenized_text %>%
      dplyr::mutate(word = textstem::lemmatize_words(word))
  }
  
  ## --- Filter words by document frequency ------------------------------------
  log_message("Filtering words by document frequency", "prepare_corpus")
  
  # Count documents
  doc_count <- length(unique(tokenized_text$doc_id))
  
  # Calculate minimum and maximum document counts from proportions
  min_doc_count <- ceiling(min_doc_prop * doc_count)
  max_doc_count <- floor(max_doc_prop * doc_count)
  
  log_message(paste("Words must appear in at least", min_doc_count, 
                    "documents and at most", max_doc_count, "documents"), 
              "prepare_corpus")
  
  # Count how many documents each word appears in
  word_doc_counts <- tokenized_text %>%
    dplyr::group_by(word) %>%
    dplyr::summarize(
      doc_count = dplyr::n_distinct(doc_id),
      doc_prop = doc_count / doc_count,
      .groups = "drop"
    ) %>%
    dplyr::filter(
      doc_count >= min_doc_count,
      doc_count <= max_doc_count
    )
  
  # Apply frequency filter
  tokenized_text <- tokenized_text %>%
    dplyr::semi_join(word_doc_counts, by = "word")
  
  log_message(paste("Retained", nrow(word_doc_counts), "unique words after frequency filtering"), 
              "prepare_corpus")
  
  ## --- Filter documents by length ---------------------------------------------
  log_message("Filtering documents by length", "prepare_corpus")
  
  # Count tokens per document after all filtering
  doc_lengths <- tokenized_text %>% 
    dplyr::count(doc_id, name = "token_count")
  
  # Filter by minimum document length
  valid_docs <- doc_lengths %>%
    dplyr::filter(token_count >= min_doc_length)
  
  # Track removed documents
  removed_doc_ids <- setdiff(doc_lengths$doc_id, valid_docs$doc_id)
  
  if (length(removed_doc_ids) > 0) {
    log_message(paste("Removed", length(removed_doc_ids), 
                      "documents with fewer than", min_doc_length, "tokens"),
                "prepare_corpus", "WARNING")
    
    diagnostics$removed_documents$short_count <- length(removed_doc_ids)
    diagnostics$removed_documents$short_ids <- removed_doc_ids
  }
  
  # Filter tokenized text to keep only documents meeting minimum length
  tokenized_text <- tokenized_text %>%
    dplyr::semi_join(valid_docs, by = "doc_id")
  
  ## --- Prepare final output ---------------------------------------------------
  # Extract metadata for remaining documents
  metadata_cols <- setdiff(names(text_data), c(text_column, "doc_id"))
  
  if (length(metadata_cols) > 0) {
    doc_metadata <- text_data[text_data$doc_id %in% valid_docs$doc_id, c("doc_id", metadata_cols)]
  } else {
    doc_metadata <- data.frame(doc_id = valid_docs$doc_id)
  }
  
  # Create vocabulary list
  vocabulary <- sort(unique(tokenized_text$word))
  
  ## --- Prepare result ---------------------------------------------------------
  # Calculate final statistics
  final_tokens <- nrow(tokenized_text)
  final_docs <- length(unique(tokenized_text$doc_id))
  final_terms <- length(vocabulary)
  
  # Update token statistics
  diagnostics$token_stats$final_tokens <- final_tokens
  diagnostics$token_stats$final_docs <- final_docs
  diagnostics$token_stats$removed_tokens <- start_tokens - final_tokens
  diagnostics$token_stats$vocabulary_size <- final_terms
  
  # Create core data result
  result_data <- list(
    tokens = tokenized_text,       # Tokenized text data
    metadata = doc_metadata,       # Document metadata
    vocab = vocabulary             # Vocabulary list
  )
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    original_docs = original_doc_count,
    final_docs = final_docs,
    original_tokens = start_tokens,
    final_tokens = final_tokens,
    vocabulary_size = final_terms,
    processing_options = list(
      lemmatize = lemmatize,
      min_word_length = min_word_length,
      min_doc_prop = min_doc_prop,
      max_doc_prop = max_doc_prop,
      min_doc_length = min_doc_length
    )
  )
  
  # Final result structure
  final_result <- create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  )
  
  log_message(sprintf("Processing complete: %d docs, %d tokens, %d terms", 
                      final_docs, final_tokens, final_terms), 
              "prepare_corpus")
  
  return(final_result)
}