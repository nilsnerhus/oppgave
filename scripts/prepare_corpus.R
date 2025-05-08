#' @title Prepare text corpus for topic modeling
#' @description Processes raw text documents into a structured corpus suitable for
#'   topic modeling analysis. Handles tokenization, stop word removal, stemming/lemmatization,
#'   and creates a document-feature matrix.
#'
#' @param text_data Data frame containing text documents
#' @param text_column Name of column containing document text (default: "pdf_text")
#' @param custom_stopwords Additional stop words to remove (default: NULL)
#' @param stem_words Whether to apply stemming (default: FALSE)
#' @param lemmatize Whether to apply lemmatization (default: TRUE)
#' @param min_word_count Minimum document frequency for terms (default: 2)
#' @param min_word_length Minimum character length for terms (default: 1)
#' @param remove_punctuation Whether to strip punctuation (default: TRUE)
#' @param min_doc_length Minimum tokens required per document (default: 50)
#' @param max_doc_proportion Maximum proportion of docs a term can appear in (default: 0.8)
#' @param output_path Path to save processed corpus (default: "data/corpus.rds")
#'
#' @return A list containing:
#'   \item{data}{List with dfm, metadata, tokens and stm_data elements}
#'   \item{metadata}{Processing information including document and token counts}
#'   \item{diagnostics}{Information about removed documents and processing issues}
#'
#' @examples
#' \dontrun{
#' corpus <- prepare_corpus(nap_data, custom_stopwords = c("https", "fig"))
#' }

prepare_corpus <- function(
    text_data, 
    text_column = "pdf_text",
    custom_stopwords = NULL, 
    stem_words = FALSE,
    lemmatize = TRUE,
    min_word_count = 2,
    min_word_length = 1,
    segment_size = 300,
    remove_punctuation = TRUE,
    min_doc_length = 50,
    max_doc_proportion = 0.8,
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
  log_message("Validating input parameters", "prepare_corpus")
  
  tryCatch({
    validate_input(text_data, c(text_column), "prepare_corpus")
    
    if (!is.null(custom_stopwords) && !is.character(custom_stopwords)) {
      stop("custom_stopwords must be a character vector")
    }
    
    if (!is.numeric(min_word_count) || min_word_count < 1) {
      stop("min_word_count must be a positive integer")
    }
    
    if (!is.numeric(min_word_length) || min_word_length < 1) {
      stop("min_word_length must be a positive integer")
    }
    
    if (!is.numeric(min_doc_length) || min_doc_length < 1) {
      stop("min_doc_length must be a positive integer")
    }
    
    if (!is.numeric(max_doc_proportion) || max_doc_proportion <= 0 || max_doc_proportion > 1) {
      stop("max_doc_proportion must be between 0 and 1")
    }
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "prepare_corpus", "ERROR")
    stop(e$message)
  })
  
  # Create segmented corpus if segment_size > 0
  if (segment_size > 0) {
    log_message(paste("Segmenting documents by", segment_size, "paragraphs"), "prepare_corpus")
    
    # Initialize segmented data
    segmented_texts <- c()
    segment_meta <- data.frame()
    
    # Process each document
    for (i in 1:nrow(text_data)) {
      doc <- text_data[i, ]
      doc_id <- as.character(doc$doc_id)
      text <- doc[[text_column]]
      
      # Split into paragraphs
      paragraphs <- unlist(strsplit(text, "\n\n+"))
      
      # Group into segments
      if (length(paragraphs) <= segment_size) {
        # If document is smaller than segment size, keep as one segment
        segments <- list(paste(paragraphs, collapse = "\n\n"))
      } else {
        # Otherwise split into segments
        n_segments <- ceiling(length(paragraphs) / segment_size)
        segments <- vector("list", n_segments)
        
        for (j in 1:n_segments) {
          start_idx <- (j-1) * segment_size + 1
          end_idx <- min(j * segment_size, length(paragraphs))
          
          if (start_idx <= length(paragraphs)) {
            segments[[j]] <- paste(paragraphs[start_idx:end_idx], collapse = "\n\n")
          }
        }
      }
      
      # Add segments to corpus
      for (j in seq_along(segments)) {
        if (nchar(segments[[j]]) > 0) {
          # Add segment text
          segmented_texts <- c(segmented_texts, segments[[j]])
          
          # Create segment metadata
          segment_id <- paste0(doc_id, "_", j)
          meta_row <- as.data.frame(doc)
          meta_row[[text_column]] <- NULL
          meta_row$doc_id <- segment_id
          meta_row$original_doc_id <- doc_id
          meta_row$segment_num <- j
          
          # Add to metadata
          if (nrow(segment_meta) == 0) {
            segment_meta <- meta_row
          } else {
            segment_meta <- rbind(segment_meta, meta_row)
          }
        }
      }
    }
    
    # Create corpus from segments
    corpus_data <- data.frame(
      doc_id = segment_meta$doc_id,
      text = segmented_texts,
      stringsAsFactors = FALSE
    )
    
    # Merge metadata
    for (col in names(segment_meta)) {
      if (!col %in% c("doc_id", text_column)) {
        corpus_data[[col]] <- segment_meta[[col]]
      }
    }
    
    log_message(paste("Created", nrow(corpus_data), "segments from", start_docs, "documents"), "prepare_corpus")
  } else {
    # No segmentation - use documents directly
    corpus_data <- text_data
    names(corpus_data)[names(corpus_data) == text_column] <- "text"
  }
  
  ## --- Assign document IDs ----------------------------------------------------
  log_message("Assigning document IDs", "prepare_corpus")
  
  text_data <- text_data %>%
    dplyr::mutate(doc_id = dplyr::row_number())
  
  start_docs <- nrow(text_data)
  diagnostics$token_stats$initial_docs <- start_docs
  
  ## --- Tokenize documents -----------------------------------------------------
  log_message("Tokenizing documents", "prepare_corpus")
  
  processed_text <- tryCatch({
    text_data %>%
      tidytext::unnest_tokens(
        word, 
        !!rlang::sym(text_column),
        to_lower = TRUE,
        strip_punct = remove_punctuation
      )
  }, error = function(e) {
    log_message(paste("Tokenization error:", e$message), "prepare_corpus", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("Tokenization failed:", e$message))
    stop(e$message)
  })
  
  ## --- Handle numbers ----------------------------------------------------------
  log_message("Filtering numeric content", "prepare_corpus")
  
  processed_text <- processed_text %>%
    # Remove pure numeric tokens (with optional commas and decimals)
    dplyr::filter(!stringr::str_detect(word, "^\\d+([,.]\\d+)*$")) %>%
    # Remove decimal numbers (e.g., .123, 0.123)
    dplyr::filter(!stringr::str_detect(word, "^[0-9]*\\.[0-9]+$")) %>%
    # Remove comma-separated numbers (e.g., 123,456)
    dplyr::filter(!stringr::str_detect(word, "^[0-9]{1,3}(,[0-9]{3})+$"))
  
  # Track initial token count
  start_tokens <- nrow(processed_text)
  diagnostics$token_stats$initial_tokens <- start_tokens
  
  ## --- Filter by word length --------------------------------------------------
  log_message(paste("Filtering words with", min_word_length, "or fewer characters"), 
              "prepare_corpus")
  
  processed_text <- processed_text %>%
    dplyr::filter(stringr::str_length(word) > min_word_length)
  
  ## --- Remove stopwords ------------------------------------------------------
  log_message("Removing stopwords", "prepare_corpus")
  
  # Remove standard stopwords
  processed_text <- processed_text %>%
    dplyr::anti_join(tidytext::get_stopwords(), by = "word")
  
  # Remove custom stopwords if provided
  if (!is.null(custom_stopwords) && length(custom_stopwords) > 0) {
    custom_stops <- tibble::tibble(word = custom_stopwords)
    processed_text <- processed_text %>%
      dplyr::anti_join(custom_stops, by = "word")
  }
  
  ## --- Apply lemmatization or stemming ---------------------------------------
  if (lemmatize) {
    log_message("Applying lemmatization", "prepare_corpus")
    processed_text <- processed_text %>%
      dplyr::mutate(word = textstem::lemmatize_words(word))
  }
  
  if (stem_words) {
    log_message("Applying word stemming", "prepare_corpus")
    processed_text <- processed_text %>%
      dplyr::mutate(word = textstem::stem_words(word))
  }
  
  ## --- Filter words by frequency ---------------------------------------------
  log_message("Filtering words by frequency", "prepare_corpus")
  
  word_doc_counts <- processed_text %>%
    dplyr::group_by(word) %>%
    dplyr::summarize(
      doc_count = dplyr::n_distinct(doc_id),
      doc_prop = doc_count / start_docs,
      .groups = "drop"
    ) %>%
    dplyr::filter(
      doc_count >= min_word_count,
      doc_prop <= max_doc_proportion
    )
  
  processed_text <- processed_text %>%
    dplyr::semi_join(word_doc_counts, by = "word")
  
  ## --- Filter documents by length ---------------------------------------------
  log_message("Filtering documents by length", "prepare_corpus")
  
  doc_lengths <- processed_text %>% 
    dplyr::count(doc_id) %>%
    dplyr::filter(n >= min_doc_length)
  
  # Track removed documents with country information
  removed_doc_ids <- setdiff(unique(processed_text$doc_id), doc_lengths$doc_id)
  
  if (length(removed_doc_ids) > 0) {
    log_message(paste("Removed", length(removed_doc_ids), 
                      "documents with fewer than", min_doc_length, "tokens"),
                "prepare_corpus", "WARNING")
    
    # Get country names for removed documents if available
    if ("country_name" %in% names(text_data)) {
      removed_info <- text_data %>%
        dplyr::filter(doc_id %in% removed_doc_ids) %>%
        dplyr::select(doc_id, country_name)
      
      diagnostics$removed_documents <- as.list(setNames(
        removed_info$country_name, paste0("doc_", removed_info$doc_id)))
    } else {
      diagnostics$removed_documents <- list(ids = removed_doc_ids)
    }
  }
  
  # Filter to keep only documents meeting the minimum length
  processed_text <- processed_text %>%
    dplyr::semi_join(doc_lengths, by = "doc_id")
  
  ## --- Extract metadata columns -----------------------------------------------
  metadata_cols <- setdiff(names(text_data), text_column)
  
  # Merge with metadata if available
  if (length(metadata_cols) > 0) {
    metadata <- text_data %>%
      dplyr::select(doc_id, dplyr::all_of(metadata_cols)) %>%
      dplyr::distinct()
    
    # Only keep metadata for documents that weren't filtered out
    metadata <- metadata %>%
      dplyr::semi_join(doc_lengths, by = "doc_id")
  } else {
    metadata <- tibble::tibble(doc_id = unique(processed_text$doc_id))
  }
  
  ## --- Create document-feature matrix -----------------------------------------
  log_message("Creating document-feature matrix", "prepare_corpus")
  
  # Create word counts
  word_counts <- processed_text %>%
    dplyr::count(doc_id, word)
  
  # Create document-feature matrix
  dfm_object <- tryCatch({
    word_counts %>%
      tidytext::cast_dfm(doc_id, word, n)
  }, error = function(e) {
    log_message(paste("DFM creation error:", e$message), "prepare_corpus", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("DFM creation failed:", e$message))
    stop(e$message)
  })
  
  ## --- Convert to STM format --------------------------------------------------
  log_message("Converting to STM format", "prepare_corpus")
  
  stm_docs <- tryCatch({
    quanteda::convert(dfm_object, to = "stm")
  }, error = function(e) {
    log_message(paste("STM conversion error:", e$message), "prepare_corpus", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("STM conversion failed:", e$message))
    stop(e$message)
  })
  
  ## --- Prepare result ---------------------------------------------------------
  final_tokens <- nrow(processed_text)
  final_docs <- nrow(metadata)
  final_terms <- length(stm_docs$vocab)
  
  # Update token statistics
  diagnostics$token_stats$final_tokens <- final_tokens
  diagnostics$token_stats$final_docs <- final_docs
  diagnostics$token_stats$removed_tokens <- start_tokens - final_tokens
  diagnostics$token_stats$removed_docs <- start_docs - final_docs
  diagnostics$token_stats$vocabulary_size <- final_terms
  
  # Create core data result - include country_metadata inside result_data
  result_data <- list(
    dfm = dfm_object,
    metadata = metadata,  # This contains country metadata
    country_metadata = metadata,  # Add explicitly as country_metadata
    tokens = processed_text,
    stm_data = list(
      documents = stm_docs$documents,
      vocab = stm_docs$vocab
    )
  )
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    original_docs = start_docs,
    final_docs = final_docs,
    original_tokens = start_tokens,
    final_tokens = final_tokens,
    vocabulary_size = final_terms,
    processing_options = list(
      lemmatize = lemmatize,
      stem_words = stem_words,
      min_word_count = min_word_count,
      min_word_length = min_word_length,
      min_doc_length = min_doc_length,
      max_doc_proportion = max_doc_proportion
    )
  )
  
  # Make the result also compatible with find_best_k by including key elements
  # at the top level (for backward compatibility) while still using the new structure
  final_result <- list(
    # New standardized result structure
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics,
    
    # Direct access for compatibility (same object references, not copies)
    dfm = result_data$dfm,
    metadata = result_data$metadata,
    stm_data = result_data$stm_data,
    tokens = result_data$tokens
  )
  
  log_message(sprintf("Processing complete: %d docs, %d tokens, %d terms", 
                      final_docs, final_tokens, final_terms), 
              "prepare_corpus")
  
  return(final_result)
}