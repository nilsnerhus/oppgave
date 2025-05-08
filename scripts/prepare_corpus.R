#' @title Prepare text corpus for topic modeling with improved segmentation
#' @description Processes raw text documents into a structured corpus suitable for
#'   topic modeling analysis. Handles tokenization, stop word removal, stemming/lemmatization,
#'   and creates a document-feature matrix. Includes improved sentence-based segmentation.
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
#' @param segmentation Number of sentences per segment (default: 300, 0 for no segmentation)
#' @param output_path Path to save processed corpus (default: "data/corpus.rds")
#'
#' @return A list containing:
#'   \item{data}{List with dfm, metadata, tokens, segments and stm_data elements}
#'   \item{metadata}{Processing information including document and token counts}
#'   \item{diagnostics}{Information about removed documents and processing issues}
#'
#' @examples
#' \dontrun{
#' # Process without segmentation
#' corpus <- prepare_corpus(nap_data, segmentation = 0)
#' 
#' # Process with segmentation (300 sentences per segment)
#' corpus <- prepare_corpus(nap_data, segmentation = 300)
#' }

prepare_corpus <- function(
    text_data, 
    text_column = "pdf_text",
    custom_stopwords = NULL, 
    stem_words = FALSE,
    lemmatize = TRUE,
    min_word_count = 2,
    min_word_length = 1,
    remove_punctuation = TRUE,
    min_doc_length = 50,
    max_doc_proportion = 0.8,
    segmentation = 300,
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
    removed_segments = list(),
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
    
    if (!is.numeric(min_word_count) || min_word_count < 0) {
      stop("min_word_count must be a non-negative number")
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
    
    if (!is.numeric(segmentation) || segmentation < 0) {
      stop("segmentation must be a non-negative number")
    }
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "prepare_corpus", "ERROR")
    stop(e$message)
  })
  
  ## --- Ensure document IDs ----------------------------------------------------
  log_message("Ensuring document IDs are present", "prepare_corpus")
  
  # Ensure we have document IDs
  if (!"doc_id" %in% names(text_data)) {
    text_data$doc_id <- as.character(1:nrow(text_data))
  } else {
    # Ensure doc_id is character type for consistency
    text_data$doc_id <- as.character(text_data$doc_id)
  }
  
  # Track original document count
  original_doc_count <- nrow(text_data)
  diagnostics$token_stats$original_docs <- original_doc_count
  
  ## --- Document segmentation --------------------------------------------------
  # Initialize variables to track segmentation
  use_segmentation <- segmentation > 0
  segment_mapping <- NULL
  
  if (use_segmentation) {
    log_message(paste("Segmenting documents into chunks of", segmentation, "sentences"), "prepare_corpus")
    
    # Initialize segment storage
    segmented_texts <- character(0)
    segment_ids <- character(0)
    segment_mapping <- data.frame(
      segment_id = character(0),
      original_doc_id = character(0),
      segment_num = integer(0),
      segment_size = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Process each document for segmentation
    for (i in 1:nrow(text_data)) {
      doc <- text_data[i, ]
      doc_id <- as.character(doc$doc_id)
      text <- doc[[text_column]]
      
      # Skip empty documents
      if (is.na(text) || nchar(text) == 0) {
        log_message(paste("Skipping empty document:", doc_id), "prepare_corpus", "WARNING")
        next
      }
      
      # Split document into sentences using a regex pattern
      # This pattern looks for end punctuation followed by whitespace or end of string
      sentences <- unlist(stringr::str_split(text, "(?<=[.!?])\\s+|(?<=[.!?])$"))
      
      # Remove empty sentences
      sentences <- sentences[nchar(sentences) > 0]
      
      # If document has no valid sentences, skip it
      if (length(sentences) == 0) {
        log_message(paste("Skipping document with no valid sentences:", doc_id), 
                    "prepare_corpus", "WARNING")
        next
      }
      
      # Group sentences into segments
      n_sentences <- length(sentences)
      
      # If document is smaller than segmentation threshold, keep as one segment
      if (n_sentences <= segmentation) {
        segment_id <- paste0(doc_id, "_1")
        segmented_texts <- c(segmented_texts, text)
        segment_ids <- c(segment_ids, segment_id)
        
        # Add to segment mapping
        segment_mapping <- rbind(segment_mapping, data.frame(
          segment_id = segment_id,
          original_doc_id = doc_id,
          segment_num = 1,
          segment_size = n_sentences,
          stringsAsFactors = FALSE
        ))
      } else {
        # Otherwise split into segments of specified size
        n_segments <- ceiling(n_sentences / segmentation)
        
        for (j in 1:n_segments) {
          start_idx <- (j-1) * segmentation + 1
          end_idx <- min(j * segmentation, n_sentences)
          
          if (start_idx <= n_sentences) {
            # Create segment text
            segment_text <- paste(sentences[start_idx:end_idx], collapse = " ")
            segment_id <- paste0(doc_id, "_", j)
            
            # Only add segment if it has content
            if (nchar(segment_text) > 0) {
              segmented_texts <- c(segmented_texts, segment_text)
              segment_ids <- c(segment_ids, segment_id)
              
              # Add to segment mapping
              segment_mapping <- rbind(segment_mapping, data.frame(
                segment_id = segment_id,
                original_doc_id = doc_id,
                segment_num = j,
                segment_size = end_idx - start_idx + 1,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
    
    log_message(paste("Created", nrow(segment_mapping), "segments from", 
                      length(unique(segment_mapping$original_doc_id)), "documents"), 
                "prepare_corpus")
    
    # Create segmented corpus data frame
    corpus_data <- data.frame(
      doc_id = segment_ids,
      text = segmented_texts,
      stringsAsFactors = FALSE
    )
    
    # Add original metadata to segments
    metadata_cols <- setdiff(names(text_data), c(text_column, "doc_id"))
    if (length(metadata_cols) > 0) {
      # Create lookup table for original document metadata
      original_metadata <- text_data[, c("doc_id", metadata_cols)]
      
      # Join segment mapping with original metadata
      segment_metadata <- merge(segment_mapping, original_metadata,
                                by.x = "original_doc_id", by.y = "doc_id")
      
      # Reorder columns to have segment_id first, then rename to doc_id for processing
      segment_metadata <- segment_metadata[, c("segment_id", setdiff(names(segment_metadata), "segment_id"))]
      names(segment_metadata)[names(segment_metadata) == "segment_id"] <- "doc_id"
      
      # Store for later use
      full_segment_metadata <- segment_metadata
    } else {
      # If no additional metadata, just use the segment mapping
      segment_metadata <- segment_mapping
      names(segment_metadata)[names(segment_metadata) == "segment_id"] <- "doc_id"
      full_segment_metadata <- segment_metadata
    }
  } else {
    # No segmentation - use original documents
    log_message("Using original documents without segmentation", "prepare_corpus")
    corpus_data <- text_data
    names(corpus_data)[names(corpus_data) == text_column] <- "text"
    full_segment_metadata <- NULL
  }
  
  ## --- Tokenize documents -----------------------------------------------------
  log_message("Tokenizing documents", "prepare_corpus")
  
  processed_text <- tryCatch({
    corpus_data %>%
      tidytext::unnest_tokens(
        word, 
        text,
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
  
  # Count how many documents each word appears in
  doc_count_total <- if(use_segmentation) {
    length(unique(segment_mapping$original_doc_id))
  } else {
    nrow(corpus_data)
  }
  
  word_doc_counts <- processed_text %>%
    dplyr::group_by(word) %>%
    dplyr::summarize(
      doc_count = dplyr::n_distinct(doc_id),
      doc_prop = doc_count / doc_count_total,
      .groups = "drop"
    ) %>%
    dplyr::filter(
      doc_count >= min_word_count,
      doc_prop <= max_doc_proportion
    )
  
  processed_text <- processed_text %>%
    dplyr::semi_join(word_doc_counts, by = "word")
  
  # Calculate document lengths immediately after filtering by word frequency
  doc_lengths <- processed_text %>% 
    dplyr::count(doc_id)
  
  ## --- Filter documents by length ---------------------------------------------
  log_message("Filtering documents by length", "prepare_corpus")
  
  # Filter by minimum document length
  min_length_docs <- doc_lengths %>%
    dplyr::filter(n >= min_doc_length)
  
  # Track removed documents with country information
  removed_doc_ids <- setdiff(doc_lengths$doc_id, min_length_docs$doc_id)
  
  if (length(removed_doc_ids) > 0) {
    log_message(paste("Removed", length(removed_doc_ids), 
                      "documents/segments with fewer than", min_doc_length, "tokens"),
                "prepare_corpus", "WARNING")
    
    if (use_segmentation) {
      # Get original document IDs for removed segments
      removed_originals <- segment_mapping$original_doc_id[segment_mapping$segment_id %in% removed_doc_ids]
      diagnostics$removed_segments$count <- length(removed_doc_ids)
      diagnostics$removed_segments$segment_ids <- removed_doc_ids
      diagnostics$removed_segments$original_docs <- unique(removed_originals)
    } else {
      diagnostics$removed_documents$count <- length(removed_doc_ids)
      diagnostics$removed_documents$doc_ids <- removed_doc_ids
    }
  }
  
  # Filter processed text to keep only documents meeting minimum length
  processed_text <- processed_text %>%
    dplyr::semi_join(min_length_docs, by = "doc_id")
  
  # Update doc_lengths to reflect filtering
  doc_lengths <- min_length_docs
  
  ## --- Count tokens per document for weighting -------------------------------
  token_counts <- processed_text %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarize(token_count = dplyr::n(), .groups = "drop")
  
  # If using segmentation, add token counts to segment metadata
  if (use_segmentation) {
    full_segment_metadata <- merge(full_segment_metadata, token_counts, by = "doc_id", all.x = TRUE)
    full_segment_metadata$token_count[is.na(full_segment_metadata$token_count)] <- 0
  }
  
  ## --- Check if segments from same document were all removed ----------------
  if (use_segmentation) {
    # Collect remaining segments
    remaining_segments <- unique(processed_text$doc_id)
    
    # Find original documents that have at least one segment
    remaining_orig_docs <- segment_mapping$original_doc_id[
      segment_mapping$segment_id %in% remaining_segments]
    remaining_orig_docs <- unique(remaining_orig_docs)
    
    # Count original documents lost entirely
    original_docs_lost <- setdiff(unique(segment_mapping$original_doc_id), remaining_orig_docs)
    
    if (length(original_docs_lost) > 0) {
      log_message(paste("Warning:", length(original_docs_lost), 
                        "original documents lost all their segments"), 
                  "prepare_corpus", "WARNING")
      diagnostics$removed_documents$count <- length(original_docs_lost)
      diagnostics$removed_documents$doc_ids <- original_docs_lost
    }
    
    # Update segment metadata to include only remaining segments
    if (!is.null(full_segment_metadata)) {
      full_segment_metadata <- full_segment_metadata[
        full_segment_metadata$doc_id %in% remaining_segments, ]
    }
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
  
  ## --- Debug variables before STM conversion -----------------------------------
  log_message("Debugging variables before STM conversion", "prepare_corpus")
  log_message(paste("Available objects:", paste(ls(), collapse=", ")), "prepare_corpus")
  log_message(paste("dfm_object exists:", exists("dfm_object")), "prepare_corpus")
  
  ## --- Convert to STM format --------------------------------------------------
  log_message("Converting to STM format", "prepare_corpus")
  
  stm_docs <- tryCatch({
    # Convert DFM to STM format
    stm_result <- quanteda::convert(dfm_object, to = "stm")
    
    # Preserve document IDs if rownames exist
    if (!is.null(rownames(dfm_object))) {
      names(stm_result$documents) <- rownames(dfm_object)
    }
    
    stm_result
  }, error = function(e) {
    log_message(paste("STM conversion error:", e$message), "prepare_corpus", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("STM conversion failed:", e$message))
    stop(e$message)
  })
  
  ## --- Prepare metadata for STM -----------------------------------------------
  # Get metadata for remaining documents
  if (use_segmentation) {
    # For segmentation, use full segment metadata
    stm_metadata <- full_segment_metadata
    
    # Critical change - ensure row names match document names
    rownames(stm_metadata) <- stm_metadata$doc_id
  } else {
    # For regular processing, use original metadata
    metadata_cols <- setdiff(names(text_data), text_column)
    if (length(metadata_cols) > 0) {
      stm_metadata <- text_data[, c("doc_id", metadata_cols)]
    } else {
      stm_metadata <- data.frame(doc_id = text_data$doc_id)
    }
    # Filter to include only documents that made it through preprocessing
    stm_metadata <- stm_metadata[stm_metadata$doc_id %in% rownames(dfm_object), ]
    
    # Critical change - ensure row names match document names
    rownames(stm_metadata) <- stm_metadata$doc_id
  }
  
  # Store the metadata in the STM data
  stm_data$meta <- stm_metadata
  
  ## --- Prepare result ---------------------------------------------------------
  final_tokens <- nrow(processed_text)
  final_docs <- nrow(doc_lengths)
  final_terms <- length(stm_docs$vocab)
  
  # Update token statistics
  diagnostics$token_stats$final_tokens <- final_tokens
  diagnostics$token_stats$final_docs <- final_docs
  diagnostics$token_stats$removed_tokens <- start_tokens - final_tokens
  diagnostics$token_stats$vocabulary_size <- final_terms
  
  # Create core data result
  result_data <- list(
    dfm = dfm_object,
    metadata = stm_metadata,
    tokens = processed_text,
    stm_data = list(
      documents = stm_docs$documents,
      vocab = stm_docs$vocab,
      meta = stm_metadata
    )
  )
  
  # If using segmentation, add segment mapping to result
  if (use_segmentation) {
    # Add segment mapping to stm_data
    result_data$stm_data$segment_mapping <- segment_mapping
  }
  
  # If using segmentation, add segment mapping in a standardized format
  if (use_segmentation) {
    # Create a clean mapping data frame
    segment_doc_mapping <- data.frame(
      segment_id = segment_mapping$segment_id,
      document_id = segment_mapping$original_doc_id,
      segment_num = segment_mapping$segment_num,
      segment_size = segment_mapping$segment_size,
      stringsAsFactors = FALSE
    )
    
    # Add to the stm_data
    stm_data$segment_mapping <- segment_doc_mapping
    
    # Also store in result_data directly
    result_data$segments$doc_mapping <- segment_doc_mapping
  }
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    original_docs = original_doc_count,
    final_docs = final_docs,
    segmentation_used = use_segmentation,
    segmentation_size = segmentation,
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