#' @title Process Documents for Structural Topic Modeling
#' @description Creates a document-feature matrix (DFM) from extracted PDFs using STM.
#'   Includes automatic segmentation for small corpora and configurable stopword filtering.
#'   
#' @param docs Result from extract_pdfs() containing document text data
#' @param metadata Result from add_metadata() containing document metadata  
#' @param remove_stopwords Whether to remove standard stopwords (default: TRUE)
#' @param stem Whether to stem words (default: TRUE)
#' @param min_docs Minimum proportion of documents a term must appear in (default: 0.02)
#' @param max_docs Maximum proportion of documents a term can appear in (default: 0.90)
#' @param segment_threshold Document count below which segmentation is applied (default: 100)
#' @param use_geo_stopwords Whether to remove geographic terms (default: TRUE)
#' @param custom_stopwords Additional terms to remove (default: NULL)
#'
#' @return A list containing STM-ready documents, vocabulary, and metadata
#'
#' @examples
#' \dontrun{
#' # Standard processing
#' dfm <- auto_cache(process_dfm, docs, metadata)
#' 
#' # With custom stopwords for adaptation terms
#' dfm <- auto_cache(process_dfm, docs, metadata, 
#'                   custom_stopwords = c("adaptation", "climate", "plan"))
#' }
process_dfm <- function(
    docs,
    metadata,
    remove_stopwords = TRUE,
    stem = TRUE,
    min_docs = 0.02,         
    max_docs = 0.90,
    segment_threshold = 100,
    use_geo_stopwords = TRUE,
    custom_stopwords = NULL
) {
  start_time <- Sys.time()
  
  ## --- Validation & Extraction -----------------------------------------------
  # Validate inputs and extract data
  if (!"tokens" %in% names(docs$data) || !"text" %in% names(docs$data$tokens)) {
    stop("docs must be from extract_pdfs() with tokens$text component")
  }
  if (!"metadata" %in% names(metadata$data)) {
    stop("metadata must be from add_metadata() with metadata component")
  }
  
  text_data <- docs$data$tokens$text
  meta_data <- metadata$data$metadata
  
  # Validate alignment
  if (length(text_data) != nrow(meta_data)) {
    stop("Mismatched data: ", length(text_data), " texts vs ", nrow(meta_data), " metadata rows")
  }
  
  log_message(paste("Processing", length(text_data), "documents"), "process_dfm")
  
  ## --- Segmentation (if needed) -----------------------------------------------
  segment_info <- list(used_segmentation = FALSE)
  
  if (length(text_data) < segment_threshold) {
    log_message("Segmenting documents", "process_dfm")
    
    segmentation_result <- segment_texts(text_data, meta_data, target_segments = segment_threshold * 2)
    text_data <- segmentation_result$texts
    meta_data <- segmentation_result$metadata
    segment_info <- segmentation_result$segment_info
    
    log_message(paste("Created", length(text_data), "segments"), "process_dfm")
  }
  
  ## --- Pre-processing Statistics ----------------------------------------------
  corpus_stats <- calculate_corpus_stats(text_data)
  final_doc_count <- length(text_data)
  
  # Calculate thresholds
  min_docs_absolute <- max(1, ceiling(final_doc_count * min_docs))
  max_docs_absolute <- max(min_docs_absolute + 1, floor(final_doc_count * max_docs))
  
  # Build stopwords
  custom_stopwords_list <- build_custom_stopwords(use_geo_stopwords, custom_stopwords)
  
  log_message(paste(length(custom_stopwords_list), "custom stopwords removed"), "process_dfm")
  
  ## --- STM Processing ---------------------------------------------------------
  # Text processing
  processed <- stm::textProcessor(
    documents = text_data,
    metadata = meta_data,
    lowercase = TRUE,
    removestopwords = remove_stopwords,
    removenumbers = TRUE,
    removepunctuation = TRUE,
    stem = stem,
    wordLengths = c(3, Inf),
    customstopwords = custom_stopwords_list,
    verbose = FALSE
  )
  
  # Document preparation with thresholds
  prepped <- stm::prepDocuments(
    documents = processed$documents,
    vocab = processed$vocab,
    meta = processed$meta,
    lower.thresh = min_docs_absolute,
    upper.thresh = max_docs_absolute,
    verbose = FALSE
  )
  
  # Final statistics
  final_token_count <- sum(sapply(prepped$documents, function(doc) sum(doc[2, ])))
  
  log_message(paste("Final:", length(prepped$documents), "documents of", final_doc_count, ",",
                    length(prepped$vocab), "terms of", corpus_stats$vocab_size, ",",
                    final_token_count, "tokens of", corpus_stats$token_count), "process_dfm")
  
  ## --- Return Result ----------------------------------------------------------
  return(create_result(
    data = list(
      documents = prepped$documents,
      vocab = prepped$vocab,
      meta = prepped$meta
    ),
    metadata = list(
      timestamp = start_time,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      # Corpus progression
      input_documents = final_doc_count,
      input_vocabulary = corpus_stats$vocab_size,
      input_tokens = corpus_stats$token_count,
      final_documents = length(prepped$documents),
      final_vocabulary = length(prepped$vocab),
      final_tokens = final_token_count,
      # Processing parameters
      thresholds = list(
        min_docs_prop = min_docs,
        max_docs_prop = max_docs,
        min_docs_abs = min_docs_absolute,
        max_docs_abs = max_docs_absolute
      ),
      stopwords = list(
        use_geo = use_geo_stopwords,
        custom_count = length(custom_stopwords_list),
        total_removed = length(custom_stopwords_list)
      ),
      segmentation = segment_info,
      success = TRUE
    ),
    diagnostics = list()
  ))
}

## --- Helper Functions -------------------------------------------------------

#' Calculate corpus statistics efficiently
calculate_corpus_stats <- function(text_data) {
  # Tokenize all documents at once
  all_tokens <- unlist(lapply(text_data, function(x) unlist(strsplit(x, "\\s+"))))
  
  list(
    vocab_size = length(unique(all_tokens)),
    token_count = length(all_tokens)
  )
}

#' Build comprehensive stopwords list
build_custom_stopwords <- function(use_geo_stopwords = TRUE, custom_stopwords = NULL) {
  stopwords_list <- character(0)
  
  # Add geographic terms
  if (use_geo_stopwords) {
    geographic_terms <- extract_geographic_terms()
    stopwords_list <- c(stopwords_list, geographic_terms)
  }
  
  # Add custom terms
  if (!is.null(custom_stopwords)) {
    stopwords_list <- c(stopwords_list, tolower(custom_stopwords))
  }
  
  unique(stopwords_list)
}

#' Extract geographic terms from country names
extract_geographic_terms <- function() {
  country_data <- countrycode::codelist
  
  # Just get English names and ISO codes
  country_names <- c(
    country_data$country.name.en[!is.na(country_data$country.name.en)],
    country_data$iso3c[!is.na(country_data$iso3c)]
  )
  
  # Extract individual words
  all_words <- unlist(strsplit(country_names, "\\s+"))
  geographic_terms <- tolower(all_words)
  
  # Basic filtering: length >= 4 and only letters
  geographic_terms <- geographic_terms[nchar(geographic_terms) >= 4]
  geographic_terms <- geographic_terms[grepl("^[a-z]+$", geographic_terms)]
  
  # Remove overly common terms
  common_terms <- c("united", "republic", "democratic", "kingdom", "island", "islands", "states")
  geographic_terms <- geographic_terms[!geographic_terms %in% common_terms]
  
  unique(geographic_terms)
}

#' Segment texts into smaller chunks for better STM performance
segment_texts <- function(text_data, meta_data, target_segments = 200) {
  
  # Calculate optimal segment length
  total_tokens <- sum(sapply(text_data, function(x) length(unlist(strsplit(x, "\\s+")))))
  segment_length <- max(100, ceiling(total_tokens / target_segments))
  
  # Pre-allocate for efficiency
  n_docs <- length(text_data)
  estimated_segments <- ceiling(target_segments * 1.2)
  
  segmented_texts <- character(estimated_segments)
  segment_to_doc_map <- character(estimated_segments)
  segment_meta_indices <- integer(estimated_segments)
  
  segment_count <- 0
  
  # Process each document
  for (i in seq_len(n_docs)) {
    doc_tokens <- unlist(strsplit(text_data[i], "\\s+"))
    
    if (length(doc_tokens) < 50) {
      # Keep short documents whole
      segment_count <- segment_count + 1
      segmented_texts[segment_count] <- text_data[i]
      segment_to_doc_map[segment_count] <- meta_data$doc_id[i]
      segment_meta_indices[segment_count] <- i
    } else {
      # Split into segments
      n_segments <- ceiling(length(doc_tokens) / segment_length)
      
      for (seg in seq_len(n_segments)) {
        start_idx <- (seg - 1) * segment_length + 1
        end_idx <- min(seg * segment_length, length(doc_tokens))
        
        segment_count <- segment_count + 1
        segmented_texts[segment_count] <- paste(doc_tokens[start_idx:end_idx], collapse = " ")
        segment_to_doc_map[segment_count] <- meta_data$doc_id[i]
        segment_meta_indices[segment_count] <- i
      }
    }
  }
  
  # Trim to actual size and replicate metadata
  segmented_texts <- segmented_texts[seq_len(segment_count)]
  segment_to_doc_map <- segment_to_doc_map[seq_len(segment_count)]
  segment_meta_indices <- segment_meta_indices[seq_len(segment_count)]
  replicated_metadata <- meta_data[segment_meta_indices, ]
  
  return(list(
    texts = segmented_texts,
    metadata = replicated_metadata,
    segment_info = list(
      used_segmentation = TRUE,
      segment_to_doc_map = segment_to_doc_map,
      original_doc_count = n_docs,
      segment_count = segment_count,
      avg_segment_length = segment_length
    )
  ))
}