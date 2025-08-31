# Process Documents for Structural Topic Modeling
process_dfm <- function(
    docs,
    metadata,
    remove_stopwords = TRUE,
    stem = TRUE,
    min_docs = 0.1,         
    max_docs = 0.8,
    segment = TRUE,
    target_segments = 200,
    use_geo_stopwords = TRUE,
    custom_stopwords = NULL
) {
  start_time <- Sys.time()
  
  log_message("Processing documents for STM", "process_dfm")
  
  # Extract data from tibble structure
  docs_tibble <- docs$data$tokens
  meta_data <- metadata$data$metadata
  
  ## --- Match by doc_id ---------------------------------------------------------
  # Get doc_ids from both datasets  
  text_doc_ids <- docs_tibble$doc_id
  meta_doc_ids <- meta_data$doc_id
  
  # Find common doc_ids
  common_doc_ids <- intersect(text_doc_ids, meta_doc_ids)
  
  # Filter both datasets to matched doc_ids
  matched_docs <- docs_tibble[docs_tibble$doc_id %in% common_doc_ids, ]
  matched_meta <- meta_data[meta_data$doc_id %in% common_doc_ids, ]
  
  # Ensure same order
  matched_docs <- matched_docs[match(common_doc_ids, matched_docs$doc_id), ]
  matched_meta <- matched_meta[match(common_doc_ids, matched_meta$doc_id), ]
  
  # Extract text data for STM processing
  text_data <- matched_docs$text
  names(text_data) <- matched_docs$doc_id
  meta_data <- matched_meta
  
  log_message(paste("Matched", length(common_doc_ids), "documents by doc_id"), "process_dfm")
  
  ## --- Segmentation (if needed) -----------------------------------------------
  segment_info <- list(used_segmentation = FALSE)
  
  if (segment) {
    log_message("Segmenting documents", "process_dfm")
    
    # Calculate optimal segment length to achieve target number of segments
    total_tokens <- sum(sapply(text_data, function(x) length(unlist(strsplit(x, "\\s+")))))
    segment_length <- max(100, ceiling(total_tokens / target_segments))
    
    log_message(paste("Target segments:", target_segments, "with length ~", segment_length, "words"), "process_dfm")
    
    # Segment documents
    segmented_texts <- character()
    segment_doc_ids <- character()
    
    for (i in seq_along(text_data)) {
      doc_tokens <- unlist(strsplit(text_data[i], "\\s+"))
      doc_id <- names(text_data)[i]
      
      if (length(doc_tokens) < 50) {
        # Keep short documents whole
        segmented_texts <- c(segmented_texts, text_data[i])
        segment_doc_ids <- c(segment_doc_ids, doc_id)
      } else {
        # Split into segments
        n_segments <- ceiling(length(doc_tokens) / segment_length)
        
        for (seg in 1:n_segments) {
          start_idx <- (seg - 1) * segment_length + 1
          end_idx <- min(seg * segment_length, length(doc_tokens))
          
          segment_text <- paste(doc_tokens[start_idx:end_idx], collapse = " ")
          segmented_texts <- c(segmented_texts, segment_text)
          segment_doc_ids <- c(segment_doc_ids, doc_id)
        }
      }
    }
    
    # Update data with segments
    text_data <- segmented_texts
    names(text_data) <- segment_doc_ids
    
    # Replicate metadata for segments (match by doc_id)
    meta_data <- meta_data[match(segment_doc_ids, meta_data$doc_id), ]
    
    segment_info <- list(
      used_segmentation = TRUE,
      original_doc_count = length(common_doc_ids),
      segment_count = length(segmented_texts),
      avg_segment_length = segment_length
    )
    
    log_message(paste("Created", length(text_data), "segments"), "process_dfm")
  }
  
  ## --- Corpus Statistics (for methods section) -------------------------------
  all_tokens <- unlist(lapply(text_data, function(x) unlist(strsplit(x, "\\s+"))))
  corpus_stats <- list(
    vocab_size = length(unique(all_tokens)),
    token_count = length(all_tokens)
  )
  
  final_doc_count <- length(text_data)
  
  # Calculate thresholds
  min_docs_absolute <- max(1, ceiling(final_doc_count * min_docs))
  max_docs_absolute <- max(min_docs_absolute + 1, floor(final_doc_count * max_docs))
  
  ## --- Build stopwords (inline) -----------------------------------------------
  stopwords_list <- character(0)
  
  # Geographic stopwords (essential to prevent country name topics)
  if (use_geo_stopwords) {
    country_data <- countrycode::codelist
    country_names <- country_data$country.name.en[!is.na(country_data$country.name.en)]
    all_words <- unlist(strsplit(country_names, "\\s+"))
    geographic_terms <- tolower(all_words)
    geographic_terms <- geographic_terms[nchar(geographic_terms) >= 4]
    geographic_terms <- geographic_terms[grepl("^[a-z]+$", geographic_terms)]
    common_terms <- c("united", "republic", "democratic", "kingdom", "island", "islands", "states")
    geographic_terms <- geographic_terms[!geographic_terms %in% common_terms]
    stopwords_list <- c(stopwords_list, unique(geographic_terms))
  }
  
  # Custom stopwords
  if (!is.null(custom_stopwords)) {
    stopwords_list <- c(stopwords_list, tolower(custom_stopwords))
  }
  
  stopwords_list <- unique(stopwords_list)
  
  log_message(paste("Using", length(stopwords_list), "custom stopwords"), "process_dfm")
  
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
    customstopwords = stopwords_list,
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
  
  log_message(paste("Final:", length(prepped$documents), "documents,", 
                    length(prepped$vocab), "terms,", final_token_count, "tokens"), "process_dfm")
  
  ## --- Return Result ----------------------------------------------------------
  return(create_result(
    data = list(
      documents = prepped$documents,
      vocab = prepped$vocab,
      meta = prepped$meta
    ),
    metadata = list(
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      # Corpus stats (for methods section)
      input_documents = final_doc_count,
      input_vocabulary = corpus_stats$vocab_size,
      input_tokens = corpus_stats$token_count,
      final_documents = length(prepped$documents),
      final_vocabulary = length(prepped$vocab),
      final_tokens = final_token_count,
      # Processing parameters
      min_docs_absolute = min_docs_absolute,
      max_docs_absolute = max_docs_absolute,
      custom_stopwords_count = length(stopwords_list),
      segmentation = segment_info
    )
  ))
}