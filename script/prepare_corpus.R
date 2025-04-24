# Purpose: Process text documents for topic modeling

library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(tidytext)    # Text processing
library(stringr)     # String manipulation
library(textstem)    # For lemmetization
library(quanteda)    # DFM creation

prepare_corpus <- function(text_data, 
                       text_column = "pdf_text",
                       custom_stopwords = NULL, 
                       stem_words = FALSE,
                       lemmatize = TRUE,
                       min_word_count = 2,
                       min_word_length = 1,
                       remove_punctuation = TRUE,
                       min_doc_length = 50,
                       max_doc_proportion = 0.8,
                       return_stats = FALSE,
                       output_path = "data/corpus.rds") {
  
  # Create output directory if it doesn't exist
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  # Validate inputs
  if (!is.data.frame(text_data)) {
    stop("text_data must be a data frame")
  }
  
  if (!text_column %in% names(text_data)) {
    stop(paste("text_data must contain a", text_column, "column"))
  }
  
  if (!is.null(custom_stopwords) && !is.character(custom_stopwords)) {
    stop("custom_stopwords must be a character vector")
  }
  
  # Assign consistent document IDs first
  text_data <- text_data %>%
    mutate(doc_id = row_number())
  
  # Process text data
  cat("Tokenizing documents...\n")
  processed_text <- text_data %>%
    unnest_tokens(
      word, 
      !!sym(text_column),
      to_lower = TRUE,
      strip_punct = remove_punctuation
    ) %>%
    # Remove numbers
    filter(!str_detect(word, "^[0-9]+$"))
  
  # Filter by word length
  cat(paste("Filtering words with", min_word_length, "or fewer characters...\n"))
  processed_text <- processed_text %>%
    filter(str_length(word) > min_word_length) 
  
  # Track statistics
  start_tokens <- nrow(processed_text)
  start_docs <- n_distinct(processed_text$doc_id)
  
  cat("Removing stopwords...\n")
  # Remove standard stopwords
  processed_text <- processed_text %>%
    anti_join(get_stopwords(), by = "word")
  
  # Remove custom stopwords if provided
  if (!is.null(custom_stopwords) && length(custom_stopwords) > 0) {
    custom_stops <- tibble(word = custom_stopwords)
    processed_text <- processed_text %>%
      anti_join(custom_stops, by = "word")
  }
  
  # Apply lemmatization
  if (lemmatize) {
    cat("Applying lemmatization...\n")
    processed_text <- processed_text %>%
      mutate(word = textstem::lemmatize_words(word))
  }
  
  # Apply stemming
  if (stem_words) {
    cat("Applying word stemming...\n")
    processed_text <- processed_text %>%
      mutate(word = textstem::stem_words(word))
  }
  
  cat("Filtering words by frequency...\n")
  # Filter words by frequency
  word_doc_counts <- processed_text %>%
    group_by(word) %>%
    summarize(
      doc_count = n_distinct(doc_id),
      doc_prop = doc_count / start_docs,
      .groups = "drop"
    ) %>%
    filter(
      doc_count >= min_word_count,
      doc_prop <= max_doc_proportion
    )
  
  processed_text <- processed_text %>%
    semi_join(word_doc_counts, by = "word")
  
  # Count words per document and filter out short documents
  doc_lengths <- processed_text %>% 
    count(doc_id) %>%
    filter(n >= min_doc_length)
  
  # Track removed documents with country information
  removed_doc_ids <- setdiff(unique(processed_text$doc_id), doc_lengths$doc_id)
  
  if (length(removed_doc_ids) > 0) {
    cat(paste("Removed", length(removed_doc_ids), 
              "documents with fewer than", min_doc_length, "tokens\n"))
  } else {
    cat("No documents were removed based on length criteria\n")
  }
  
  # Filter to keep only documents meeting the minimum length
  processed_text <- processed_text %>%
    semi_join(doc_lengths, by = "doc_id")
  
  # Extract metadata columns if they exist (grab all columns except text_column)
  metadata_cols <- setdiff(names(text_data), text_column)
  
  # Merge with metadata if available
  if (length(metadata_cols) > 0) {
    metadata <- text_data %>%
      select(doc_id, all_of(metadata_cols)) %>%
      distinct()
    
    # Only keep metadata for documents that weren't filtered out
    metadata <- metadata %>%
      semi_join(doc_lengths, by = "doc_id")
  } else {
    metadata <- tibble(doc_id = unique(processed_text$doc_id))
  }
  
  cat("Creating document-feature matrix...\n")
  
  # Create word counts
  word_counts <- processed_text %>%
    count(doc_id, word)
  
  # Create document-feature matrix
  dfm_object <- word_counts %>%
    cast_dfm(doc_id, word, n)
  
  # Prepare result object
  result <- list(
    dfm = dfm_object,
    metadata = metadata,
    tokens = processed_text
  )
  
  # Add statistics if requested
  if (return_stats) {
    result$stats <- list(
      documents = list(
        initial = start_docs,
        final = n_distinct(processed_text$doc_id),
        removed = start_docs - n_distinct(processed_text$doc_id)
      ),
      tokens = list(
        initial = start_tokens,
        final = nrow(processed_text),
        removed = start_tokens - nrow(processed_text)
      ),
      vocabulary = list(
        size = n_distinct(processed_text$word),
        unique_terms = n_distinct(word_counts$word)
      )
    )
  }
  
  # Convert to STM format for direct use in topic modeling
  cat("Converting to STM format...\n")
  stm_docs <- convert(dfm_object, to = "stm")
  result$stm_data <- list(
    documents = stm_docs$documents,
    vocab = stm_docs$vocab
  )
  
  # Save the result
  cat("Saving processed corpus to", output_path, "...\n")
  saveRDS(result, output_path)
  
  cat("Preprocessing complete!\n")
  return(result)
}