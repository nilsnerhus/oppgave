# =========================================================================
# NAP PREPROCESSING FUNCTION
# =========================================================================
# Purpose: Process National Adaptation Plan documents for topic modeling

# Load required packages
library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(tidytext)    # Text processing
library(stringr)     # String manipulation
library(SnowballC)   # Word stemming
library(quanteda)    # For dfm creation
library(napr)        # For data

preprocess <- function(nap_data, 
                       custom_stopwords = NULL, 
                       stem_words = FALSE,
                       remove_punctuation = TRUE,
                       min_doc_length = 50,
                       min_word_count = 2,
                       max_doc_proportion = 0.8,
                       return_stats = FALSE) {
  
  # Validate inputs
  if (!is.data.frame(nap_data)) {
    stop("nap_data must be a data frame")
  }
  
  if (!"pdf_text" %in% names(nap_data)) {
    stop("NAP data must contain a 'pdf_text' column")
  }
  
  if (!is.null(custom_stopwords) && !is.character(custom_stopwords)) {
    stop("custom_stopwords must be a character vector")
  }
  
  # Assign consistent document IDs first
  nap_data <- nap_data %>%
    mutate(doc_id = row_number())
  
  # Process NAP text data
  processed_text <- nap_data %>%
    unnest_tokens(
      word, 
      pdf_text,
      to_lower = TRUE,
      strip_punct = remove_punctuation
    ) %>%
    # Remove numbers
    filter(!str_detect(word, "^[0-9]+$"))
  
  # Track statistics
  start_tokens <- nrow(processed_text)
  start_docs <- n_distinct(processed_text$doc_id)
  
  # Remove standard stopwords
  processed_text <- processed_text %>%
    anti_join(get_stopwords(), by = "word")
  
  # Remove custom stopwords if provided
  if (!is.null(custom_stopwords) && length(custom_stopwords) > 0) {
    custom_stops <- tibble(word = custom_stopwords)
    processed_text <- processed_text %>%
      anti_join(custom_stops, by = "word")
  }
  
  # Apply stemming if requested
  if (stem_words) {
    processed_text <- processed_text %>%
      mutate(word = wordStem(word))
  }
  
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
    # Get country names for removed documents
    removed_countries <- nap_data %>%
      filter(doc_id %in% removed_doc_ids) %>%
      select(doc_id, country_name) %>% # Adjust column name if needed
      distinct()
    
    # Print message about removed documents
    cat(paste("Removed", length(removed_doc_ids), 
              "documents with fewer than", min_doc_length, "tokens\n"))
    
    # Print information about which countries were removed
    countries_list <- paste(removed_countries$country_name, collapse = ", ")
    cat(paste("Countries removed:", countries_list, "\n"))
  } else {
    removed_countries <- tibble(doc_id = integer(), country_name = character())
    cat("No documents were removed based on length criteria\n")
  }
  
  # Filter to keep only documents meeting the minimum length
  processed_text <- processed_text %>%
    semi_join(doc_lengths, by = "doc_id")
  
  # Extract metadata columns if they exist
  metadata_cols <- intersect(
    c("country_name", "region", "ldc_sids_marker", "date_posted", "Income", "Geography", "Country"),
    names(nap_data)
  )
  
  # Merge with metadata if available
  if (length(metadata_cols) > 0) {
    metadata <- nap_data %>%
      select(doc_id, all_of(metadata_cols)) %>%
      distinct()
    
    # Only keep metadata for documents that weren't filtered out
    metadata <- metadata %>%
      semi_join(doc_lengths, by = "doc_id")
  } else {
    metadata <- tibble(doc_id = unique(processed_text$doc_id))
  }
  
  cat("Creating document-term matrix from processed tokens...\n")
  
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
    processed_tokens = processed_text
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
  
  # Add class for method dispatch in subsequent functions
  class(result) <- c("nap_processed", "list")
  
  return(result)
}