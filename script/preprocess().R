
# Load required packages
library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(stringr)     # Data stringing
library(tidytext)    # Text processing
library(SnowballC)   # Word stemming
library(napr)        # NAP data

preprocess <- function(nap_data, 
                           custom_stopwords = NULL, 
                           stem_words = FALSE) {
  
  # ---- Document preparation ----
  cat("Starting preprocessing of", nrow(nap_data), "NAP documents\n")
  
  # Select only the columns we need and adding document ID
  nap_data <- nap_data %>% 
    select(country_name, pdf_text, date_posted)

  nap_data <- nap_data %>% 
    mutate(doc_id = row_number())
  
  cat("Added document IDs\n")
  
  # ---- Text tokenization and cleaning ----
  cat("Tokenizing documents...\n")
  processed_text <- nap_data %>%
    unnest_tokens(word, pdf_text)
  
  cat("Removing stopwords and numbers...\n")
  processed_text <- processed_text %>%
    anti_join(get_stopwords(), by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$"))
  
  # Remove custom stopwords if provided
  if (!is.null(custom_stopwords)) {
    cat("Removing", length(custom_stopwords), "custom stopwords...\n")
    custom_stops <- tibble(word = custom_stopwords)
    processed_text <- processed_text %>%
      anti_join(custom_stops, by = "word")
  }
  
  # Apply stemming if requested
  if (stem_words) {
    cat("Stemming words...\n")
    processed_text <- processed_text %>%
      mutate(word = wordStem(word))
  }
  
  # Remove documents with too few tokens
  min_tokens <- 10
  
  doc_lengths <- processed_text %>%
    count(doc_id)
  
  short_docs <- doc_lengths %>%
    filter(n < min_tokens) %>%
    left_join(processed_text %>% distinct(doc_id, country_name), by = "doc_id")
  
  if (nrow(short_docs) > 0) {
    cat("Removing", nrow(short_docs), "document(s) with too few tokens:\n")
    short_docs %>%
      mutate(msg = paste0("- doc_id ", doc_id, " (", country_name, "), ", n, " tokens")) %>%
      pull(msg) %>%
      cat(sep = "\n")
    
    processed_text <- processed_text %>%
      filter(!(doc_id %in% short_docs$doc_id))
  }
  
  
  # Return the processed text data
  cat("Preprocessing complete!\n")
  return(processed_text)
}