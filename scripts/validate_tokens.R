validate_tokens <- function(
    text_data,         # Data frame with doc_id and text columns
    text_column = "text",
    validation_config = list(
      whitelist = NULL,        # Custom terms to always include
      geo_stops = TRUE,        # Whether to remove geographic terms
      min_word_length = 3      # Minimum word length to consider
    )
) {
  ## --- Setup & Initialization -------------------------------------------------
  log_message("Initializing minimal text validation", "validate_tokens")
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    removed_documents = list(),
    processing_issues = character(),
    token_stats = list()
  )
  
  ## --- Create English dictionary ----------------------------------------------
  log_message("Creating English dictionary", "validate_tokens")
  
  english_dictionary <- unique(c(
    # Core dictionary 
    lexicon::grady_augmented,
    # Add whitelist terms if provided
    validation_config$whitelist
  ))
  
  # Convert to lowercase for consistent matching
  english_dictionary <- unique(tolower(english_dictionary))
  
  ## --- Create geographic stopwords (if enabled) -------------------------------
  geographic_terms <- character(0)
  
  if (validation_config$geo_stops) {
    log_message("Creating geographic stopwords", "validate_tokens")
    
    # Get country names from countrycode package
    country_data <- countrycode::codelist
    country_names <- country_data$country.name.en
    country_names <- country_names[!is.na(country_names)]
    
    # Process country names into individual terms
    for (country in country_names) {
      words <- unlist(strsplit(country, "\\s+"))
      
      for (word in words) {
        word_lower <- tolower(word)
        if (nchar(word_lower) >= validation_config$min_word_length) {
          geographic_terms <- c(geographic_terms, word_lower)
        }
      }
    }
    
    geographic_terms <- unique(geographic_terms)
  }
  
  ## --- Process each document --------------------------------------------------
  log_message("Processing documents with minimal validation", "validate_tokens")
  
  result <- text_data
  
  # For each document, perform minimal validation
  for (i in 1:nrow(result)) {
    if (!is.na(result[[text_column]][i])) {
      # Get document text
      text <- result[[text_column]][i]
      
      # Apply minimal processing (just for dictionary check)
      words <- unlist(strsplit(tolower(text), "\\W+"))
      words <- words[words != ""]
      words <- words[nchar(words) >= validation_config$min_word_length]
      
      # Check against dictionary and remove geo-terms
      valid_words <- words[words %in% english_dictionary]
      if (validation_config$geo_stops) {
        valid_words <- valid_words[!valid_words %in% geographic_terms]
      }
      
      # Reconstruct clean text
      clean_text <- paste(valid_words, collapse = " ")
      result[[text_column]][i] <- clean_text
    }
  }
  
  ## --- Return clean documents -------------------------------------------------
  log_message("Validation complete", "validate_tokens")
  
  # Return standardized result
  return(create_result(
    data = list(
      documents = result,
      valid_words = english_dictionary[!english_dictionary %in% geographic_terms]
    ),
    metadata = list(
      document_count = nrow(result),
      dictionary_size = length(english_dictionary),
      geo_terms_removed = length(geographic_terms)
    ),
    diagnostics = diagnostics
  ))
}