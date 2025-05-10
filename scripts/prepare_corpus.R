prepare_corpus <- function(
    text_data, 
    text_column = "pdf_text",
    whitelist = NULL,           # Additional terms to always include
    geo_stops = TRUE,           # Remove geographic names
    lemma = TRUE,               # Lemmatize before dictionary check
    stem = FALSE,               # Stem after dictionary check (overrides lemma)
    min_word_length = 3,
    min_prop = 0.02,           # Minimum document proportion
    max_prop = 0.7,            # Maximum document proportion  
    prop_length = 50,          # Minimum tokens per document
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
  
  # Use standard validation function
  validate_input(text_data, required_cols = c(text_column), func_name = "prepare_corpus")
  
  # Additional parameter validation
  tryCatch({
    if (!is.numeric(min_prop) || min_prop < 0 || min_prop > 1) {
      stop("min_prop must be a value between 0 and 1")
    }
    
    if (!is.numeric(max_prop) || max_prop < 0 || max_prop > 1) {
      stop("max_prop must be a value between 0 and 1")
    }
    
    if (min_prop >= max_prop) {
      stop("min_prop must be less than max_prop")
    }
    
    if (stem && lemma) {
      log_message("Both stem and lemma are TRUE. Stemming will override lemmatization.", 
                  "prepare_corpus", "WARNING")
    }
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "prepare_corpus", "ERROR")
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, e$message)
    stop(e$message)
  })
  
  # Record original document count
  original_doc_count <- nrow(text_data)
  diagnostics$token_stats$original_docs <- original_doc_count
  
  log_message(paste("Processing", original_doc_count, "documents"), "prepare_corpus")
  
  ## --- Create sequential doc_id -----------------------------------------------
  log_message("Creating document identifiers", "prepare_corpus")
  
  # Create sequential doc_id
  text_data$doc_id <- as.character(1:nrow(text_data))
  
  ## --- Create English dictionary ----------------------------------------------
  log_message("Creating comprehensive English dictionary (US + UK)", "prepare_corpus")
  
  english_dictionary <- time_operation({
    tryCatch({
      dict_words <- unique(c(
        # Lexicon dictionaries
        lexicon::grady_augmented,
        lexicon::function_words,
        
        # Add whitelist terms if provided
        whitelist
      ))
      
      # Convert to lowercase for consistent matching
      dict_words <- unique(tolower(dict_words))
      
      log_message(paste("Created dictionary with", length(dict_words), "unique words"), 
                  "prepare_corpus")
      dict_words
      
    }, error = function(e) {
      log_message(paste("Error creating full dictionary:", e$message), 
                  "prepare_corpus", "WARNING")
      # Fallback
      unique(tolower(lexicon::grady_augmented))
    })
  }, "prepare_corpus")
  
  ## --- Prepare corpus dataframe -----------------------------------------------
  corpus_data <- text_data
  names(corpus_data)[names(corpus_data) == text_column] <- "text"
  
  # Check for empty documents
  empty_docs <- which(is.na(corpus_data$text) | nchar(corpus_data$text) == 0)
  if (length(empty_docs) > 0) {
    log_message(paste("Removing", length(empty_docs), "empty documents"), "prepare_corpus")
    removed_rows <- empty_docs
    corpus_data <- corpus_data[-empty_docs, ]
    
    diagnostics$removed_documents$empty_count <- length(empty_docs)
    diagnostics$removed_documents$empty_doc_ids <- text_data$doc_id[empty_docs]
    # Also track country names if available
    if ("country_name" %in% names(text_data)) {
      diagnostics$removed_documents$empty_countries <- text_data$country_name[empty_docs]
    }
  }
  
  ## --- Tokenize documents -----------------------------------------------------
  log_message("Tokenizing documents", "prepare_corpus")
  
  tokenized_text <- time_operation({
    tryCatch({
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
  }, "prepare_corpus")
  
  # Track initial token count
  start_tokens <- nrow(tokenized_text)
  diagnostics$token_stats$initial_tokens <- start_tokens
  log_message(paste("Initial tokenization produced", start_tokens, "tokens"), "prepare_corpus")
  
  ## --- Apply lemmatization if requested (BEFORE dictionary check) -------------
  if (lemma && !stem) {
    log_message("Applying lemmatization", "prepare_corpus")
    
    tokenized_text <- time_operation({
      tokenized_text %>%
        dplyr::mutate(word = textstem::lemmatize_words(word))
    }, "prepare_corpus")
  }
  
  ## --- Token validation: Dictionary + Length ----------------------------------
  log_message("Validating tokens (dictionary check + word length)", "prepare_corpus")
  
  tokenized_text <- tokenized_text %>%
    dplyr::filter(
      word %in% english_dictionary,              # Must be in dictionary
      nchar(word) >= min_word_length             # Must meet length requirement
    )
  
  tokens_after_validation <- nrow(tokenized_text)
  log_message(paste("Token validation kept", tokens_after_validation, 
                    "of", start_tokens, "tokens"), "prepare_corpus")
  
  ## --- Geographic token removal (if enabled) ----------------------------------
  if (geo_stops) {
    log_message("Creating geographic stopwords (countries + cities)", "prepare_corpus")
    
    geographic_terms <- time_operation({
      tryCatch({
        # Get country names in multiple languages
        country_data <- countrycode::codelist
        country_names <- unique(c(
          country_data$country.name.en,   # English
          country_data$country.name.fr,   # French
          country_data$country.name.es,   # Spanish
          country_data$country.name.de    # Portuguese (using German as proxy)
        ))
        
        # Clean up NA values
        country_names <- country_names[!is.na(country_names)]
        
        # Add country names from our actual data (if available)
        if ("country_name" %in% names(text_data)) {
          country_names <- unique(c(country_names, text_data$country_name))
        }
        
        # Process country names
        geographic_tokens <- character()
        
        for (country in country_names) {
          # Split multi-word countries
          words <- unlist(strsplit(country, "\\s+"))
          
          for (word in words) {
            word_lower <- tolower(word)
            geographic_tokens <- c(
              geographic_tokens,
              word_lower,                    # "albania"
              paste0(word_lower, "'s"),      # "albania's"
              paste0(word_lower, "s"),       # "albanias" (just in case)
              paste0(word_lower, "'")        # "albania'" (apostrophe only)
            )
          }
        }
        
        # Add city names
        city_names <- maps::world.cities$name
        for (city in city_names) {
          city_lower <- tolower(city)
          geographic_tokens <- c(
            geographic_tokens,
            city_lower,                      # "paris"
            paste0(city_lower, "'s"),        # "paris's"
            paste0(city_lower, "s"),         # "pariss" (for consistency)
            paste0(city_lower, "'")          # "paris'"
          )
        }
        
        # Clean and deduplicate
        geographic_tokens <- unique(geographic_tokens)
        geographic_tokens <- geographic_tokens[geographic_tokens != ""]
        
        log_message(paste("Created", length(geographic_tokens), "geographic stopwords"), 
                    "prepare_corpus")
        
        geographic_tokens
        
      }, error = function(e) {
        log_message(paste("Error creating geographic stopwords:", e$message), 
                    "prepare_corpus", "WARNING")
        # Fallback to just using data's country names
        if ("country_name" %in% names(text_data)) {
          country_names <- text_data$country_name
          tokens <- character()
          for (country in country_names) {
            words <- unlist(strsplit(tolower(country), "\\s+"))
            tokens <- c(tokens, words, paste0(words, "'s"))
          }
          unique(tokens)
        } else {
          character(0)
        }
      })
    }, "prepare_corpus")
    
    # Remove geographic tokens
    tokens_before <- nrow(tokenized_text)
    tokenized_text <- tokenized_text %>%
      dplyr::filter(!word %in% geographic_terms)
    
    tokens_removed <- tokens_before - nrow(tokenized_text)
    log_message(paste("Removed", tokens_removed, "geographic tokens"), "prepare_corpus")
    
    diagnostics$token_stats$geographic_tokens_removed <- tokens_removed
  }
  
  ## --- Remove stopwords ------------------------------------------------------
  log_message("Removing stopwords", "prepare_corpus")
  
  tokenized_text <- tokenized_text %>%
    dplyr::anti_join(tidytext::get_stopwords(), by = "word")
  
  ## --- Apply stemming if requested (AFTER validation) ------------------------
  if (stem) {
    log_message("Applying stemming", "prepare_corpus")
    
    tokenized_text <- time_operation({
      tokenized_text %>%
        dplyr::mutate(word = SnowballC::wordStem(word))
    }, "prepare_corpus")
  }
  
  ## --- Filter words by document frequency ------------------------------------
  log_message("Filtering words by document frequency", "prepare_corpus")
  
  # Count documents
  doc_count <- length(unique(tokenized_text$doc_id))
  
  # Calculate minimum and maximum document counts from proportions
  min_doc_count <- ceiling(min_prop * doc_count)
  max_doc_count <- floor(max_prop * doc_count)
  
  log_message(paste("Words must appear in at least", min_doc_count, 
                    "documents and at most", max_doc_count, "documents"), 
              "prepare_corpus")
  
  # Count how many documents each word appears in
  word_doc_counts <- tokenized_text %>%
    dplyr::group_by(word) %>%
    dplyr::summarize(
      doc_count = dplyr::n_distinct(doc_id),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      doc_count >= min_doc_count,
      doc_count <= max_doc_count
    )
  
  # Apply frequency filter
  tokenized_text <- tokenized_text %>%
    dplyr::semi_join(word_doc_counts, by = "word")
  
  log_message(paste("Retained", nrow(word_doc_counts), 
                    "unique words after frequency filtering"), 
              "prepare_corpus")
  
  ## --- Filter documents by length ---------------------------------------------
  log_message("Filtering documents by length", "prepare_corpus")
  
  # Count tokens per document after all filtering
  doc_lengths <- tokenized_text %>% 
    dplyr::count(doc_id, name = "token_count")
  
  # Filter by minimum document length
  valid_docs <- doc_lengths %>%
    dplyr::filter(token_count >= prop_length)
  
  # Track removed documents
  removed_docs <- setdiff(doc_lengths$doc_id, valid_docs$doc_id)
  
  if (length(removed_docs) > 0) {
    log_message(paste("Removed", length(removed_docs), 
                      "documents with fewer than", prop_length, "tokens"),
                "prepare_corpus", "WARNING")
    
    diagnostics$removed_documents$short_count <- length(removed_docs)
    diagnostics$removed_documents$short_doc_ids <- removed_docs
    
    # Also track country names if available
    if ("country_name" %in% names(corpus_data)) {
      diagnostics$removed_documents$short_countries <- 
        corpus_data$country_name[corpus_data$doc_id %in% removed_docs]
    }
  }
  
  # Filter tokenized text to keep only documents meeting minimum length
  tokenized_text <- tokenized_text %>%
    dplyr::inner_join(valid_docs, by = "doc_id")
  
  ## --- Prepare final output ---------------------------------------------------
  # Extract metadata for remaining documents
  metadata_cols <- setdiff(names(text_data), c(text_column))
  
  if (length(metadata_cols) > 0) {
    doc_metadata <- text_data[text_data$doc_id %in% valid_docs$doc_id, metadata_cols]
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
  
  # Create core data result - including the dictionary
  result_data <- list(
    tokens = tokenized_text,       # Tokenized text data
    metadata = doc_metadata,       # Document metadata
    vocab = vocabulary,            # Vocabulary list
    dictionary = english_dictionary # Dictionary used for validation
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
      lemma = lemma,
      stem = stem,
      geo_stops = geo_stops,
      min_word_length = min_word_length,
      min_prop = min_prop,
      max_prop = max_prop,
      prop_length = prop_length,
      dict_size = length(english_dictionary)
    )
  )
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}