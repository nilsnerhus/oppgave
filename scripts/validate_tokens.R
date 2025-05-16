#' @title Validate tokens for topic modeling
#' @description Processes text data into validated tokens suitable for topic modeling
#'   by applying tokenization, dictionary validation, stopword removal, and frequency
#'   filtering. Handles document filtering based on token counts.
#'
#' @param tokens_data A dataframe containing doc_id and text columns from extract_pdfs
#' @param text_column Column name containing text to process (default: "text")
#' @param validation_config List with validation options:
#'   \itemize{
#'     \item whitelist - Additional terms to always include (default: NULL)
#'     \item geo_stops - Whether to remove geographic names (default: TRUE)
#'     \item use_lemma - Whether to lemmatize words (default: TRUE)
#'     \item min_word_length - Minimum word length to keep (default: 3)
#'     \item min_doc_freq - Minimum document frequency proportion (default: 0.02)
#'     \item max_doc_freq - Maximum document frequency proportion (default: 0.7)
#'     \item min_doc_tokens - Minimum tokens per document (default: 50)
#'   }
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item tokens - Processed tokens dataframe with doc_id and word columns
#'       \item vocab - List of unique terms in corpus
#'       \item doc_metadata - Document-level statistics
#'     }
#'   }
#'   \item{metadata}{Processing statistics and timestamps}
#'   \item{diagnostics}{Token statistics and processing issues}
#'
#' @examples
#' \dontrun{
#' # First scrape the website and extract PDF content
#' web_data <- scrape_web()
#' pdf_data <- extract_pdfs(web_data$data$tokens)
#' 
#' # Then validate and process tokens
#' tokens <- validate_tokens(pdf_data$data$tokens)
#' }
validate_tokens <- function(
    tokens_data, 
    text_column = "text",
    validation_config = list(
      whitelist = NULL,
      geo_stops = TRUE,
      use_lemma = TRUE,
      min_word_length = 3,
      min_doc_freq = 0.02,
      max_doc_freq = 0.7,
      min_doc_tokens = 50
    )
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    removed_documents = list(),
    processing_issues = character(),
    token_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "validate_tokens")
  
  # Validate tokens_data is a data frame
  if (!is.data.frame(tokens_data)) {
    error_msg <- "Input must be a dataframe"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "validate_tokens", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL,
        vocab = NULL,
        doc_metadata = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check required columns
  required_cols <- c("doc_id", text_column)
  missing_cols <- setdiff(required_cols, names(tokens_data))
  if (length(missing_cols) > 0) {
    error_msg <- paste("Input is missing required columns:", paste(missing_cols, collapse = ", "))
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "validate_tokens", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL,
        vocab = NULL,
        doc_metadata = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check for empty dataframe
  if (nrow(tokens_data) == 0) {
    error_msg <- "Input dataframe has no rows"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "validate_tokens", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL,
        vocab = NULL,
        doc_metadata = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate validation_config
  if (!is.list(validation_config)) {
    error_msg <- "validation_config must be a list"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "validate_tokens", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL,
        vocab = NULL,
        doc_metadata = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Set defaults for any missing config options
  validation_config$whitelist <- validation_config$whitelist %||% NULL
  validation_config$geo_stops <- if(is.null(validation_config$geo_stops)) TRUE else validation_config$geo_stops
  validation_config$use_lemma <- if(is.null(validation_config$use_lemma)) TRUE else validation_config$use_lemma
  validation_config$min_word_length <- validation_config$min_word_length %||% 3
  validation_config$min_doc_freq <- validation_config$min_doc_freq %||% 0.02
  validation_config$max_doc_freq <- validation_config$max_doc_freq %||% 0.7
  validation_config$min_doc_tokens <- validation_config$min_doc_tokens %||% 50
  
  # Record original document count
  original_doc_count <- nrow(tokens_data)
  diagnostics$token_stats$original_docs <- original_doc_count
  
  log_message(paste("Processing", original_doc_count, "documents"), "validate_tokens")
  
  ## --- Create English dictionary ----------------------------------------------
  log_message("Creating English dictionary", "validate_tokens")
  
  english_dictionary <- tryCatch({
    # Use a simplified dictionary approach with just one main source
    dict_words <- unique(c(
      # Core dictionary from lexicon package
      lexicon::grady_augmented,
      
      # Add whitelist terms if provided
      validation_config$whitelist
    ))
    
    # Convert to lowercase for consistent matching
    dict_words <- unique(tolower(dict_words))
    
    log_message(paste("Created dictionary with", length(dict_words), "unique words"), 
                "validate_tokens")
    dict_words
    
  }, error = function(e) {
    log_message(paste("Error creating dictionary:", e$message), 
                "validate_tokens", "WARNING")
    # Fallback to basic dictionary
    basic_dict <- unique(tolower(lexicon::grady_augmented))
    log_message(paste("Using fallback dictionary with", length(basic_dict), "words"), 
                "validate_tokens", "WARNING")
    basic_dict
  })
  
  ## --- Tokenize documents -----------------------------------------------------
  log_message("Tokenizing documents", "validate_tokens")
  
  # Prepare corpus dataframe
  corpus_data <- tokens_data
  names(corpus_data)[names(corpus_data) == text_column] <- "text"
  
  # Check for empty documents
  empty_docs <- which(is.na(corpus_data$text) | nchar(corpus_data$text) == 0)
  if (length(empty_docs) > 0) {
    log_message(paste("Removing", length(empty_docs), "empty documents"), "validate_tokens")
    corpus_data <- corpus_data[-empty_docs, ]
    
    diagnostics$removed_documents$empty_count <- length(empty_docs)
    diagnostics$removed_documents$empty_doc_ids <- tokens_data$doc_id[empty_docs]
  }
  
  # Tokenize text
  tokenized_text <- tryCatch({
    corpus_data %>%
      tidytext::unnest_tokens(
        word, 
        text,
        to_lower = TRUE,
        strip_punct = TRUE
      )
  }, error = function(e) {
    log_message(paste("Tokenization error:", e$message), "validate_tokens", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("Tokenization failed:", e$message))
    stop(e$message)
  })
  
  # Track initial token count
  start_tokens <- nrow(tokenized_text)
  diagnostics$token_stats$initial_tokens <- start_tokens
  log_message(paste("Initial tokenization produced", start_tokens, "tokens"), "validate_tokens")
  
  ## --- Apply lemmatization if requested ---------------------------------------
  if (validation_config$use_lemma) {
    log_message("Applying lemmatization", "validate_tokens")
    
    tokenized_text <- tryCatch({
      tokenized_text %>%
        dplyr::mutate(word = textstem::lemmatize_words(word))
    }, error = function(e) {
      log_message(paste("Lemmatization error:", e$message), "validate_tokens", "WARNING")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                         paste("Lemmatization skipped:", e$message))
      # Return unlemmatized text as fallback
      tokenized_text
    })
  }
  
  ## --- Token validation: Dictionary + Length ----------------------------------
  log_message("Validating tokens (dictionary check + word length)", "validate_tokens")
  
  tokenized_text <- tokenized_text %>%
    dplyr::filter(
      word %in% english_dictionary,              # Must be in dictionary
      nchar(word) >= validation_config$min_word_length  # Must meet length requirement
    )
  
  tokens_after_validation <- nrow(tokenized_text)
  log_message(paste("Token validation kept", tokens_after_validation, 
                    "of", start_tokens, "tokens"), "validate_tokens")
  
  ## --- Geographic token removal (if enabled) ----------------------------------
  if (validation_config$geo_stops) {
    log_message("Creating geographic stopwords", "validate_tokens")
    
    geographic_terms <- tryCatch({
      # Simplified approach: use just country names from countrycode package
      country_data <- countrycode::codelist
      country_names <- country_data$country.name.en
      
      # Clean up NA values
      country_names <- country_names[!is.na(country_names)]
      
      # Process country names
      geographic_tokens <- character()
      
      for (country in country_names) {
        # Split multi-word countries
        words <- unlist(strsplit(country, "\\s+"))
        
        for (word in words) {
          word_lower <- tolower(word)
          if (nchar(word_lower) >= validation_config$min_word_length) {
            geographic_tokens <- c(geographic_tokens, word_lower)
          }
        }
      }
      
      # Clean and deduplicate
      geographic_tokens <- unique(geographic_tokens)
      geographic_tokens <- geographic_tokens[geographic_tokens != ""]
      
      log_message(paste("Created", length(geographic_tokens), "geographic stopwords"), 
                  "validate_tokens")
      
      geographic_tokens
      
    }, error = function(e) {
      log_message(paste("Error creating geographic stopwords:", e$message), 
                  "validate_tokens", "WARNING")
      # Fallback to minimal list
      basic_geo <- c("africa", "america", "asia", "europe", "oceania", 
                     "north", "south", "east", "west", "central")
      log_message("Using fallback geographic stopwords", "validate_tokens")
      basic_geo
    })
    
    # Remove geographic tokens
    tokens_before <- nrow(tokenized_text)
    tokenized_text <- tokenized_text %>%
      dplyr::filter(!word %in% geographic_terms)
    
    tokens_removed <- tokens_before - nrow(tokenized_text)
    log_message(paste("Removed", tokens_removed, "geographic tokens"), "validate_tokens")
    
    diagnostics$token_stats$geographic_tokens_removed <- tokens_removed
  }
  
  ## --- Remove stopwords ------------------------------------------------------
  log_message("Removing stopwords", "validate_tokens")
  
  tokenized_text <- tokenized_text %>%
    dplyr::anti_join(tidytext::get_stopwords(), by = "word")
  
  ## --- Filter words by document frequency ------------------------------------
  log_message("Filtering words by document frequency", "validate_tokens")
  
  # Count documents
  doc_count <- length(unique(tokenized_text$doc_id))
  
  # Calculate minimum and maximum document counts from proportions
  min_doc_count <- ceiling(validation_config$min_doc_freq * doc_count)
  max_doc_count <- floor(validation_config$max_doc_freq * doc_count)
  
  log_message(paste("Words must appear in at least", min_doc_count, 
                    "documents and at most", max_doc_count, "documents"), 
              "validate_tokens")
  
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
              "validate_tokens")
  
  ## --- Filter documents by length ---------------------------------------------
  log_message("Filtering documents by length", "validate_tokens")
  
  # Count tokens per document after all filtering
  doc_lengths <- tokenized_text %>% 
    dplyr::count(doc_id, name = "token_count")
  
  # Filter by minimum document length
  valid_docs <- doc_lengths %>%
    dplyr::filter(token_count >= validation_config$min_doc_tokens)
  
  # Track removed documents
  removed_docs <- setdiff(doc_lengths$doc_id, valid_docs$doc_id)
  
  if (length(removed_docs) > 0) {
    log_message(paste("Removed", length(removed_docs), 
                      "documents with fewer than", validation_config$min_doc_tokens, "tokens"),
                "validate_tokens", "WARNING")
    
    diagnostics$removed_documents$short_count <- length(removed_docs)
    diagnostics$removed_documents$short_doc_ids <- removed_docs
  }
  
  tokenized_text <- tokenized_text %>%
    dplyr::inner_join(valid_docs, by = "doc_id")
  
  ## --- Prepare final output ---------------------------------------------------
  # Extract document metadata
  doc_metadata <- valid_docs
  
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
  
  # Create data result
  result_data <- list(
    tokens = tokenized_text,       # Tokenized text data
    doc_metadata = doc_metadata,   # Document metadata
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
    validation_config = validation_config
  )
  
  log_message(paste("Token validation complete:", final_docs, "documents,", 
                    final_terms, "unique terms,", final_tokens, "total tokens"), 
              "validate_tokens")
  
  # Return standardized result
  return(create_result(
    data = list(
      tokens = tokenized_text,
      vocab = vocabulary,
      doc_metadata = doc_metadata
    ),
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}