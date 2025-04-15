# NAP ENTROPY ANALYSIS FUNCTIONS
# ==============================
# Author: Nils Nerhus Rørstad
# Created: April 2025
# 
# This script contains functions for processing National Adaptation Plans (NAPs),
# performing structural topic modeling, and preparing data for entropy analysis.
# Data is sourced from the napr package.

# Load required packages
library(dplyr)       # Data manipulation
library(tidyr)       # Data reshaping
library(tidytext)    # Text processing
library(stm)         # Structural Topic Modeling
library(SnowballC)   # Word stemming
library(countrycode) # Country standardization
library(napr)        # NAP data

# =========================================================================
# FUNCTION: preprocess_nap
# =========================================================================

preprocess_nap <- function(nap_data, 
                           custom_stopwords = NULL, 
                           stem_words = FALSE) {
  
  # Validate inputs
  if (!"pdf_text" %in% names(nap_data)) {
    stop("NAP data must contain a 'pdf_text' column")
  }
  
  # Process NAP text data - tokenize, remove stopwords and numbers
  processed_text <- nap_data %>%
    mutate(doc_id = row_number()) %>%
    unnest_tokens(word, pdf_text) %>%
    anti_join(get_stopwords(), by = c("word" = "word")) %>%
    filter(!str_detect(word, "^[0-9]+$"))
  
  # Remove custom stopwords if provided
  if (!is.null(custom_stopwords)) {
    custom_stops <- tibble(word = custom_stopwords)
    processed_text <- processed_text %>%
      anti_join(custom_stops, by = "word")
  }
  
  # Apply stemming if requested
  if (stem_words) {
    processed_text <- processed_text %>%
      mutate(word = wordStem(word))
  }
  
  # Merge with metadata
  processed_text <- processed_text %>%
    select(doc_id, word, country_name, region, ldc_sids_marker, date_posted)
  
  # Return the processed text data
  return(processed_text)
}

# =========================================================================
# FUNCTION: optimal_topics
# =========================================================================

optimal_topics <- function(processed_text, 
                           k_min = 5, 
                           k_max = 50, 
                           k_step = 5,
                           seed = 1234) {

  # Create DTM for STM
  # Convert from tidy format to document-term matrix
  words_count <- processed_text %>%
    count(doc_id, word) %>%
    cast_dtm(doc_id, word, n)
  
  # Convert to STM format
  stm_data <- convert(words_count, to = "stm")
  
  # Extract document metadata
  doc_meta <- processed_text %>%
    distinct(doc_id, country_name, region, ldc_sids_marker, date_posted)
  
  # Define range of K values to test
  k_values <- seq(k_min, k_max, by = k_step)
  
  # Store results
  model_results <- data.frame(
    k = integer(),
    semantic_coherence = numeric(),
    exclusivity = numeric(),
    held_out = numeric(),
    score = numeric()
  )
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Test models with different k values
  cat("Testing models with different numbers of topics. This may take some time...\n")
  
  for (k in k_values) {
    cat(paste0("Fitting model with k = ", k, "...\n"))
    
    # Fit model
    model_k <- stm(
      documents = stm_data$documents,
      vocab = stm_data$vocab,
      K = k,
      max.em.its = 75,
      init.type = "Spectral",
      seed = seed,
      verbose = FALSE
    )
    
    # Calculate diagnostics
    model_diagnostics <- checkResiduals(model_k)
    heldout <- make.heldout(stm_data$documents, stm_data$vocab)
    heldout_likelihood <- eval.heldout(model_k, heldout)
    semantic_coherence <- mean(semanticCoherence(model_k, stm_data$documents))
    exclusivity <- mean(exclusivity(model_k))
    
    # Combine metrics
    model_results <- rbind(model_results, data.frame(
      k = k,
      semantic_coherence = semantic_coherence,
      exclusivity = exclusivity,
      held_out = heldout_likelihood,
      score = semantic_coherence + exclusivity
    ))
  }
  
  # Find best k value
  # Higher coherence and exclusivity is better
  best_k <- model_results %>%
    arrange(desc(score)) %>%
    slice(1) %>%
    pull(k)
  
  # Create plot of results
  plot <- model_results %>%
    gather(metric, value, -k, -score) %>%
    ggplot(aes(x = k, y = value, color = metric)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = best_k, linetype = "dashed") +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = "Topic Model Diagnostics",
      subtitle = paste("Optimal number of topics:", best_k),
      x = "Number of Topics (k)",
      y = "Value"
    ) +
    theme_minimal()
  
  # Return results
  return(list(
    best_k = best_k,
    diagnostics = model_results,
    plot = plot,
    stm_data = stm_data,
    doc_meta = doc_meta
  ))
}


# ========================
# FUNCTION: extract_topic_proportions
# ========================

extract_topic_proportions <- function(optimal_result, 
                                      final_k = NULL,
                                      include_top_terms = FALSE) {
  
  # Extract data from optimization results
  stm_data <- optimal_result$stm_data
  doc_meta <- optimal_result$doc_meta
  
  # Use provided k or best k from optimization
  k <- ifelse(is.null(final_k), optimal_result$best_k, final_k)
  
  # Fit final STM model
  cat(paste0("Fitting final model with k = ", k, "...\n"))
  final_model <- stm(
    documents = stm_data$documents,
    vocab = stm_data$vocab,
    K = k,
    max.em.its = 100,
    init.type = "Spectral",
    seed = 1234,
    verbose = FALSE
  )
  
  # Extract topic proportions (theta matrix)
  topic_props <- as.data.frame(final_model$theta)
  
  # Add document identifiers
  topic_props$doc_id <- as.integer(rownames(topic_props))
  
  # Rename topic columns with simple numeric identifiers
  colnames(topic_props)[1:k] <- paste0("Topic_", 1:k)
  
  # Store top terms for reference but don't use in labels
  top_terms <- labelTopics(final_model, n = 5)
  topic_labels <- sapply(1:k, function(i) {
    paste(top_terms$frex[i,], collapse = ", ")
  })
  
  # Reshape to long format
  topic_props_long <- topic_props %>%
    pivot_longer(
      cols = starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  # Join with document metadata
  result_df <- topic_props_long %>%
    left_join(doc_meta, by = "doc_id")
  
  # Standardize country information using countrycode
  result_df <- result_df %>%
    mutate(
      Year = as.integer(substring(date_posted, 1, 4)),
      Country = countrycode(country_name, "country.name", "country.name", nomatch = NULL),
      Region = countrycode(Country, "country.name", "region", nomatch = NULL),
      Income = countrycode(Country, "country.name", "income.level", nomatch = NULL),
      Geography = case_when(
        grepl("LDC", ldc_sids_marker) & grepl("SIDS", ldc_sids_marker) ~ "LDC, SIDS",
        grepl("LDC", ldc_sids_marker) ~ "LDC",
        grepl("SIDS", ldc_sids_marker) ~ "SIDS",
        TRUE ~ "-"
      )
    )
  
  # Select and rearrange columns for final output
  final_df <- result_df %>%
    select(Year, Country, Region, Income, Geography, Topic, Proportion)
  
  # Return the formatted data
  return(list(
    data = final_df,
    model = final_model,
    topic_labels = setNames(topic_labels, paste0("Topic_", 1:k))  # Store but don't use in main data
  ))
}