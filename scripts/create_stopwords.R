#' @title Generate enhanced stopwords for NAP corpus
#' @description Creates a comprehensive programmatic stopword list including
#'   geographical names, non-English words, and document artifacts for NAP document
#'   preprocessing. Uses a simplified, comprehensive approach.
#'
#' @param text_data Data frame containing document text and metadata
#' @param text_column Column name with document text (default: "pdf_text")
#' @param categories Categories of stopwords to include (default: c("geographic", "non_english", "artifacts"))
#' @param custom_additions Additional custom stopwords to include (default: NULL)
#'
#' @return A list with standardized structure containing:
#'   \item{data}{Stopwords vector and categorized lists}
#'   \item{metadata}{Processing information and statistics}
#'   \item{diagnostics}{Information about the process and any issues}
#'
#' @examples
#' \dontrun{
#' # Generate comprehensive stopwords
#' nap_stops <- generate_nap_stopwords(nap_data)
#' saveRDS(nap_stops, "data/nap_stopwords.rds")
#' 
#' # Use in corpus preparation
#' corpus <- prepare_corpus(nap_data, custom_stopwords = nap_stops$data$stopwords)
#' }

generate_nap_stopwords <- function(
    # Core inputs
  text_data,
  text_column = "pdf_text",
  categories = c("geographic", "non_english", "artifacts"),
  custom_additions = NULL
) {
  
  ## --- Initialization -------------------------------------------------------
  
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize storage for stopwords by category
  stopwords_by_category <- list()
  
  # Track packages used (for diagnostics)
  used_packages <- c("base", "magrittr")
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    corpus_stats = list()
  )
  
  ## --- Input validation -----------------------------------------------------
  
  if (!is.data.frame(text_data)) {
    stop("Input must be a dataframe")
  }
  
  if (!text_column %in% names(text_data)) {
    stop(paste("Text column", text_column, "not found in input data"))
  }
  
  log_message("Starting stopword generation", "generate_nap_stopwords")
  
  ## --- Extract corpus terms -------------------------------------------------
  
  corpus_terms <- list()
  
  # Only process if we have text data
  if (any(!is.na(text_data[[text_column]])) && 
      any(nchar(text_data[[text_column]]) > 0)) {
    
    log_message("Extracting corpus terms for analysis", "generate_nap_stopwords")
    
    # Basic tokenization 
    all_words <- character()
    
    for (i in seq_len(nrow(text_data))) {
      if (!is.na(text_data[[text_column]][i]) && nchar(text_data[[text_column]][i]) > 0) {
        # Convert to lowercase
        text <- tolower(text_data[[text_column]][i])
        
        # Replace all punctuation with spaces
        text <- gsub("[[:punct:]]", " ", text)
        
        # Split by whitespace
        words <- unlist(strsplit(text, "\\s+"))
        
        # Remove empty strings
        words <- words[words != ""]
        
        # Add to collection
        all_words <- c(all_words, words)
      }
    }
    
    # Calculate word frequencies
    if (length(all_words) > 0) {
      # Count word frequencies
      word_freq <- table(all_words)
      word_freq_df <- data.frame(
        word = names(word_freq),
        freq = as.numeric(word_freq),
        stringsAsFactors = FALSE
      )
      
      # Sort by frequency
      word_freq_df <- word_freq_df[order(-word_freq_df$freq), ]
      
      # Store for later use
      corpus_terms$freq_terms <- word_freq_df
      
      # Store corpus stats in diagnostics
      diagnostics$corpus_stats$total_terms <- nrow(word_freq_df)
      diagnostics$corpus_stats$total_tokens <- sum(word_freq_df$freq)
      diagnostics$corpus_stats$top_terms <- head(word_freq_df, 50)
      
      log_message(paste("Extracted", nrow(word_freq_df), "unique terms from corpus"), 
                  "generate_nap_stopwords")
    }
  }
  
  ## --- Generate artifact stopwords ------------------------------------------
  
  if ("artifacts" %in% categories) {
    log_message("Generating document artifact stopwords", "generate_nap_stopwords")
    
    artifact_stops <- c()
    
    # Process directly from input text if corpus terms not available
    if (is.null(corpus_terms$freq_terms)) {
      log_message("Extracting artifact tokens directly from input text", "generate_nap_stopwords")
      direct_artifacts <- extract_artifact_tokens(text_data, text_column)
      artifact_stops <- c(artifact_stops, direct_artifacts)
      
      log_message(paste("Found", length(direct_artifacts), "tokens with numbers or punctuation"), 
                  "generate_nap_stopwords")
    } else {
      # Enhanced number detection
      number_pattern <- "[0-9]|[⁰¹²³⁴⁵⁶⁷⁸⁹½¼¾]"
      number_tokens <- corpus_terms$freq_terms$word[grepl(number_pattern, corpus_terms$freq_terms$word)]
      artifact_stops <- c(artifact_stops, number_tokens)
      
      log_message(paste("Found", length(number_tokens), "tokens containing numbers"), 
                  "generate_nap_stopwords")
      
      # Simplified approach that will work in any environment
      punct_tokens <- corpus_terms$freq_terms$word[grepl("[^[:alnum:][:space:]]", corpus_terms$freq_terms$word)]
      artifact_stops <- c(artifact_stops, punct_tokens)
      
      log_message(paste("Found", length(punct_tokens), "tokens containing punctuation"), 
                  "generate_nap_stopwords")
      
      # Climate document specific formats
      format_patterns <- c(
        "\\b[12][0-9]{3}\\b",                          # Years
        "\\b[0-9]+%|\\b[0-9]+\\.[0-9]+%",              # Percentages
        "\\b[0-9]+°C|\\b[0-9]+\\.[0-9]+°C",            # Temperatures
        "\\b[0-9]+\\s*(mm|cm|m|km|ha|km²|m²)",         # Measurements
        "\\bsection\\s*[0-9]|\\bchapter\\s*[0-9]"      # Document references
      )
      
      format_pattern <- paste(format_patterns, collapse = "|")
      format_tokens <- corpus_terms$freq_terms$word[grepl(format_pattern, corpus_terms$freq_terms$word, ignore.case = TRUE)]
      artifact_stops <- c(artifact_stops, format_tokens)
      
      log_message(paste("Found", length(format_tokens), "tokens with document-specific formats"), 
                  "generate_nap_stopwords")
    }
    
    # Rest of your existing code...
  }
  
  ## --- Generate geographic stopwords ----------------------------------------
  
  if ("geographic" %in% categories) {
    log_message("Generating geographic stopwords", "generate_nap_stopwords")
    used_packages <- c(used_packages, "countrycode", "maps")
    
    geo_stops <- c()
    
    # Get country data from countrycode
    country_data <- countrycode::codelist
    
    # Get ALL country names
    country_names <- c(
      tolower(country_data$country.name.en),
      tolower(country_data$country.name.fr),
      tolower(country_data$country.name.de),
      tolower(country_data$country.name.it),
      tolower(country_data$country.name.es)
    )
    country_names <- country_names[!is.na(country_names)]
    
    # Get ALL country codes
    country_codes <- c(
      tolower(country_data$iso2c),
      tolower(country_data$iso3c),
      tolower(country_data$cowc),
      tolower(country_data$ioc),
      tolower(country_data$fips),
      tolower(country_data$un.name.en),
      tolower(country_data$p4.name)
    )
    country_codes <- country_codes[!is.na(country_codes)]
    
    # Add to stopwords
    geo_stops <- c(geo_stops, country_names, country_codes)
    
    # Add individual words from multi-word countries
    for (country in unique(country_names)) {
      parts <- unlist(strsplit(country, "\\s+"))
      if (length(parts) > 1) {
        geo_stops <- c(geo_stops, parts)
      }
    }
    
    # Get ALL cities from maps package
    world_cities <- maps::world.cities
    
    # Include ALL cities, not just capitals or major ones
    all_cities <- tolower(world_cities$name)
    geo_stops <- c(geo_stops, all_cities)
    
    # Add possessive forms for countries and cities
    possessives <- paste0(c(country_names, all_cities), "'s")
    geo_stops <- c(geo_stops, possessives)
    
    # Standard adjectival forms for countries
    country_adjs <- c()
    
    for (country in unique(country_names)) {
      # Various common adjectival patterns
      country_adjs <- c(
        country_adjs,
        paste0(country, "n"),
        paste0(country, "an"),
        paste0(country, "ian"),
        paste0(country, "ish"),
        paste0(country, "ese"),
        paste0(country, "i"),
        gsub("a$", "an", country),
        gsub("e$", "ean", country),
        gsub("y$", "ian", country)
      )
    }
    
    # Add a comprehensive list of nationalities
    common_nationalities <- c(
      "afghan", "albanian", "algerian", "andorran", "angolan", "argentine", "armenian",
      "australian", "austrian", "azerbaijani", "bahamian", "bahraini", "bangladeshi", 
      "barbadian", "belarusian", "belgian", "belizean", "beninese", "bhutanese", 
      "bolivian", "bosnian", "botswanan", "brazilian", "british", "bruneian", 
      "bulgarian", "burkinabe", "burmese", "burundian", "cambodian", "cameroonian", 
      "canadian", "chadian", "chilean", "chinese", "colombian", "comorian", 
      "congolese", "croatian", "cuban", "cypriot", "czech", "danish", "djiboutian", 
      "dominican", "dutch", "ecuadorian", "egyptian", "emirati", "english", 
      "equatoguinean", "eritrean", "estonian", "ethiopian", "fijian", "filipino", 
      "finnish", "french", "gabonese", "gambian", "georgian", "german", "ghanaian", 
      "greek", "grenadian", "guatemalan", "guinean", "guyanese", "haitian", 
      "honduran", "hungarian", "icelandic", "indian", "indonesian", "iranian", 
      "iraqi", "irish", "israeli", "italian", "ivorian", "jamaican", "japanese", 
      "jordanian", "kazakhstani", "kenyan", "korean", "kuwaiti", "kyrgyzstani", 
      "laotian", "latvian", "lebanese", "lesothan", "liberian", "libyan", 
      "liechtensteiner", "lithuanian", "luxembourgish", "macedonian", "malagasy", 
      "malawian", "malaysian", "maldivian", "malian", "maltese", "marshallese", 
      "mauritanian", "mauritian", "mexican", "micronesian", "moldovan", "monacan", 
      "mongolian", "montenegrin", "moroccan", "mozambican", "namibian", "nauruan", 
      "nepalese", "dutch", "zealander", "nicaraguan", "nigerian", "nigerien", 
      "norwegian", "omani", "pakistani", "palauan", "palestinian", "panamanian", 
      "guinean", "paraguayan", "peruvian", "filipino", "polish", "portuguese", 
      "qatari", "romanian", "russian", "rwandan", "kittian", "lucian", "vincentian", 
      "samoan", "marinese", "sao", "tomean", "saudi", "scottish", "senegalese", 
      "serbian", "seychellois", "leonean", "singaporean", "slovak", "slovenian", 
      "somali", "somaliland", "african", "sudanese", "surinamese", "swazi", 
      "swedish", "swiss", "syrian", "taiwanese", "tajikistani", "tanzanian", 
      "thai", "timorese", "togolese", "tongan", "trinidadian", "tobagonian", 
      "tunisian", "turkish", "turkmen", "tuvaluan", "ugandan", "ukrainian", 
      "uruguayan", "uzbekistani", "vanuatuan", "venezuelan", "vietnamese", 
      "welsh", "yemeni", "zambian", "zimbabwean"
    )
    
    geo_stops <- c(geo_stops, country_adjs, common_nationalities)
    
    # Add common geographic terms
    geo_common <- c(
      # Directions
      "north", "south", "east", "west", "northeast", "southeast", 
      "northwest", "southwest", "northern", "southern", "eastern", "western",
      "central", "coastal", "inland", "offshore", "onshore", "upland", "lowland",
      
      # Region types
      "country", "nation", "state", "province", "region", "territory", "district",
      "city", "town", "village", "county", "municipality", "prefecture", "zone",
      "area", "locality", "settlement", "continent", "island", "peninsula", "coast",
      "gulf", "bay", "strait", "isthmus", "canal", "archipelago", "atoll", "oasis",
      "river", "lake", "ocean", "sea", "desert", "mountain", "valley", "plain",
      "plateau", "basin", "delta", "estuary", "highland", "lowland", "wetland",
      
      # Regional groupings
      "africa", "asia", "europe", "americas", "oceania", "antarctica",
      "caribbean", "pacific", "mediterranean", "arctic", "atlantic",
      "sahara", "sahel", "maghreb", "levant", "caucasus", "balkans", "scandinavia",
      "amazon", "andes", "alpine", "himalayan", "siberia", "steppe", "pampas",
      "asean", "saarc", "commonwealth", "union", "league", "alliance"
    )
    
    geo_stops <- c(geo_stops, geo_common)
    
    # Clean up and store
    geo_stops <- unique(geo_stops)
    geo_stops <- geo_stops[geo_stops != ""]
    geo_stops <- sort(geo_stops)
    
    stopwords_by_category$geographic <- geo_stops
    
    log_message(paste("Generated", length(geo_stops), "geographic stopwords"), 
                "generate_nap_stopwords")
  }
  
  ## --- Generate non-English stopwords ---------------------------------------
  
  if ("non_english" %in% categories) {
    log_message("Generating non-English stopwords", "generate_nap_stopwords")
    used_packages <- c(used_packages, "stopwords")
    
    non_english_stops <- c()
    
    # Get comprehensive stopwords from multiple languages
    # Excluding Thai (th) which has caused encoding issues
    languages <- c("fr", "es", "pt", "de", "it", "nl", "sv", "no", "da", "fi", 
                   "hu", "ru", "ar", "zh", "ja", "ko", "hi", "bn", "id", "ms")
    
    # Process each language with error handling
    for (lang in languages) {
      tryCatch({
        # Get stopwords for this language
        lang_words <- stopwords::stopwords(lang, source = "stopwords-iso")
        non_english_stops <- c(non_english_stops, lang_words)
        
        log_message(paste("Added", length(lang_words), "stopwords for language:", lang),
                    "generate_nap_stopwords")
      }, error = function(e) {
        log_message(paste("Error loading stopwords for language", lang, "-", e$message),
                    "generate_nap_stopwords", "WARNING")
      })
    }
    
    # Add common non-English climate terms
    additional_terms <- c(
      # French
      "ministere", "departement", "agence", "developpement", "environnement",
      "changement", "climatique", "gouvernement", "adaptation", "vulnerabilite",
      "climatiques", "superficie", "superficies", "secteurs", "agricoles", 
      "ainsi", "precipitations", "temperature", 
      
      # Spanish
      "ministerio", "departamento", "agencia", "desarrollo", "medio", "ambiente",
      "cambio", "climatico", "gobierno", "adaptacion", "america", "latina",
      "caribe", "emisiones", "migracion", 
      
      # Portuguese
      "ministerio", "departamento", "agencia", "desenvolvimento", "ambiente",
      "mudanca", "climatica", "governo", "adaptacao", 
      
      # Any other language terms you've noticed in your documents
      "mocambique", "vulnerabilite", "emissions", "schweizerische", "bundesrat",
      "eidgenossisches"
    )
    
    non_english_stops <- c(non_english_stops, additional_terms)
    
    # Safe approach to find non-English terms in the corpus
    if (!is.null(corpus_terms$freq_terms)) {
      log_message("Analyzing corpus for potential non-English terms", "generate_nap_stopwords")
      
      # Only process words with valid UTF-8 encoding
      valid_words <- which(sapply(corpus_terms$freq_terms$word, function(w) {
        tryCatch({
          identical(w, iconv(w, "UTF-8", "UTF-8", sub=""))
        }, error = function(e) FALSE)
      }))
      
      if (length(valid_words) > 0) {
        # Simple non-English markers (safe ASCII characters only)
        safe_markers <- c(
          # Common accent markers that are generally safe
          "e\\u0301", "e\\u0300", "a\\u0301", "a\\u0300", "i\\u0301", "o\\u0301", "u\\u0301",
          # Common letter combinations rare in English
          "cz", "sz", "zz", "sch", "ij"
        )
        
        # Use a combined regex to find potential non-English words
        # This avoids processing each pattern separately
        for (marker in safe_markers) {
          tryCatch({
            matching_words <- corpus_terms$freq_terms$word[valid_words][
              grepl(marker, corpus_terms$freq_terms$word[valid_words], perl=TRUE)
            ]
            
            if (length(matching_words) > 0) {
              non_english_stops <- c(non_english_stops, matching_words)
              log_message(paste("Found", length(matching_words), 
                                "potential non-English terms with marker:", marker),
                          "generate_nap_stopwords")
            }
          }, error = function(e) {
            log_message(paste("Error processing marker:", marker, "-", e$message),
                        "generate_nap_stopwords", "WARNING")
          })
        }
        
        # Find words with unusual consonant patterns (safe approach)
        tryCatch({
          consonant_pattern <- "[bcdfghjklmnpqrstvwxz]{4,}"
          consonant_clusters <- corpus_terms$freq_terms$word[valid_words][
            grepl(consonant_pattern, corpus_terms$freq_terms$word[valid_words])
          ]
          
          if (length(consonant_clusters) > 0) {
            non_english_stops <- c(non_english_stops, consonant_clusters)
            log_message(paste("Found", length(consonant_clusters), "terms with unusual consonant clusters"),
                        "generate_nap_stopwords")
          }
        }, error = function(e) {
          log_message(paste("Error processing consonant clusters:", e$message),
                      "generate_nap_stopwords", "WARNING")
        })
      }
    }
    
    # Additional heuristic: very long words are often non-English (especially in technical documents)
    if (!is.null(corpus_terms$freq_terms)) {
      tryCatch({
        long_words <- corpus_terms$freq_terms$word[nchar(corpus_terms$freq_terms$word) > 15]
        if (length(long_words) > 0) {
          non_english_stops <- c(non_english_stops, long_words)
          log_message(paste("Added", length(long_words), "unusually long words (potential non-English terms)"),
                      "generate_nap_stopwords")
        }
      }, error = function(e) {
        log_message(paste("Error processing long words:", e$message),
                    "generate_nap_stopwords", "WARNING")
      })
    }
    
    # Clean up and store
    non_english_stops <- unique(non_english_stops)
    non_english_stops <- non_english_stops[non_english_stops != ""]
    non_english_stops <- sort(non_english_stops)
    
    stopwords_by_category$non_english <- non_english_stops
    
    log_message(paste("Generated", length(non_english_stops), "non-English stopwords"),
                "generate_nap_stopwords")
  }
  
  ## --- Combine all stopwords ------------------------------------------------
  
  # Start with an empty vector
  all_stopwords <- c()
  
  # Add each category
  for (category in names(stopwords_by_category)) {
    all_stopwords <- c(all_stopwords, stopwords_by_category[[category]])
  }
  
  # Add custom additions if provided
  if (!is.null(custom_additions) && length(custom_additions) > 0) {
    all_stopwords <- c(all_stopwords, custom_additions)
    log_message(paste("Added", length(custom_additions), "custom stopwords"),
                "generate_nap_stopwords")
  }
  
  # Remove duplicates and sort
  all_stopwords <- unique(all_stopwords)
  all_stopwords <- all_stopwords[all_stopwords != ""]
  all_stopwords <- sort(all_stopwords)
  
  log_message(paste("Final stopwords list contains", length(all_stopwords), "unique terms"),
              "generate_nap_stopwords")
  
  ## --- Calculate category overlap -------------------------------------------
  
  category_overlap <- list()
  if (length(stopwords_by_category) >= 2) {
    log_message("Calculating category overlap statistics", "generate_nap_stopwords")
    cats <- names(stopwords_by_category)
    
    # Calculate pairwise overlaps
    for (i in 1:(length(cats) - 1)) {
      for (j in (i + 1):length(cats)) {
        cat1 <- cats[i]
        cat2 <- cats[j]
        
        # Get the terms for each category
        terms1 <- stopwords_by_category[[cat1]]
        terms2 <- stopwords_by_category[[cat2]]
        
        # Calculate intersection
        intersection <- intersect(terms1, terms2)
        
        # Calculate metrics
        overlap_count <- length(intersection)
        overlap_percent1 <- round(100 * overlap_count / length(terms1), 1)
        overlap_percent2 <- round(100 * overlap_count / length(terms2), 1)
        
        # Store results
        pair_name <- paste(cat1, cat2, sep = "_")
        category_overlap[[pair_name]] <- list(
          count = overlap_count,
          percent_of_first = overlap_percent1,
          percent_of_second = overlap_percent2,
          common_terms = head(intersection, 20) # Just store a sample of common terms
        )
        
        log_message(sprintf("Overlap between %s and %s: %d terms (%.1f%% of %s, %.1f%% of %s)",
                            cat1, cat2, overlap_count, overlap_percent1, cat1, overlap_percent2, cat2),
                    "generate_nap_stopwords")
      }
    }
  }
  
  ## --- Prepare result object ------------------------------------------------
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create category counts
  category_counts <- sapply(stopwords_by_category, length)
  
  # Build the result object
  result <- list(
    # Core output
    data = list(
      stopwords = all_stopwords,
      categories = stopwords_by_category,
      stats = list(
        top_stopwords = head(all_stopwords, 100),
        length_distribution = table(nchar(all_stopwords))
      )
    ),
    
    # Processing metadata
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      total_stopwords = length(all_stopwords),
      category_counts = category_counts,
      parameters = list(
        categories = categories
      )
    ),
    
    # Diagnostic information
    diagnostics = c(
      diagnostics,
      list(
        packages_used = unique(used_packages),
        category_overlap = category_overlap
      )
    )
  )
  
  # Log completion
  log_message(paste("Generated", length(all_stopwords), "stopwords in", 
                    round(processing_time, 2), "seconds"),
              "generate_nap_stopwords")
  
  return(result)
}