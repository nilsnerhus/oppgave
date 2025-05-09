#' @title Generate enhanced stopwords for NAP corpus
#' @description Creates a comprehensive programmatic stopword list including
#'   geographical names, document artifacts, and multilingual terms for NAP document
#'   preprocessing. Uses external packages and pattern detection.
#'
#' @param text_data Data frame containing document text and metadata
#' @param text_column Column name with document text (default: "pdf_text")
#' @param categories Categories of stopwords to include (default: c("geographic", "artifacts", "multilingual"))
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
  categories = c("geographic", "artifacts", "multilingual"),
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
    common_patterns = list(),
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
  
  ## --- Extract corpus terms for pattern detection ---------------------------
  
  corpus_terms <- list()
  
  # Only process if we have text data
  if (any(!is.na(text_data[[text_column]])) && 
      any(nchar(text_data[[text_column]]) > 0)) {
    
    log_message("Extracting corpus terms for pattern detection", "generate_nap_stopwords")
    
    # Basic tokenization 
    corpus_tokens <- data.frame(
      doc_id = integer(),
      word = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_len(nrow(text_data))) {
      if (!is.na(text_data[[text_column]][i]) && nchar(text_data[[text_column]][i]) > 0) {
        # Convert to lowercase
        text <- tolower(text_data[[text_column]][i])
        
        # Replace punctuation with spaces
        text <- gsub("[[:punct:]]", " ", text)
        
        # Split by whitespace
        words <- unlist(strsplit(text, "\\s+"))
        
        # Remove empty strings
        words <- words[words != ""]
        
        if (length(words) > 0) {
          corpus_tokens <- rbind(
            corpus_tokens,
            data.frame(
              doc_id = i,
              word = words,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
    
    # Calculate word frequencies
    if (nrow(corpus_tokens) > 0) {
      # Count word frequencies
      word_freq <- table(corpus_tokens$word)
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
  
  ## --- Generate geographic stopwords ----------------------------------------
  
  if ("geographic" %in% categories) {
    log_message("Generating geographic stopwords", "generate_nap_stopwords")
    used_packages <- c(used_packages, "countrycode", "maps")
    
    geo_stops <- c()
    
    # Get country data from countrycode
    country_data <- countrycode::codelist
    
    # Get country names
    country_names <- tolower(country_data$country.name.en)
    country_names <- country_names[!is.na(country_names)]
    
    # Add country codes
    country_codes <- c(
      tolower(country_data$iso2c),
      tolower(country_data$iso3c)
    )
    country_codes <- country_codes[!is.na(country_codes)]
    
    # Add to stopwords
    geo_stops <- c(geo_stops, country_names, country_codes)
    
    # Add individual words from multi-word countries
    for (country in country_names) {
      parts <- unlist(strsplit(country, "\\s+"))
      if (length(parts) > 1) {
        geo_stops <- c(geo_stops, parts)
      }
    }
    
    # Generate variations: possessive forms
    country_possessive <- paste0(country_names, "'s")
    geo_stops <- c(geo_stops, country_possessive)
    
    # Generate variations: adjectival forms
    country_adjs <- c()
    
    for (country in country_names) {
      if (grepl("a$", country)) {
        # Countries ending in 'a' -> 'an' (e.g., America -> American)
        adj <- gsub("a$", "an", country)
        country_adjs <- c(country_adjs, adj)
      } else if (grepl("e$", country)) {
        # Countries ending in 'e' -> 'ean' (e.g., Europe -> European)
        adj <- paste0(country, "an")
        country_adjs <- c(country_adjs, adj)
        
        # Alternative: 'ese' (e.g., Japanese)
        adj_alt <- paste0(country, "se")
        country_adjs <- c(country_adjs, adj_alt)
      } else {
        # Add common suffixes
        country_adjs <- c(
          country_adjs,
          paste0(country, "ian"),
          paste0(country, "ese"),
          paste0(country, "ish")
        )
      }
    }
    
    # Add common nationality exceptions that don't follow regular rules
    common_nationalities <- c(
      "afghan", "albanian", "algerian", "angolan", "argentine", "australian", 
      "austrian", "azerbaijani", "bahraini", "bangladeshi", "belgian", "bolivian", 
      "brazilian", "british", "cambodian", "cameroonian", "canadian", "chilean", 
      "chinese", "colombian", "congolese", "croatian", "cuban", "czech", 
      "danish", "dominican", "dutch", "ecuadorian", "egyptian", "emirati", 
      "english", "ethiopian", "fijian", "filipino", "finnish", "french", 
      "german", "ghanaian", "greek", "guatemalan", "haitian", "hungarian", 
      "icelandic", "indian", "indonesian", "iranian", "iraqi", "irish", 
      "israeli", "italian", "jamaican", "japanese", "jordanian", "kazakhstani", 
      "kenyan", "korean", "kuwaiti", "lao", "latvian", "lebanese", "liberian", 
      "libyan", "lithuanian", "malaysian", "mexican", "moldovan", "mongolian", 
      "moroccan", "mozambican", "namibian", "nepalese", "nigerian", "norwegian", 
      "omani", "pakistani", "palestinian", "paraguayan", "peruvian", "polish", 
      "portuguese", "qatari", "romanian", "russian", "rwandan", "saudi", 
      "scottish", "senegalese", "serbian", "somali", "south african", "spanish", 
      "sri lankan", "sudanese", "swedish", "swiss", "syrian", "taiwanese", 
      "tajik", "thai", "tunisian", "turkish", "ugandan", "ukrainian", "uruguayan", 
      "uzbekistani", "venezuelan", "vietnamese", "welsh", "yemeni", "zambian", "zimbabwean"
    )
    
    geo_stops <- c(geo_stops, country_adjs, common_nationalities)
    
    # Add city names from maps package
    world_cities <- maps::world.cities
    
    # Extract capital cities
    capitals <- world_cities$name[world_cities$capital == 1]
    capitals <- tolower(capitals)
    geo_stops <- c(geo_stops, capitals)
    
    # Add major cities (population > 1 million)
    major_cities <- world_cities$name[world_cities$pop > 1000000]
    major_cities <- tolower(major_cities)
    geo_stops <- c(geo_stops, major_cities)
    
    # Add possessive forms for cities
    cities_possessive <- paste0(c(capitals, major_cities), "'s")
    geo_stops <- c(geo_stops, cities_possessive)
    
    # Add common geographic terms
    geo_common <- c(
      # Directions
      "north", "south", "east", "west", "northeast", "southeast", 
      "northwest", "southwest", "northern", "southern", "eastern", "western",
      
      # Region types
      "country", "region", "province", "state", "city", "district",
      "territory", "zone", "area", "continent", "island", "peninsula",
      
      # Regional groupings
      "africa", "asia", "europe", "americas", "oceania", "antarctic",
      "caribbean", "pacific", "mediterranean", "arctic", "atlantic",
      "subsaharan", "saharan", "latinamerican", "commonwealth", "union"
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
  
  ## --- Generate document artifact stopwords ---------------------------------
  
  if ("artifacts" %in% categories) {
    log_message("Generating document artifact stopwords", "generate_nap_stopwords")
    
    artifact_stops <- c()
    
    # Basic file and document artifacts
    basic_artifacts <- c(
      # File extensions
      "pdf", "doc", "docx", "xlsx", "ppt", "html", "xml", "txt", "csv", "xls", "svg",
      
      # URL components
      "www", "http", "https", "gov", "org", "com", "net", "edu", "doi",
      
      # Document formatting
      "svg", "jpg", "png", "fig", "tab", "annex", "appendix", "chapter", 
      "section", "page", "paragraph", "ref", "figure", "table", "chart",
      "toc", "contents", "bibliography", "glossary", "footnote", "endnote",
      
      # Document metadata
      "draft", "final", "version", "official", "confidential", "internal",
      "author", "date", "updated", "revision", "copyright", "confidential",
      
      # Common document structure terms
      "header", "footer", "title", "subtitle", "heading", "subheading",
      "introduction", "conclusion", "summary", "executive", "abstract",
      "background", "methodology", "results", "discussion", "references",
      "annex", "appendix", "acknowledgements", "acknowledgments",
      
      # PDF-specific artifacts
      "page", "pg", "ibid", "et", "al", "etc", "op", "cit",
      
      # Common formatting artifacts from PDFs
      "et_al", "ibid", "eg", "ie", "viz", "cf", "nb", "q.v", "p.s"
    )
    
    artifact_stops <- c(artifact_stops, basic_artifacts)
    
    # Data-driven artifact detection (if corpus terms available)
    if (!is.null(corpus_terms$freq_terms)) {
      # Define patterns that likely indicate artifacts
      artifact_patterns <- list(
        # Numeric prefixes/suffixes
        number_prefix = "^[0-9]+[a-z]",        # e.g., "20english"
        number_suffix = "[a-z]+[0-9]+$",       # e.g., "section5"
        
        # Section/heading patterns
        section_ref = "^[a-z][0-9]+(\\.[0-9]+)?$",  # e.g., "a1", "a1.2"
        
        # URL fragments
        url_domain = "\\.(com|org|gov|edu|net)$",
        url_start = "^www\\.",
        
        # Measurement units
        measurements = "[0-9]+(mm|cm|m|km|g|kg|ha|l)$",
        
        # Special characters
        underscores = "^_+$",             # Underscores
        non_alpha = "^[^a-z0-9]+$",       # No alphanumeric characters
        
        # Common PDF extraction artifacts
        hyphenated = "\\w+\\-$",          # Words ending with hyphen
        pg_numbers = "^p[0-9]+$",         # Page numbers (e.g., p42)
        
        # Document formatting artifacts
        list_markers = "^[ivxlcdm]+\\.?$", # Roman numerals
        camelcase = "^[A-Z][a-z]+[A-Z]"    # CamelCase without spaces
      )
      
      # Find terms matching these patterns
      for (pattern_name in names(artifact_patterns)) {
        pattern <- artifact_patterns[[pattern_name]]
        
        matching_terms <- corpus_terms$freq_terms$word[
          grepl(pattern, corpus_terms$freq_terms$word)
        ]
        
        if (length(matching_terms) > 0) {
          # Add to stopwords
          artifact_stops <- c(artifact_stops, matching_terms)
          
          # Track patterns found in diagnostics
          diagnostics$common_patterns[[pattern_name]] <- list(
            pattern = pattern,
            matches = head(matching_terms, 20), # First 20 for brevity
            count = length(matching_terms)
          )
          
          log_message(paste("Found", length(matching_terms), "matches for pattern:", pattern_name),
                      "generate_nap_stopwords")
        }
      }
    }
    
    # Clean up and store
    artifact_stops <- unique(artifact_stops)
    artifact_stops <- artifact_stops[artifact_stops != ""]
    artifact_stops <- sort(artifact_stops)
    
    stopwords_by_category$artifacts <- artifact_stops
    
    log_message(paste("Generated", length(artifact_stops), "document artifact stopwords"),
                "generate_nap_stopwords")
  }
  
  ## --- Generate multilingual stopwords --------------------------------------
  
  if ("multilingual" %in% categories) {
    log_message("Generating multilingual stopwords", "generate_nap_stopwords")
    used_packages <- c(used_packages, "stopwords")
    
    multi_stops <- c()
    
    # Get stopwords from common languages in NAPs
    languages <- c("fr", "es", "pt")
    
    for (lang in languages) {
      tryCatch({
        lang_stops <- stopwords::stopwords(lang, source = "snowball")
        multi_stops <- c(multi_stops, lang_stops)
        log_message(paste("Added", length(lang_stops), "stopwords for language:", lang),
                    "generate_nap_stopwords")
      }, error = function(e) {
        log_message(paste("Error getting stopwords for language", lang, ":", e$message),
                    "generate_nap_stopwords", "WARNING")
      })
    }
    
    # Add common non-English terms found in NAPs
    additional_terms <- c(
      # French terms
      "ministère", "département", "agence", "conseil", "comité",
      "développement", "environnement", "changement", "climatique",
      "gouvernement", "national", "international", "adaptation",
      
      # Spanish terms
      "ministerio", "departamento", "agencia", "consejo", "comité",
      "desarrollo", "medio", "ambiente", "cambio", "climático",
      "gobierno", "nacional", "internacional", "adaptación",
      
      # Portuguese terms
      "ministério", "departamento", "agência", "conselho", "comitê",
      "desenvolvimento", "ambiente", "mudança", "climática",
      "governo", "nacional", "internacional", "adaptação"
    )
    
    multi_stops <- c(multi_stops, additional_terms)
    
    # Clean up and store
    multi_stops <- unique(multi_stops)
    multi_stops <- multi_stops[multi_stops != ""]
    multi_stops <- sort(multi_stops)
    
    stopwords_by_category$multilingual <- multi_stops
    
    log_message(paste("Generated", length(multi_stops), "multilingual stopwords"),
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