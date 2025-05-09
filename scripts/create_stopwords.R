#' @title Generate enhanced stopwords for NAP corpus
#' @description Creates a comprehensive programmatic stopword list including
#'   geographical names, document artifacts, and unwanted patterns for NAP document
#'   preprocessing. Uses pattern detection and external packages for reproducibility.
#'
#' @param text_data Data frame containing country and region metadata
#' @param country_col Column name with country names (default: "country_name")
#' @param region_col Column name with region information (default: "region")
#' @param include_countries Whether to include country stopwords (default: TRUE)
#' @param include_cities Whether to include city stopwords (default: TRUE)
#' @param include_regions Whether to include region stopwords (default: TRUE)
#' @param include_artifacts Whether to include document artifacts (default: TRUE)
#' @param include_patterns Whether to include pattern-based terms (default: TRUE)
#' @param include_multilingual Whether to include non-English stopwords (default: TRUE)
#' @param exclude_abbreviations Whether to exclude common abbreviations (default: TRUE)
#' @param custom_additions Additional custom stopwords to include (default: NULL)
#'
#' @return A character vector of stopwords
#'
#' @examples
#' \dontrun{
#' # Generate comprehensive stopwords
#' nap_stops <- generate_nap_stopwords(nap_data)
#' 
#' # Use in corpus preparation
#' corpus <- prepare_corpus(nap_data, custom_stopwords = nap_stops)
#' }

generate_nap_stopwords <- function(
    text_data,
    country_col = "country_name",
    region_col = "region",
    include_countries = TRUE,
    include_cities = TRUE,
    include_regions = TRUE,
    include_artifacts = TRUE,
    include_patterns = TRUE,
    include_multilingual = TRUE,
    exclude_abbreviations = FALSE,
    custom_additions = NULL
) {
  # Initialize stopwords list
  stopwords <- c()
  
  # Common basic stopwords specific to NAP documents
  base_stops <- c("mr", "https", "http", "la", "yet", "de", "i.e", "yr", "tion", "des", "svg")
  stopwords <- c(stopwords, base_stops)
  
  # --- Process countries if requested -------------------------------------
  if (include_countries) {
    country_stops <- generate_country_stopwords(
      text_data, 
      country_col = country_col,
      region_col = region_col,
      include_adjectives = TRUE,
      include_regions = include_regions
    )
    stopwords <- c(stopwords, country_stops)
  }
  
  # --- Process cities if requested ---------------------------------------
  if (include_cities) {
    city_stops <- generate_city_stopwords()
    stopwords <- c(stopwords, city_stops)
  }
  
  # --- Process document artifacts if requested ---------------------------
  if (include_artifacts) {
    artifact_stops <- generate_artifact_stopwords()
    stopwords <- c(stopwords, artifact_stops)
  }
  
  # --- Process pattern-based terms if requested --------------------------
  if (include_patterns) {
    pattern_stops <- generate_pattern_stopwords()
    stopwords <- c(stopwords, pattern_stops)
  }
  
  # --- Process multilingual stopwords if requested -----------------------
  if (include_multilingual) {
    multilingual_stops <- generate_multilingual_stopwords()
    stopwords <- c(stopwords, multilingual_stops)
  }
  
  # --- Process abbreviations if not excluded -----------------------------
  if (!exclude_abbreviations) {
    abbrev_stops <- generate_abbreviation_stopwords()
    stopwords <- c(stopwords, abbrev_stops)
  }
  
  # --- Add custom additions if provided ----------------------------------
  if (!is.null(custom_additions)) {
    stopwords <- c(stopwords, custom_additions)
  }
  
  # --- Final processing -------------------------------------------------
  # Remove duplicates and sort
  stopwords <- unique(stopwords)
  stopwords <- sort(stopwords)
  
  # Log some statistics
  log_message(paste("Generated", length(stopwords), "stopwords"), "generate_nap_stopwords")
  
  return(stopwords)
}

# Helper functions for each stopword category
# -------------------------------------------------------------------------

#' Generate country-related stopwords
generate_country_stopwords <- function(
    text_data,
    country_col = "country_name",
    region_col = "region",
    include_adjectives = TRUE,
    include_regions = TRUE
) {
  # This leverages your existing function with enhancements
  # The function is in utils.R but we'll extend it here
  
  # Check if countrycode package is available for enhanced country data
  has_countrycode <- requireNamespace("countrycode", quietly = TRUE)
  
  country_stops <- c()
  
  # Process country names if available
  if (country_col %in% names(text_data)) {
    # Extract unique country names and convert to lowercase
    country_names <- unique(text_data[[country_col]])
    country_names <- tolower(country_names[!is.na(country_names)])
    
    # Add country names to stopwords
    country_stops <- c(country_stops, country_names)
    
    # Add alternative forms (e.g., "United States" -> "united", "states")
    for (country in country_names) {
      parts <- unlist(strsplit(country, "\\s+"))
      if (length(parts) > 1) {
        country_stops <- c(country_stops, parts)
      }
    }
    
    # Add possessive forms (country's)
    country_possessive <- paste0(country_names, "'s")
    country_stops <- c(country_stops, country_possessive)
    
    # If countrycode is available, get additional country variations
    if (has_countrycode) {
      # Get ISO codes
      iso_codes <- c()
      for (country in country_names) {
        # Try to get ISO codes
        tryCatch({
          iso3c <- countrycode::countrycode(country, "country.name", "iso3c")
          iso2c <- countrycode::countrycode(country, "country.name", "iso2c")
          if (!is.na(iso3c)) iso_codes <- c(iso_codes, tolower(iso3c))
          if (!is.na(iso2c)) iso_codes <- c(iso_codes, tolower(iso2c))
        }, error = function(e) {
          # Just continue if code not found
        })
      }
      country_stops <- c(country_stops, iso_codes)
      
      # Get alternative names if available
      alt_names <- c()
      for (country in country_names) {
        # Try different name formats
        tryCatch({
          iso3c <- countrycode::countrycode(country, "country.name", "iso3c")
          if (!is.na(iso3c)) {
            alt_name <- countrycode::countrycode(iso3c, "iso3c", "country.name")
            if (!is.na(alt_name) && tolower(alt_name) != tolower(country)) {
              alt_names <- c(alt_names, tolower(alt_name))
            }
          }
        }, error = function(e) {
          # Just continue if not found
        })
      }
      country_stops <- c(country_stops, alt_names)
    }
    
    # Add adjectival forms if requested
    if (include_adjectives) {
      country_adjs <- c()
      
      for (country in country_names) {
        # Handle common endings
        if (grepl("a$", country)) {
          # Countries ending in 'a' -> 'an' (America -> American)
          adj <- gsub("a$", "an", country)
          country_adjs <- c(country_adjs, adj)
        } else if (grepl("e$", country)) {
          # Countries ending in 'e' -> 'ese' (Chinese)
          adj <- paste0(country, "se")
          country_adjs <- c(country_adjs, adj)
        } else {
          # Default -> add 'ian' (Canada -> Canadian)
          adj <- paste0(country, "ian")
          country_adjs <- c(country_adjs, adj)
        }
      }
      
      # Add the adjectival forms
      country_stops <- c(country_stops, country_adjs)
      
      # Add common nationality patterns
      common_nationalities <- c(
        # Common exceptions that don't follow regular rules
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
        "scottish", "senegalese", "serbian", "sierra leonean", "singaporean", 
        "slovak", "somali", "south african", "spanish", "sri lankan", "sudanese", 
        "swedish", "swiss", "syrian", "taiwanese", "tajik", "thai", "tunisian", 
        "turkish", "ugandan", "ukrainian", "uruguayan", "uzbekistani", 
        "venezuelan", "vietnamese", "welsh", "yemeni", "zambian", "zimbabwean"
      )
      
      country_stops <- c(country_stops, common_nationalities)
    }
  }
  
  # Process region names if available and requested
  if (include_regions && region_col %in% names(text_data)) {
    # Extract unique region names
    region_names <- unique(text_data[[region_col]])
    region_names <- tolower(region_names[!is.na(region_names)])
    
    # Add region names to stopwords
    country_stops <- c(country_stops, region_names)
    
    # Add parts of region names (e.g., "Latin America & Caribbean" -> "latin", "america", "caribbean")
    for (region in region_names) {
      parts <- unlist(strsplit(region, "[\\s&]+"))
      if (length(parts) > 1) {
        country_stops <- c(country_stops, parts)
      }
    }
    
    # Add common region terms
    common_regions <- c(
      "africa", "african", "americas", "american", "asia", "asian",
      "europe", "european", "pacific", "caribbean", "latin", "central",
      "south", "north", "east", "west", "eastern", "western", "southern", "northern",
      "middle", "east", "southeast", "northeast", "central", "sub", "saharan"
    )
    
    country_stops <- c(country_stops, common_regions)
  }
  
  # Additional country-specific terms often appearing in NAPs
  additional_country_terms <- c(
    # Special cases from your list
    "saint lucia's", "congo", "drc"
  )
  
  country_stops <- c(country_stops, additional_country_terms)
  
  # Remove duplicates and sort
  country_stops <- unique(country_stops)
  country_stops <- sort(country_stops)
  
  return(country_stops)
}

#' Generate city-related stopwords
generate_city_stopwords <- function() {
  city_stops <- c()
  
  # Check if maps package is available
  has_maps <- requireNamespace("maps", quietly = TRUE)
  has_rnaturalearth <- requireNamespace("rnaturalearth", quietly = TRUE)
  
  if (has_maps) {
    # Get world cities from maps package
    world_cities <- maps::world.cities
    
    # Extract capitals
    capitals <- world_cities$name[world_cities$capital == 1]
    capitals <- tolower(capitals)
    city_stops <- c(city_stops, capitals)
    
    # Add possessive forms
    capitals_possessive <- paste0(capitals, "'s")
    city_stops <- c(city_stops, capitals_possessive)
    
    # Add major cities (population > 1 million)
    major_cities <- world_cities$name[world_cities$pop > 1000000]
    major_cities <- tolower(major_cities)
    city_stops <- c(city_stops, major_cities)
  }
  
  if (has_rnaturalearth) {
    # Get additional geographic data from rnaturalearth
    tryCatch({
      # Get populated places
      populated <- rnaturalearth::ne_download(
        scale = 110, 
        type = "populated_places", 
        category = "cultural",
        returnclass = "sf"
      )
      
      if (!is.null(populated) && "name" %in% names(populated)) {
        city_names <- tolower(populated$name)
        city_stops <- c(city_stops, city_names)
      }
    }, error = function(e) {
      # Continue if there's an error
    })
  }
  
  # If neither package is available, provide a basic list of major cities
  if (!has_maps && !has_rnaturalearth) {
    basic_cities <- c(
      "tokyo", "delhi", "shanghai", "sao paulo", "mexico city", "cairo", 
      "mumbai", "beijing", "dhaka", "osaka", "new york", "karachi", 
      "buenos aires", "chongqing", "istanbul", "kolkata", "manila", 
      "lagos", "rio de janeiro", "tianjin", "kinshasa", "guangzhou", 
      "los angeles", "moscow", "shenzhen", "lahore", "bangalore", 
      "paris", "bogota", "jakarta", "chennai", "lima", "bangkok", 
      "seoul", "nagoya", "hyderabad", "london", "tehran", "chicago", 
      "chengdu", "nanjing", "wuhan", "ho chi minh city", "luanda", 
      "ahmedabad", "kuala lumpur", "xi'an", "hong kong", "dongguan", 
      "hangzhou", "foshan", "shenyang"
    )
    city_stops <- c(city_stops, basic_cities)
  }
  
  # Remove duplicates and sort
  city_stops <- unique(city_stops)
  city_stops <- sort(city_stops)
  
  return(city_stops)
}

#' Generate document artifact stopwords
generate_artifact_stopwords <- function() {
  # Create patterns for common document artifacts
  artifact_stops <- c(
    # File extensions
    "pdf", "doc", "docx", "xlsx", "ppt", "html", "xml", "txt", "csv", "xls",
    
    # URL components
    "www", "http", "https", "gov", "org", "com", "net", "edu", "doi", "org",
    
    # Document formatting
    "svg", "jpg", "png", "fig", "tab", "annex", "appendix", "chapter", 
    "section", "page", "paragraph", "ref", "figure", "table",
    
    # Document metadata
    "english", "final", "draft", "version", "official", "confidential",
    "20english", "20final", "20is", "20serbia",
    
    # Specific artifacts from your examples
    "20english.pdf", "20final_serbia", "www.pcbs.gov.ps", "doi.org",
    "________________"
  )
  
  # Generate variations based on common patterns
  variations <- c()
  
  # Add number-prefixed variations (e.g., "20english")
  for (num in 0:99) {
    variations <- c(variations, paste0(num, "english"))
    variations <- c(variations, paste0(num, "final"))
    variations <- c(variations, paste0(num, "draft"))
  }
  
  artifact_stops <- c(artifact_stops, variations)
  
  # Remove duplicates and sort
  artifact_stops <- unique(artifact_stops)
  artifact_stops <- sort(artifact_stops)
  
  return(artifact_stops)
}

#' Generate pattern-based stopwords
generate_pattern_stopwords <- function() {
  pattern_stops <- c()
  
  # Titles
  titles <- c("mr", "dr", "ms", "prof", "officer", "sir", "madam", "excellency")
  pattern_stops <- c(pattern_stops, titles)
  
  # Section headings & markers
  headings <- c(
    # From your examples
    "a2.5", "a8", "ag1", "ag10", "ag11", "ag12", "ag13", "ag14", 
    "ag15", "ag16", "4.9n", "9.5n", "a1.2.3"
  )
  
  # Generate common heading patterns (a1, a2, b1, b2, etc.)
  for (letter in letters) {
    for (num in 1:30) {
      headings <- c(headings, paste0(letter, num))
      headings <- c(headings, paste0(letter, num, "."))
      
      # Add subsections
      for (subnum in 1:10) {
        headings <- c(headings, paste0(letter, num, ".", subnum))
      }
    }
  }
  
  pattern_stops <- c(pattern_stops, headings)
  
  # Measurement units with numbers
  measurements <- c(
    # From your examples
    "60mm", "1,000m", "15th"
  )
  
  # Generate common measurement patterns
  units <- c("mm", "cm", "m", "km", "ha", "ml", "l", "kg", "g", "t")
  for (unit in units) {
    # Add different number formats with the unit
    measurements <- c(measurements, paste0("10", unit))
    measurements <- c(measurements, paste0("100", unit))
    measurements <- c(measurements, paste0("1000", unit))
    measurements <- c(measurements, paste0("10,000", unit))
  }
  
  # Add ordinals - fixing the vectorized operation
  ordinals <- c()
  for (num in 1:100) {
    # Handle each number individually to avoid vectorization issues
    if (num %% 100 %in% c(11, 12, 13)) {
      suffix <- "th"
    } else if (num %% 10 == 1) {
      suffix <- "st"
    } else if (num %% 10 == 2) {
      suffix <- "nd"
    } else if (num %% 10 == 3) {
      suffix <- "rd"
    } else {
      suffix <- "th"
    }
    ordinals <- c(ordinals, paste0(num, suffix))
  }
  
  pattern_stops <- c(pattern_stops, measurements, ordinals)
  
  # Truncated/incomplete words
  truncated <- c(
    # From your examples
    "implementa", "adapta", "incr"
  )
  pattern_stops <- c(pattern_stops, truncated)
  
  # Climate-specific patterns
  climate_patterns <- c(
    # From your examples
    "rcp2.6", "rcp6.0", "bdp2100", 
    
    # Common climate scenarios
    "rcp1.9", "rcp4.5", "rcp8.5", "ssp1", "ssp2", "ssp3", "ssp4", "ssp5",
    "ssp1-1.9", "ssp1-2.6", "ssp2-4.5", "ssp3-7.0", "ssp5-8.5"
  )
  pattern_stops <- c(pattern_stops, climate_patterns)
  
  # Remove duplicates and sort
  pattern_stops <- unique(pattern_stops)
  pattern_stops <- sort(pattern_stops)
  
  return(pattern_stops)
}

#' Generate multilingual stopwords
generate_multilingual_stopwords <- function() {
  multilingual_stops <- c()
  
  # Check if the stopwords package is available
  has_stopwords <- requireNamespace("stopwords", quietly = TRUE)
  
  if (has_stopwords) {
    # Get stopwords from common languages in NAPs
    tryCatch({
      # Add French stopwords
      french_stops <- stopwords::stopwords("fr", source = "snowball")
      multilingual_stops <- c(multilingual_stops, french_stops)
      
      # Add Spanish stopwords
      spanish_stops <- stopwords::stopwords("es", source = "snowball")
      multilingual_stops <- c(multilingual_stops, spanish_stops)
      
      # Add Portuguese stopwords
      portuguese_stops <- stopwords::stopwords("pt", source = "snowball")
      multilingual_stops <- c(multilingual_stops, portuguese_stops)
      
    }, error = function(e) {
      # Continue if there's an error
    })
  }
  
  # Add specific non-English terms from your examples
  specific_terms <- c(
    "précipitations", "être", "agência", "atmosférica"
  )
  multilingual_stops <- c(multilingual_stops, specific_terms)
  
  # Remove duplicates and sort
  multilingual_stops <- unique(multilingual_stops)
  multilingual_stops <- sort(multilingual_stops)
  
  return(multilingual_stops)
}

#' Generate abbreviation stopwords
generate_abbreviation_stopwords <- function() {
  # Common abbreviations found in NAP documents
  abbrev_stops <- c(
    # From your examples
    "cccsp", "nr", "cvi", "snc", "ssp2", "bdp2100",
    
    # Common climate and development abbreviations
    "nap", "ndc", "ipcc", "unfccc", "unep", "undp", "gcf", "gef", "ldcf",
    "sccf", "mdg", "sdg", "ngo", "igo", "cbdr", "lulucf", "redd",
    "indc", "ghg", "co2", "ch4", "n2o", "gdp", "gni", "gii", "hdi",
    "wmo", "fao", "who", "wfp", "unisdr", "oecd", "opec", "afolu",
    "napa", "nama", "leds", "giz", "usaid", "dfid", "jica", "sida"
  )
  
  # Remove duplicates and sort
  abbrev_stops <- unique(abbrev_stops)
  abbrev_stops <- sort(abbrev_stops)
  
  return(abbrev_stops)
}
