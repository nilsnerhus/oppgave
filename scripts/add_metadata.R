#' @title Add metadata to NAP country data
#' @description Enhances NAP country data with standardized metadata from World Bank 
#'   and custom country classifications. Separates text from metadata, standardizes 
#'   country names, adds ISO codes, parses dates, and adds region, income level, 
#'   and special status indicators.
#'   
#' @param data A dataframe containing country NAP data
#' @param text_column Column name containing document text (default: "pdf_text")
#' @param match_col Column name containing country names (default: "country_name")
#' @param date_col Column name containing dates (default: "date_posted")
#' @param geo_config List with geographic classification lists:
#'   \itemize{
#'     \item sids_list Character vector of ISO3C codes for small island developing states
#'     \item lldc_list Character vector of ISO3C codes for landlocked developing countries
#'   }
#' @param category_map List mapping higher-level categories to dimensions (e.g.,
#'   list(Income = "wb_income_level", Region = "region", Geography = c("is_sids", "is_lldc")))
#' 
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item documents - Dataframe with doc_id and text columns
#'       \item config - List with metadata, category_map and other configuration
#'     }
#'   }
#'   \item{metadata}{Processing statistics and timestamps}
#'   \item{diagnostics}{Validation results and issues encountered}
#'
#' @examples
#' \dontrun{
#' # Define category map
#' category_map <- list(
#'   Income = "wb_income_level", 
#'   Region = "region", 
#'   Geography = c("is_sids", "is_lldc"),
#'   Time = "year"
#' )
#' 
#' # Add metadata with geographic classifications and category map
#' geo_config <- list(sids_list = sids, lldc_list = lldc)
#' nap_data <- add_metadata(pdfs$data, 
#'                         geo_config = geo_config,
#'                         category_map = category_map)
#' }
add_metadata <- function(
    data, 
    text_column = "pdf_text",
    match_col = "country_name",
    date_col = "date_posted",
    geo_config = list(
      sids_list = NULL,          
      lldc_list = NULL           
    ),
    category_map = NULL
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    unmatched_countries = character(),
    unparsed_dates = character(),
    issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "add_metadata")
  
  # Validate data is a data frame
  if (!is.data.frame(data)) {
    error_msg <- "Input must be a dataframe"
    diagnostics$issues <- c(diagnostics$issues, error_msg)
    log_message(error_msg, "add_metadata", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check for empty data frame
  if (nrow(data) == 0) {
    error_msg <- "Input dataframe has no rows"
    diagnostics$issues <- c(diagnostics$issues, error_msg)
    log_message(error_msg, "add_metadata", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check required columns exist
  required_cols <- c(match_col, text_column)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    error_msg <- paste("Input is missing required columns:", paste(missing_cols, collapse = ", "))
    diagnostics$issues <- c(diagnostics$issues, error_msg)
    log_message(error_msg, "add_metadata", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate geo_config ISO codes if provided
  if (!is.null(geo_config$sids_list)) {
    invalid_sids <- geo_config$sids_list[nchar(geo_config$sids_list) != 3]
    if (length(invalid_sids) > 0) {
      warning_msg <- paste("Invalid ISO3C codes in sids_list:", paste(invalid_sids, collapse=", "))
      diagnostics$issues <- c(diagnostics$issues, warning_msg)
      log_message(warning_msg, "add_metadata", "WARNING")
    }
  }
  
  if (!is.null(geo_config$lldc_list)) {
    invalid_lldc <- geo_config$lldc_list[nchar(geo_config$lldc_list) != 3]
    if (length(invalid_lldc) > 0) {
      warning_msg <- paste("Invalid ISO3C codes in lldc_list:", paste(invalid_lldc, collapse=", "))
      diagnostics$issues <- c(diagnostics$issues, warning_msg)
      log_message(warning_msg, "add_metadata", "WARNING")
    }
  }
  
  # Create a copy of the input data
  result <- data
  
  # Add doc_id if not present (sequential numeric ID)
  if (!"doc_id" %in% names(result)) {
    result$doc_id <- as.character(1:nrow(result))
    log_message("Added sequential doc_id column", "add_metadata")
  }
  
  ## --- World Bank country data and matching -----------------------------------
  log_message("Fetching World Bank country data", "add_metadata")
  
  # Fetch World Bank data - stop with error if not available
  wb_countries <- tryCatch({
    wb_data <- wbstats::wb_countries()
    wb_data <- dplyr::select(wb_data, iso3c, country, region, income_level)
    wb_data
  }, error = function(e) {
    error_msg <- paste("Failed to fetch World Bank data:", e$message)
    diagnostics$issues <<- c(diagnostics$issues, error_msg)
    log_message(error_msg, "add_metadata", "ERROR")
    stop(error_msg)  # Stop with error as requested
  })
  
  log_message("Building country name lookup table", "add_metadata")
  
  # Build country lookup table with common variations
  country_lookup <- tryCatch({
    # Start with WB country names
    lookup_wb <- dplyr::select(wb_countries, country, iso3c)
    
    # Create custom variations dataframe for common alternate names
    variations <- tibble::tibble(
      country = c(
        "Czechia", "North Korea", "South Korea", "Russia", "Taiwan", 
        "Bolivia", "Venezuela", "Tanzania", "Syria", "Vietnam",
        "Democratic Republic of Congo", "United States", "UK", "Britain", "Saint Lucia", "St. Lucia",
        "Saint Vincent and the Grenadines", "St. Vincent and the Grenadines",
        "State of Palestine", "Palestine"
      ),
      iso3c = c(
        "CZE", "PRK", "KOR", "RUS", "TWN", 
        "BOL", "VEN", "TZA", "SYR", "VNM",
        "COD", "USA", "GBR", "GBR", "LCA", "LCA", 
        "VCT", "VCT",
        "PSE", "PSE"
      )
    )
    
    # Combine the two sources
    dplyr::bind_rows(lookup_wb, variations)
  }, error = function(e) {
    error_msg <- paste("Error building country lookup table:", e$message)
    diagnostics$issues <<- c(diagnostics$issues, error_msg)
    log_message(error_msg, "add_metadata", "WARNING")
    lookup_wb  # Return just the WB data as fallback
  })
  
  log_message("Matching countries to ISO codes", "add_metadata")
  
  # Match countries to ISO codes
  result$iso3c <- NA_character_
  match_count <- 0
  
  for (i in 1:nrow(result)) {
    # Try exact match first
    match_idx <- which(tolower(country_lookup$country) == tolower(result[[match_col]][i]))
    
    # If no exact match, try partial matching
    if (length(match_idx) == 0) {
      for (j in 1:nrow(country_lookup)) {
        if (grepl(tolower(country_lookup$country[j]), tolower(result[[match_col]][i]), fixed = TRUE) ||
            grepl(tolower(result[[match_col]][i]), tolower(country_lookup$country[j]), fixed = TRUE)) {
          match_idx <- j
          break
        }
      }
    }
    
    # Assign ISO code if match found
    if (length(match_idx) > 0) {
      result$iso3c[i] <- country_lookup$iso3c[match_idx[1]]
      match_count <- match_count + 1
    }
  }
  
  # Track unmatched countries
  unmatched <- which(is.na(result$iso3c))
  if (length(unmatched) > 0) {
    diagnostics$unmatched_countries <- result[[match_col]][unmatched]
    log_message(paste("Could not match", length(unmatched), "countries to ISO codes"), "add_metadata", "WARNING")
  }
  
  log_message("Joining with World Bank country data", "add_metadata")
  
  # Join with World Bank country data
  result <- dplyr::left_join(result, wb_countries, by = "iso3c")
  
  ## --- Date standardization ---------------------------------------------------
  parsed_count <- 0
  if (!is.null(date_col) && date_col %in% names(result)) {
    log_message(paste("Standardizing dates from", date_col, "column"), "add_metadata")
    
    # Initialize date column
    result$date_standardized <- NA
    result$year <- NA_integer_
    result$quarter <- NA_character_
    
    log_message(paste("Attempting to parse", nrow(result), "dates"), "add_metadata")
    for (i in 1:nrow(result)) {
      if (!is.na(result[[date_col]][i])) {
        # Try different date formats
        # 1. Try "Month Day, Year" format (e.g., "October 27, 2021")
        parsed_date <- try(as.Date(result[[date_col]][i], format = "%B %d, %Y"), silent = TRUE)
        
        # 2. If that fails, try "Day Month Year" format without comma (e.g., "29 September 2021")
        if (inherits(parsed_date, "try-error") || is.na(parsed_date)) {
          parsed_date <- try(as.Date(result[[date_col]][i], format = "%d %B %Y"), silent = TRUE)
        }
        
        # Store the result if successful
        if (!inherits(parsed_date, "try-error") && !is.na(parsed_date)) {
          # Format as human-readable string "YYYY-MM-DD"
          result$date_standardized[i] <- format(parsed_date, format = "%Y-%m-%d")
          
          # Add year and quarter for temporal analysis
          result$year[i] <- as.integer(format(parsed_date, format = "%Y"))
          month_num <- as.integer(format(parsed_date, format = "%m"))
          result$quarter[i] <- paste0(format(parsed_date, format = "%Y"), "-Q", ceiling(month_num / 3))
          
          parsed_count <- parsed_count + 1
        } else {
          diagnostics$unparsed_dates <- c(diagnostics$unparsed_dates, result[[date_col]][i])
        }
      }
    }
    
    log_message(paste("Successfully parsed", parsed_count, "dates"), "add_metadata")
    
    # Report date parsing issues
    date_issues <- which(is.na(result$date_standardized) & !is.na(result[[date_col]]))
    if (length(date_issues) > 0) {
      log_message(paste("Could not parse", length(date_issues), "dates"), "add_metadata", "WARNING")
    }
    
    # Rename standardized date column
    if ("date_standardized" %in% names(result)) {
      result <- dplyr::rename(result, date = date_standardized)
      
      # Remove original date column
      if (date_col %in% names(result) && date_col != "date") {
        result <- result[, !names(result) %in% date_col]
        log_message(paste("Removed original date column:", date_col), "add_metadata")
      }
    }
  }
  
  ## --- Geographic classification ---------------------------------------------
  log_message("Adding geographic classifications", "add_metadata")
  
  # Add SIDS indicator if provided
  if (!is.null(geo_config$sids_list)) {
    log_message("Adding SIDS classifications", "add_metadata")
    result$is_sids <- result$iso3c %in% geo_config$sids_list
  }
  
  # Add LLDC indicator if provided
  if (!is.null(geo_config$lldc_list)) {
    log_message("Adding LLDC classifications", "add_metadata")
    result$is_lldc <- result$iso3c %in% geo_config$lldc_list
  }
  
  ## --- Standardize column names and format ------------------------------------
  log_message("Standardizing column names and format", "add_metadata")
  
  # Rename columns for clarity and consistency
  result <- dplyr::rename(result,
                          country_iso3c = iso3c,
                          wb_income_level = income_level
  )
  
  # Handle country name standardization
  # If 'country' exists from World Bank join, use it to update country_name
  if ("country" %in% names(result)) {
    # Update country_name with the standardized World Bank name where available
    for (i in 1:nrow(result)) {
      if (!is.na(result$country[i])) {
        result[[match_col]][i] <- result$country[i]
      }
    }
    # Remove duplicate country column
    result$country <- NULL
  }
  
  # Ensure logical columns are actually logical
  if ("is_sids" %in% names(result) && !is.logical(result$is_sids)) {
    result$is_sids <- as.logical(result$is_sids)
  }
  
  if ("is_lldc" %in% names(result) && !is.logical(result$is_lldc)) {
    result$is_lldc <- as.logical(result$is_lldc)
  }
  
  ## --- Separate documents and metadata ----------------------------------------
  log_message("Separating documents and metadata", "add_metadata")
  
  # Extract text into documents dataframe
  documents <- data.frame(
    doc_id = result$doc_id,
    text = result[[text_column]],
    stringsAsFactors = FALSE
  )
  
  # Remove text column from metadata
  if (text_column %in% names(result)) {
    result <- result[, !names(result) %in% text_column]
  }
  
  ## --- Create config ----------------------------------------------------------
  log_message("Creating configuration", "add_metadata")
  
  # Add category map if provided, or create default
  if (!is.null(category_map)) {
    config_category_map <- category_map
  } else {
    # Use a basic default category map if none provided
    available_cols <- names(result)
    config_category_map <- list()
    
    if ("wb_income_level" %in% available_cols) {
      config_category_map$Income <- "wb_income_level"
    }
    
    if ("region" %in% available_cols) {
      config_category_map$Region <- "region"
    }
    
    geo_cols <- c("is_sids", "is_lldc")[c("is_sids", "is_lldc") %in% available_cols]
    if (length(geo_cols) > 0) {
      config_category_map$Geography <- geo_cols
    }
    
    # Add temporal dimension if available
    if ("year" %in% available_cols) {
      config_category_map$Time <- "year"  # Or could use "quarter" for finer granularity
    }
    
    log_message("Created default category_map from available columns", "add_metadata")
  }
  
  # Create complete config
  config <- list(
    metadata = result,
    category_map = config_category_map,
    date_parsed = "date" %in% names(result),
    temporal_features = c("year", "quarter")[c("year", "quarter") %in% names(result)],
    geo_features = c("is_sids", "is_lldc")[c("is_sids", "is_lldc") %in% names(result)],
    columns = names(result)
  )
  
  ## --- Create result ----------------------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create statistics metadata
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    records_processed = nrow(data),
    countries_matched = match_count,
    match_rate = round(match_count/nrow(data) * 100, 1),
    dates_parsed = parsed_count,
    date_parsing_success_rate = if(parsed_count > 0 && !is.null(date_col)) 
      round(parsed_count/sum(!is.na(data[[date_col]]))*100, 1) else NA,
    success = TRUE
  )
  
  # Log completion message
  log_message(paste("Successfully processed", nrow(result), "records"), "add_metadata")
  
  # Return standardized result with nested data structure
  return(create_result(
    data = list(
      documents = documents,
      config = config
    ),
    metadata = metadata,
    diagnostics = diagnostics
  ))
}