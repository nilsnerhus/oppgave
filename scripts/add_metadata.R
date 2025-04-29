#' @title Add metadata to NAP country data
#' @description Enhances NAP country data with standardized metadata from World Bank 
#'   and custom country classifications. Standardizes country names, adds ISO codes,
#'   parses dates, and adds region, income level, and special status indicators (LLDC,
#'   SIDS, LDC).
#'   
#' @param data A dataframe containing country NAP data
#' @param match_col Column name in data containing country names (default: "country_name")
#' @param date_col Column name in data containing dates (default: "date_posted")
#' @param lldc_list Character vector of ISO3C codes for landlocked developing countries
#' @param sids_list Character vector of ISO3C codes for small island developing states
#' @param ldc_list Character vector of ISO3C codes for least developed countries
#' @param output_path File path to save the processed data (default: "data/nap_data.rds")
#' 
#' @return A list containing:
#'   \item{data}{Enhanced dataframe with added metadata}
#'   \item{metadata}{Processing statistics and timestamps}
#'   \item{diagnostics}{Validation results and issues encountered}
#'
#' @examples
#' \dontrun{
#' # Add metadata to NAP data
#' nap_data <- add_metadata(pdfs, sids_list = sids, lldc_list = lldc, ldc_list = ldc)
#' }

add_metadata <- function(
    data, 
    match_col = "country_name",
    date_col = "date_posted",
    lldc_list = NULL,          # Optional LLDC list
    sids_list = NULL,          # Optional SIDS list
    ldc_list = NULL,           # Optional LDC list
    output_path = "data/nap_data.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    unmatched_countries = character(),
    unparsed_dates = character(),
    issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "add_metadata")
  
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
  
  if (!match_col %in% names(data)) {
    error_msg <- paste("Input is missing required column:", match_col)
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
  
  # Create a copy of the input data
  result <- data
  
  ## --- Fetch World Bank data --------------------------------------------------
  log_message("Fetching World Bank country data", "add_metadata")
  
  wb_countries <- time_operation({
    tryCatch({
      wb_data <- wbstats::wb_countries()
      wb_data <- dplyr::select(wb_data, iso3c, country, region, income_level)
      wb_data
    }, error = function(e) {
      error_msg <- paste("Failed to fetch World Bank data:", e$message)
      diagnostics$issues <<- c(diagnostics$issues, error_msg)
      log_message(error_msg, "add_metadata", "ERROR")
      NULL
    })
  }, "add_metadata")
  
  if (is.null(wb_countries)) {
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = start_time,
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Build country lookup table ---------------------------------------------
  log_message("Building country name lookup table", "add_metadata")
  
  country_lookup <- time_operation({
    # First select columns from WB data
    lookup_wb <- dplyr::select(wb_countries, country, iso3c)
    
    # Create custom variations dataframe
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
  }, "add_metadata")
  
  ## --- Match countries to ISO codes -------------------------------------------
  log_message("Matching countries to ISO codes", "add_metadata")
  
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
  
  ## --- Track unmatched countries ----------------------------------------------
  unmatched <- which(is.na(result$iso3c))
  if (length(unmatched) > 0) {
    diagnostics$unmatched_countries <- result[[match_col]][unmatched]
    log_message(paste("Could not match", length(unmatched), "countries to ISO codes"), "add_metadata", "WARNING")
    
    for (i in 1:min(length(unmatched), 5)) {
      log_message(paste("  -", result[[match_col]][unmatched[i]]), "add_metadata")
    }
    
    if (length(unmatched) > 5) {
      log_message(paste("  - ...and", length(unmatched) - 5, "more"), "add_metadata")
    }
  }
  
  ## --- Parse dates if needed --------------------------------------------------
  parsed_count <- 0
  if (!is.null(date_col) && date_col %in% names(result)) {
    log_message(paste("Standardizing dates from", date_col, "column"), "add_metadata")
    
    # Initialize date column
    result$date_standardized <- NA
    
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
      log_message(paste("Could not parse", length(date_issues), "dates:"), "add_metadata", "WARNING")
      for (i in 1:min(5, length(date_issues))) {
        log_message(paste("  -", result[[date_col]][date_issues[i]]), "add_metadata")
      }
      if (length(date_issues) > 5) {
        log_message(paste("  - ...and", length(date_issues) - 5, "more"), "add_metadata")
      }
    }
    
    # Rename standardized date column
    if ("date_standardized" %in% names(result)) {
      result <- dplyr::rename(result, date = date_standardized)
    }
  }
  
  ## --- Join with World Bank country data --------------------------------------
  log_message("Joining with World Bank country data", "add_metadata")
  
  result <- dplyr::left_join(result, wb_countries, by = "iso3c")
  
  ## --- Add special country classifications ------------------------------------
  
  # Add LLDC indicator if provided
  if (!is.null(lldc_list)) {
    log_message("Adding LLDC classifications", "add_metadata")
    result$is_lldc <- result$iso3c %in% lldc_list
  }
  
  # Add SIDS indicator if provided
  if (!is.null(sids_list)) {
    log_message("Adding SIDS classifications", "add_metadata")
    result$is_sids <- result$iso3c %in% sids_list
  }
  
  # Add LDC indication if provided
  if (!is.null(ldc_list)) {
    log_message("Adding LDC classifications", "add_metadata")
    result$is_ldc <- result$iso3c %in% ldc_list
  }
  
  ## --- Rename and reorder columns ---------------------------------------------
  log_message("Standardizing column names and order", "add_metadata")
  
  # Rename columns for clarity and standardize names
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
  
  # Reorder columns in a more intuitive way
  col_order <- c(
    match_col,           # country_name first
    "country_iso3c",     # followed by ISO code
    if(!is.null(date_col) && "date" %in% names(result)) "date" else NULL,
    "region",            # geographical info
    "wb_income_level",   # economic classification
    "is_lldc",           # special designations
    "is_sids",
    "is_ldc", 
    if("pdf_text" %in% names(result)) "pdf_text" else NULL  # content at the end if present
  )
  
  # Keep only columns that exist in the result
  col_order <- intersect(col_order, names(result))
  result <- result[, col_order]
  
  ## --- Calculate processing time ----------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  ## --- Save results if specified ----------------------------------------------
  if (!is.null(output_path)) {
    saveRDS(result, output_path)
    log_message(paste("Saved metadata to", output_path), "add_metadata")
  }
  
  ## --- Prepare and return final result ----------------------------------------
  metadata <- list(
    records_processed = nrow(data),
    countries_matched = match_count,
    dates_parsed = parsed_count,
    date_parsing_success_rate = if(parsed_count > 0 && !is.null(date_col)) 
      round(parsed_count/sum(!is.na(data[[date_col]]))*100, 1) else NA,
    timestamp = start_time,
    processing_time_sec = processing_time,
    success = TRUE
  )
  
  # Log completion message
  success_msg <- paste("Successfully processed", nrow(result), "records")
  if (!is.null(date_col) && "date" %in% names(result)) {
    success_msg <- paste0(
      success_msg, 
      " (", parsed_count, "/", sum(!is.na(data[[date_col]])), 
      " dates parsed, ", metadata$date_parsing_success_rate, "%)"
    )
  }
  log_message(success_msg, "add_metadata")
  
  # Return standardized result
  return(create_result(
    data = result,
    metadata = metadata,
    diagnostics = diagnostics
  ))
}