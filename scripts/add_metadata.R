#' @title Add metadata to NAP country data 
#' @description Takes scraped web data and UN classifications to create enhanced metadata
#'   for the topic modeling pipeline. Simple, focused interface.
#'   
#' @param web_data Result from scrape_web() containing scraped NAP metadata
#' @param un_classifications Result from get_un_classifications() containing SIDS/LLDC lists
#' @param time Named vector of temporal breakpoints (optional, e.g., c("Early" = 2018, "Late" = Inf))
#' 
#' @return Enhanced metadata ready for the topic modeling pipeline
add_metadata <- function(web_data, un_classifications, time = NULL) {
  
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics
  diagnostics <- list(
    unmatched_countries = character(),
    unparsed_dates = character(),
    processing_issues = character()
  )
  
  log_message("Processing NAP metadata with UN classifications", "add_metadata")
  
  ## --- Input validation -------------------------------------------------------
  if (!is.list(web_data) || !"data" %in% names(web_data) || !"metadata" %in% names(web_data$data)) {
    stop("web_data must be result from scrape_web() with metadata component")
  }
  
  if (!is.list(un_classifications) || !all(c("sids_countries", "lldc_countries") %in% names(un_classifications))) {
    stop("un_classifications must contain sids_countries and lldc_countries")
  }
  
  # Extract input data
  result <- web_data$data$metadata
  sids_countries <- un_classifications$sids_countries
  lldc_countries <- un_classifications$lldc_countries
  
  if (nrow(result) == 0) {
    stop("No metadata to process")
  }
  
  ## --- Country name to ISO code conversion ------------------------------------
  log_message("Converting country names to ISO codes", "add_metadata")
  
  # Convert country names to ISO codes using countrycode
  result$iso3c <- countrycode::countrycode(
    sourcevar = result$country_name,
    origin = "country.name",
    destination = "iso3c",
    warn = TRUE
  )
  
  # Track matching success
  unmatched <- which(is.na(result$iso3c))
  match_count <- nrow(result) - length(unmatched)
  
  if (length(unmatched) > 0) {
    diagnostics$unmatched_countries <- result$country_name[unmatched]
    log_message(paste("Could not match", length(unmatched), "countries"), "add_metadata", "WARNING")
  }
  
  ## --- Add World Bank classifications -----------------------------------------
  log_message("Adding World Bank income and region data", "add_metadata")
  
  wb_countries <- tryCatch({
    wb_data <- wbstats::wb_countries()
    dplyr::select(wb_data, iso3c, country, region, income_level)
  }, error = function(e) {
    stop(paste("Failed to fetch World Bank data:", e$message))
  })
  
  # Join with World Bank data
  result <- dplyr::left_join(result, wb_countries, by = "iso3c")
  
  # Update country names with WB standardized names where available
  wb_names <- !is.na(result$country)
  result$country_name[wb_names] <- result$country[wb_names]
  result$country <- NULL
  
  ## --- Parse dates to extract year --------------------------------------------
  log_message("Parsing dates to extract year", "add_metadata")
  
  # Try the most common format first: "Month Day, Year" (e.g., "October 27, 2021")
  result$year <- as.integer(format(as.Date(result$date_posted, format = "%B %d, %Y"), "%Y"))
  
  # For any that failed, try alternative format: "Day Month Year" (e.g., "27 October 2021")
  failed_dates <- is.na(result$year)
  if (any(failed_dates)) {
    result$year[failed_dates] <- as.integer(format(as.Date(result$date_posted[failed_dates], format = "%d %B %Y"), "%Y"))
  }
  
  # Track parsing success
  parsed_count <- sum(!is.na(result$year))
  unparsed <- which(is.na(result$year) & !is.na(result$date_posted))
  if (length(unparsed) > 0) {
    diagnostics$unparsed_dates <- result$date_posted[unparsed]
    log_message(paste("Could not parse", length(unparsed), "dates"), "add_metadata", "WARNING")
  }
  
  log_message(paste("Successfully parsed", parsed_count, "years"), "add_metadata")
  
  ## --- Apply temporal grouping (if provided) ----------------------------------
  if (!is.null(time)) {
    log_message("Applying temporal grouping", "add_metadata")
    
    if (is.null(names(time)) || any(names(time) == "")) {
      warning_msg <- "time parameter must be named vector (e.g., c('Early' = 2019, 'Late' = Inf))"
      diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
      log_message(warning_msg, "add_metadata", "WARNING")
    } else {
      result$time_period <- NA_character_
      
      for (i in 1:nrow(result)) {
        if (!is.na(result$year[i])) {
          doc_year <- result$year[i]
          
          # Find appropriate time group
          for (j in 1:length(time)) {
            if (doc_year <= time[j]) {
              result$time_period[i] <- names(time)[j]
              break
            }
          }
          
          # If no group found, assign to last group
          if (is.na(result$time_period[i])) {
            result$time_period[i] <- names(time)[length(time)]
          }
        }
      }
      
      log_message(paste("Created", length(unique(result$time_period)), "temporal groups"), "add_metadata")
    }
  }
  
  ## --- Add geographic classifications -----------------------------------------
  log_message("Adding UN geographic classifications", "add_metadata")
  
  # Convert UN country names to ISO codes
  sids_iso <- countrycode::countrycode(sids_countries, "country.name", "iso3c", warn = FALSE)
  lldc_iso <- countrycode::countrycode(lldc_countries, "country.name", "iso3c", warn = FALSE)
  
  # Remove any NAs from conversion
  sids_iso <- sids_iso[!is.na(sids_iso)]
  lldc_iso <- lldc_iso[!is.na(lldc_iso)]
  
  # Create binary indicators
  result$is_sids <- result$iso3c %in% sids_iso
  result$is_lldc <- result$iso3c %in% lldc_iso
  
  log_message(paste("Classified", sum(result$is_sids, na.rm = TRUE), "SIDS and", 
                    sum(result$is_lldc, na.rm = TRUE), "LLDC countries"), "add_metadata")
  
  ## --- Standardize column names -----------------------------------------------
  result <- dplyr::rename(result,
                          country_iso3c = iso3c,
                          wb_income_level = income_level)
  
  # Remove original date column
  result$date_posted <- NULL
  
  ## --- Create category map ----------------------------------------------------
  category_map <- list(
    Global = "global_category",
    Income = "wb_income_level", 
    Region = "region", 
    Geography = c("is_sids", "is_lldc")
  )
  
  # Add temporal category if grouping was applied
  if (!is.null(time) && "time_period" %in% names(result)) {
    category_map[["Time"]] <- "time_period"
  }
  
  # Add global category column
  result$global_category <- "Global"
  
  ## --- Finalize results -------------------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    records_processed = nrow(web_data$data$metadata),
    countries_matched = match_count,
    match_rate = round(match_count/nrow(web_data$data$metadata) * 100, 1),
    dates_parsed = parsed_count,
    sids_classified = sum(result$is_sids, na.rm = TRUE),
    lldc_classified = sum(result$is_lldc, na.rm = TRUE),
    temporal_grouping = !is.null(time),
    success = TRUE
  )
  
  log_message(paste("Successfully processed", nrow(result), "records"), "add_metadata")
  
  return(create_result(
    data = list(
      metadata = result,
      config = list(category_map = category_map)
    ),
    metadata = metadata,
    diagnostics = diagnostics
  ))
}