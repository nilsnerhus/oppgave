# Add metadata to NAP country data 
add_metadata <- function(web_data, un_classifications, time = NULL) {
  
  start_time <- Sys.time()
  
  log_message("Processing NAP metadata", "add_metadata")
  
  # Extract input data
  result <- web_data$data$metadata
  sids_countries <- un_classifications$sids_countries
  lldc_countries <- un_classifications$lldc_countries
  
  ## --- Add ISO codes and World Bank data ------------------------------------
  # Convert to ISO codes
  result$iso3c <- countrycode::countrycode(result$country_name, "country.name", "iso3c", warn = FALSE)
  
  # Add World Bank classifications
  wb_data <- wbstats::wb_countries()
  wb_countries <- dplyr::select(wb_data, iso3c, country, region, income_level)
  result <- dplyr::left_join(result, wb_countries, by = "iso3c")
  
  # Use World Bank names where available
  result$country_name[!is.na(result$country)] <- result$country[!is.na(result$country)]
  
  ## --- Process dates and time grouping -------------------------------------
  # Extract year - try format 1: "Month Day, Year"
  result$year <- as.integer(format(as.Date(result$date_posted, format = "%B %d, %Y"), "%Y"))
  
  # Try format 2 for failed dates: "Day Month Year"
  failed_dates <- is.na(result$year)
  if (any(failed_dates)) {
    result$year[failed_dates] <- as.integer(format(as.Date(result$date_posted[failed_dates], format = "%d %B %Y"), "%Y"))
  }
  
  # Apply time grouping if provided
  if (!is.null(time)) {
    result$time_period <- NA_character_
    for (i in 1:nrow(result)) {
      if (!is.na(result$year[i])) {
        for (j in 1:length(time)) {
          if (result$year[i] <= time[j]) {
            result$time_period[i] <- names(time)[j]
            break
          }
        }
      }
    }
    
    # Assign default time period for documents with unparseable dates
    missing_time <- is.na(result$time_period) & !is.na(result$doc_id)
    if (any(missing_time)) {
      result$time_period[missing_time] <- "Middle"  # Default fallback
      log_message(paste("Assigned default time period to", sum(missing_time), "documents with unparseable dates"), "add_metadata", "WARNING")
    }
  }
  
  ## --- Add geographic classifications --------------------------------------
  # Convert UN lists to ISO codes
  sids_iso <- countrycode::countrycode(sids_countries, "country.name", "iso3c", warn = FALSE)
  lldc_iso <- countrycode::countrycode(lldc_countries, "country.name", "iso3c", warn = FALSE)
  sids_iso <- sids_iso[!is.na(sids_iso)]
  lldc_iso <- lldc_iso[!is.na(lldc_iso)]
  
  # Create geography column
  result$geography <- dplyr::case_when(
    result$iso3c %in% sids_iso ~ "SIDS", 
    result$iso3c %in% lldc_iso ~ "LLDC",
    TRUE ~ NA_character_
  )
  
  # Convert geography NAs to "Other"
  result$geography[is.na(result$geography)] <- "Other"
  
  ## --- Clean up and finalize -----------------------------------------------
  result <- dplyr::select(result, -country, -date_posted)
  result$global_category <- "Global"
  
  log_message(paste("Processed", nrow(result), "records"), "add_metadata")
  
  return(create_result(
    data = list(metadata = result),
    metadata = list(
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )
  ))
}