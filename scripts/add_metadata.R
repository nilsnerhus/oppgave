# Purpose: Add metadata to a table based on the country name

library(dplyr)
library(wbstats)

add_metadata <- function(
    data, 
    match_col = "country_name",
    date_col = "date_posted",
    lldc_list = NULL,          # Optional LLDC list
    sids_list = NULL,          # Optional SIDS list
    ldc_list = NULL,           # Optional LDC list
    output_file = "data/nap_data.rds"
) {
  # Input validation
  if (!is.data.frame(data)) stop("Input must be a dataframe")
  if (nrow(data) == 0) stop("Input dataframe has no rows")
  if (!match_col %in% names(data)) stop(paste("Input is missing required column:", match_col))
  
  # Create a copy of the input data
  result <- data
  
  # Get World Bank country data for basic metadata
  message("Fetching World Bank country data...")
  wb_countries <- wbstats::wb_countries() %>%
    select(iso3c, country, region, income_level)
  
  # Add common country name variations to improve matching
  country_lookup <- wb_countries %>%
    select(country, iso3c) %>%
    bind_rows(
      tibble::tibble(
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
    )
  
  # Match countries to ISO codes
  result$iso3c <- NA_character_
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
    }
  }
  
  # Report unmatched countries
  unmatched <- which(is.na(result$iso3c))
  if (length(unmatched) > 0) {
    message("Could not match these countries to ISO codes:")
    for (i in unmatched) {
      message("  - ", result[[match_col]][i])
    }
  }
  
  # Parse dates if a date column is specified
  if (!is.null(date_col) && date_col %in% names(result)) {
    message("Standardizing dates from '", date_col, "' column...")
    
    # Simple function to parse dates in various formats
    result$date_standardized <- NA
    parsed_count <- 0
    
    message("Attempting to parse", nrow(result), "dates")
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
        }
      }
    }
    message("  • Successfully parsed", parsed_count, "dates")
    
    # Report date parsing issues as before...
  }
    
    # Report date parsing issues
    date_issues <- which(is.na(result$date_standardized) & !is.na(result[[date_col]]))
    if (length(date_issues) > 0) {
      message("  • Could not parse", length(date_issues), "dates:")
      for (i in 1:min(5, length(date_issues))) {  # Show at most 5 examples
        message("    - ", result[[date_col]][date_issues[i]])
      }
      if (length(date_issues) > 5) {
        message("    - ... and ", length(date_issues) - 5, " more")
      }
    }
  
  # Join with World Bank country data
  result <- result %>%
    left_join(wb_countries, by = "iso3c")
  
  # Add LLDC indicator if provided
  if (!is.null(lldc_list)) {
    message("Adding LLDC classifications...")
    result$is_lldc <- result$iso3c %in% lldc_list
  }
  
  # Add SIDS indicator if provided
  if (!is.null(sids_list)) {
    message("Adding SIDS classifications...")
    result$is_sids <- result$iso3c %in% sids_list
  }
  
  # Add LDC indication if provided
  if (!is.null(ldc_list)) {
    message("Adding LDC classifications...")
    result$is_ldc <- result$iso3c %in% ldc_list
  }
  
# Rename columns for clarity and standardize names
  result <- result %>%
    rename(
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
  
  if (!is.null(date_col) && "date_standardized" %in% names(result)) {
    result <- result %>%
      rename(date = date_standardized)
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
  
  # Save results if output_file is specified
  if (!is.null(output_file)) {
    # Create directories if needed
    dir_path <- dirname(output_file)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    saveRDS(result, output_file)
    message("Saved metadata to ", output_file)
  }
  
  # Clear completion message
  message("✓ Metadata processing complete!")
  message("  • ", nrow(result), " records processed")
  if (!is.null(date_col)) {
    date_success <- sum(!is.na(result$date))
    message("  • ", date_success, "/", nrow(result), " dates successfully parsed (", 
            round(date_success/nrow(result)*100), "%)")
  }
  return(result)
}