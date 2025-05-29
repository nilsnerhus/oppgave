#' @title Get UN Geographic Classifications
#' @description Scrapes current SIDS and LLDC country names from UN OHRLLS websites.
#'   Returns raw country names for add_metadata() to process with its existing lookup logic.
#'   
#' @return A list containing:
#'   \item{sids_countries}{Character vector of SIDS country names as listed on UN website}
#'   \item{lldc_countries}{Character vector of LLDC country names as listed on UN website}
#'   \item{metadata}{Information about the scraping process}
#'   \item{diagnostics}{Issues encountered during scraping}
get_un_classifications <- function() {
  
  ## --- Setup & Initialization -----------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics
  diagnostics <- list(
    processing_issues = character(),
    scraping_details = list()
  )
  
  log_message("Scraping UN geographic classifications from official sources", "get_un_classifications")
  
  ## --- Scrape SIDS list from UN OHRLLS -------------------------------------
  log_message("Scraping SIDS list from UN OHRLLS", "get_un_classifications")
  
  sids_countries <- tryCatch({
    # Use polite to respect robots.txt
    session <- polite::bow("https://www.un.org/ohrlls/content/list-sids")
    page <- polite::scrape(session)
    
    # Extract all text content
    page_text <- rvest::html_text(page)
    
    # Split into lines and clean
    lines <- unlist(strsplit(page_text, "\n"))
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    
    # Find lines that contain numbered country entries (pattern: "number. Country Name")
    country_pattern <- "^[0-9]+\\.\\s+(.+)$"
    country_lines <- lines[grepl(country_pattern, lines)]
    
    # Extract country names (everything after "number. ")
    countries <- gsub(country_pattern, "\\1", country_lines)
    
    # Clean up: remove asterisks and trailing text, trim whitespace
    countries <- gsub("\\*.*$", "", countries)
    countries <- trimws(countries)
    countries <- countries[countries != ""]
    
    log_message(paste("Successfully scraped", length(countries), "SIDS countries"), "get_un_classifications")
    countries
    
  }, error = function(e) {
    error_msg <- paste("Failed to scrape SIDS list:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "get_un_classifications", "ERROR")
    stop(error_msg)
  })
  
  ## --- Scrape LLDC list from UN OHRLLS -------------------------------------
  log_message("Scraping LLDC list from UN OHRLLS", "get_un_classifications")
  
  lldc_countries <- tryCatch({
    # Use polite to respect robots.txt
    session <- polite::bow("https://www.un.org/ohrlls/content/list-lldcs")
    page <- polite::scrape(session)
    
    # The LLDC countries are in a table within div.pane-content
    # Extract all links within the table that point to country profiles
    country_links <- rvest::html_nodes(page, "div.pane-content table a[href*='unctadstat.unctad.org']")
    
    # Extract the text content of these links (which are the country names)
    countries <- rvest::html_text(country_links)
    
    # Clean up the country names
    countries <- trimws(countries)
    countries <- countries[countries != "" & nchar(countries) > 2]
    
    # Remove duplicates
    countries <- unique(countries)
    
    log_message(paste("Successfully scraped", length(countries), "LLDC countries from UN table"), "get_un_classifications")
    countries
    
  }, error = function(e) {
    error_msg <- paste("Failed to scrape LLDC list:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "get_un_classifications", "ERROR")
    stop(error_msg)
  })
  
  ## --- Create result --------------------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  return(list(
    sids_countries = sids_countries,
    lldc_countries = lldc_countries,
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      sids_count = length(sids_countries),
      lldc_count = length(lldc_countries),
      source = "UN OHRLLS official websites",
      success = TRUE
    ),
    diagnostics = diagnostics
  ))
}