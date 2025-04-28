# Purpose: Scraping public websites and identifying their pdf-links

library(dplyr)
library(polite)
library(rvest)
library(httr)

scrape_web <- function(
    url = "https://napcentral.org/submitted-naps", 
    name_col = 2,
    date_col = 6,
    link_col = 5,
    table_index = 1,
    has_header = TRUE,
    exclude_countries = NULL,
    output_file = "data/scraped_website.rds"
) {
  
  ## --- Input validation -------------------------------------------------------
  if (!is.numeric(name_col) || name_col <= 0) stop("name_col must be a positive number")
  if (!is.numeric(date_col) || date_col <= 0) stop("date_col must be a positive number")
  if (!is.numeric(link_col) || link_col <= 0) stop("link_col must be a positive number")
  if (!is.numeric(table_index) || table_index <= 0) stop("table_index must be a positive number")
  
  # Convert exclude_countries to lowercase for case-insensitive matching
  exclude_countries <- tolower(exclude_countries)
  
  ## --- Setup empty tibble -----------------------------------------------------
  results <- tibble::tibble(
    country_name = character(),
    date_posted = character(),
    pdf_link = character()
  )
  
  ## --- Connect to website -----------------------------------------------------
  message("Connecting to ", url)
  session <- try(polite::bow(
    url = url,
    user_agent = "napr (nnrorstad@gmail.com)",
    delay = 3
  ), silent = TRUE)
  
  if (inherits(session, "try-error")) {
    stop("Failed to connect to the website.")
  }
  
  page_html <- try(polite::scrape(session), silent = TRUE)
  
  if (inherits(page_html, "try-error")) {
    stop("Failed to scrape the website content.")
  }
  
  ## --- Find and select table --------------------------------------------------
  tables <- page_html %>% rvest::html_nodes("table")
  
  if (length(tables) == 0) {
    message("No tables found, returning empty results")
    return(results)
  }
  
  if (table_index > length(tables)) {
    message("Table index ", table_index, " too high (only ", length(tables), " tables). Using first table.")
    table_index <- 1
  }
  
  table_html <- tables[[table_index]]
  
  ## --- Parse table rows -------------------------------------------------------
  rows <- table_html %>% rvest::html_nodes("tr")
  
  if (length(rows) == 0) {
    message("Selected table has no rows")
    return(results)
  }
  
  if (has_header && length(rows) > 1) {
    rows <- rows[-1]
  }
  
  message("Processing ", length(rows), " rows")
  
  ## --- Helper inside: make absolute URL ---------------------------------------
  make_absolute_url <- function(href, base_url) {
    if (!grepl("^https?://", href)) {
      if (startsWith(href, "/")) {
        domain <- sub("^(https?://[^/]+).*", "\\1", base_url)
        return(paste0(domain, href))
      } else {
        dir <- dirname(base_url)
        return(file.path(dir, href))
      }
    }
    return(href)
  }
  
  ## --- Loop through rows ------------------------------------------------------
  for (i in seq_along(rows)) {
    cells <- rows[[i]] %>% rvest::html_nodes("td")
    
    if (length(cells) < max(name_col, date_col, link_col)) {
      message("Skipping row ", i, " (too few columns)")
      next
    }
    
    country_name <- cells[[name_col]] %>% rvest::html_text(trim = TRUE)
    date_posted <- cells[[date_col]] %>% rvest::html_text(trim = TRUE)
    
    # Check if country is in the exclude list (case-insensitive)
    if (!is.null(exclude_countries) && tolower(country_name) %in% exclude_countries) {
      message("Skipping excluded country: ", country_name)
      next
    }
    
    pdf_link <- NULL
    
    ## Try to find direct English PDF links first
    links <- cells[[link_col]] %>% rvest::html_nodes("a")
    
    for (link in links) {
      href <- link %>% rvest::html_attr("href")
      link_text <- link %>% rvest::html_text(trim = TRUE)
      
      if (is.na(href) || href == "") next
      
      is_pdf <- grepl("\\.pdf$", href, ignore.case = TRUE) || 
        grepl("pdf|document", link_text, ignore.case = TRUE)
      
      # Only match English PDFs
      is_match <- is_pdf && grepl("english", link_text, ignore.case = TRUE)
      
      if (is_match) {
        pdf_link <- make_absolute_url(href, url)
        break
      }
    }
    
    ## If no link found, try spans inside links
    if (is.null(pdf_link)) {
      spans <- cells[[link_col]] %>% rvest::html_nodes("a span")
      for (span in spans) {
        span_text <- span %>% rvest::html_text(trim = TRUE)
        if (grepl("english", span_text, ignore.case = TRUE)) {
          parent_link <- span %>% rvest::html_node(xpath = "..") %>% rvest::html_attr("href")
          if (!is.na(parent_link) && parent_link != "") {
            is_pdf <- grepl("\\.pdf$", parent_link, ignore.case = TRUE)
            if (is_pdf) {
              pdf_link <- make_absolute_url(parent_link, url)
              break
            }
          }
        }
      }
    }
    
    ## Save result if found
    if (!is.null(pdf_link)) {
      results <- tibble::add_row(
        results,
        country_name = country_name,
        date_posted = date_posted,
        pdf_link = pdf_link
      )
      message("Added English PDF for: ", country_name)
    }
  } 
  
  ## --- Save results -----------------------------------------------------------
  if (nrow(results) > 0) {
    saveRDS(results, output_file)
    message("Saved ", nrow(results), " entries to ", output_file)
  } else {
    message("No entries to save")
  }
  
  ## --- Return -----------------------------------------------------------------
  return(results)
}