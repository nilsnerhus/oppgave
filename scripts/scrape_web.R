#' @title Scrape NAP documents from website
#' @description Scrapes the UNFCCC NAP Central website to extract National Adaptation Plans
#'   document links and metadata. Returns separate structures for text and metadata paths.
#'
#' @param url URL of the website to scrape (default: "https://napcentral.org/submitted-naps")
#' @param name_col Column index containing country names (default: 2)
#' @param date_col Column index containing publication dates (default: 6)
#' @param link_col Column index containing document links (default: 5)
#' @param table_index Index of the table to extract from the page (default: 1)
#' @param has_header Whether the table has a header row (default: TRUE)
#' @param exclude_countries Vector of country names to exclude (default: NULL)
#' @param output_path Path to save results (default: "data/scraped_website.rds")
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item tokens - Data frame with doc_id and pdf_link for text extraction path
#'       \item metadata - Data frame with doc_id, country_name, and date_posted for metadata path
#'     }
#'   }
#'   \item{metadata}{Processing information including timestamp and statistics}
#'   \item{diagnostics}{Information about processing issues}
#'
#' @examples
#' \dontrun{
#' nap_data <- scrape_web(exclude_countries = c("Uruguay"))
#' # Access the two data paths
#' tokens_data <- nap_data$data$tokens
#' metadata_data <- nap_data$data$metadata
#' }

scrape_web <- function(
    url = "https://napcentral.org/submitted-naps", 
    name_col = 2,
    date_col = 6,
    link_col = 5,
    table_index = 1,
    has_header = TRUE,
    exclude_countries = NULL,
    output_path = "data/scraped_website.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    excluded_countries = 0,
    skipped_rows = 0,
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input parameters", "scrape_web")
  
  if (!is.numeric(name_col) || name_col <= 0) 
    stop("name_col must be a positive number")
  if (!is.numeric(date_col) || date_col <= 0) 
    stop("date_col must be a positive number")
  if (!is.numeric(link_col) || link_col <= 0) 
    stop("link_col must be a positive number")
  if (!is.numeric(table_index) || table_index <= 0) 
    stop("table_index must be a positive number")
  
  # Convert exclude_countries to lowercase for case-insensitive matching
  exclude_countries <- tolower(exclude_countries)
  
  ## --- Setup empty tibbles ----------------------------------------------------
  # Initialize tokens data structure (for extract_pdfs path)
  tokens_data <- tibble::tibble(
    doc_id = character(),
    pdf_link = character()
  )
  
  # Initialize metadata data structure (for add_metadata path)
  metadata_data <- tibble::tibble(
    doc_id = character(),
    country_name = character(),
    date_posted = character()
  )
  
  ## --- Connect to website -----------------------------------------------------
  log_message(paste("Connecting to", url), "scrape_web")
  
  session <- try(polite::bow(
    url = url,
    user_agent = "napr (nnrorstad@gmail.com)",
    delay = 3
  ), silent = TRUE)
  
  if (inherits(session, "try-error")) {
    error_msg <- "Failed to connect to the website"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "scrape_web", "ERROR")
    
    # Return early with error information
    return(create_result(
      data = list(
        tokens = tokens_data,
        metadata = metadata_data
      ),
      metadata = list(
        url = url,
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  page_html <- try(polite::scrape(session), silent = TRUE)
  
  if (inherits(page_html, "try-error")) {
    error_msg <- "Failed to scrape the website content"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "scrape_web", "ERROR")
    
    # Return early with error information
    return(create_result(
      data = list(
        tokens = tokens_data,
        metadata = metadata_data
      ),
      metadata = list(
        url = url,
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Find and select table --------------------------------------------------
  tables <- rvest::html_nodes(page_html, "table")
  
  if (length(tables) == 0) {
    log_message("No tables found, returning empty results", "scrape_web", "WARNING")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, "No tables found on page")
    
    return(create_result(
      data = list(
        tokens = tokens_data,
        metadata = metadata_data
      ),
      metadata = list(
        url = url,
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  if (table_index > length(tables)) {
    log_message(
      paste("Table index", table_index, "too high (only", length(tables), "tables). Using first table."),
      "scrape_web", 
      "WARNING"
    )
    diagnostics$processing_issues <- c(
      diagnostics$processing_issues, 
      paste("Table index too high, defaulted to first table")
    )
    table_index <- 1
  }
  
  table_html <- tables[[table_index]]
  
  ## --- Parse table rows -------------------------------------------------------
  rows <- rvest::html_nodes(table_html, "tr")
  
  if (length(rows) == 0) {
    log_message("Selected table has no rows", "scrape_web", "WARNING")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, "Selected table has no rows")
    
    return(create_result(
      data = list(
        tokens = tokens_data,
        metadata = metadata_data
      ),
      metadata = list(
        url = url,
        timestamp = Sys.time(),
        table_count = length(tables),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  if (has_header && length(rows) > 1) {
    rows <- rows[-1]
  }
  
  log_message(paste("Processing", length(rows), "rows"), "scrape_web")
  
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
  doc_counter <- 0  # Counter for generating unique doc_ids
  
  for (i in seq_along(rows)) {
    cells <- rvest::html_nodes(rows[[i]], "td")
    
    if (length(cells) < max(name_col, date_col, link_col)) {
      log_message(paste("Skipping row", i, "(too few columns)"), "scrape_web", "WARNING")
      diagnostics$skipped_rows <- diagnostics$skipped_rows + 1
      next
    }
    
    country_name <- rvest::html_text(cells[[name_col]], trim = TRUE)
    date_posted <- rvest::html_text(cells[[date_col]], trim = TRUE)
    
    # Check if country is in the exclude list (case-insensitive)
    if (!is.null(exclude_countries) && tolower(country_name) %in% exclude_countries) {
      log_message(paste("Skipping excluded country:", country_name), "scrape_web")
      diagnostics$excluded_countries <- diagnostics$excluded_countries + 1
      next
    }
    
    pdf_link <- NULL
    
    ## Try to find direct English PDF links first
    links <- rvest::html_nodes(cells[[link_col]], "a")
    
    for (link in links) {
      href <- rvest::html_attr(link, "href")
      link_text <- rvest::html_text(link, trim = TRUE)
      
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
      spans <- rvest::html_nodes(cells[[link_col]], "a span")
      for (span in spans) {
        span_text <- rvest::html_text(span, trim = TRUE)
        if (grepl("english", span_text, ignore.case = TRUE)) {
          parent_link <- rvest::html_attr(rvest::html_node(span, xpath = ".."), "href")
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
      # Increment counter and generate doc_id
      doc_counter <- doc_counter + 1
      doc_id <- paste0("nap_", sprintf("%03d", doc_counter))
      
      # Add to tokens data (for extract_pdfs path)
      tokens_data <- tibble::add_row(
        tokens_data,
        doc_id = doc_id,
        pdf_link = pdf_link
      )
      
      # Add to metadata data (for add_metadata path)
      metadata_data <- tibble::add_row(
        metadata_data,
        doc_id = doc_id,
        country_name = country_name,
        date_posted = date_posted
      )
      
      log_message(paste("Added English PDF for:", country_name), "scrape_web")
    }
  } 
  
  ## --- Calculate processing time ----------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  ## --- Prepare and return final result ----------------------------------------
  metadata <- list(
    url = url,
    timestamp = start_time,
    processing_time_sec = processing_time,
    table_count = length(tables),
    row_count = length(rows),
    document_count = doc_counter,
    success = TRUE
  )
  
  # Return standardized result with separated data structures
  return(create_result(
    data = list(
      tokens = tokens_data,
      metadata = metadata_data
    ),
    metadata = metadata,
    diagnostics = diagnostics
  ))
}