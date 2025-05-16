#' @title Extract text from NAP PDF documents
#' @description Downloads and extracts the text content from National Adaptation Plan
#'   PDF documents using links obtained from the NAP Central website. Handles 
#'   connection errors and extraction issues.
#'
#' @param tokens_data Data frame containing doc_id and pdf_link columns from scrape_web
#' @param pdf_dir Directory to temporarily store downloaded PDFs (default: "data")
#' @param output_path Path to save the extracted text data (default: "data/pdfs.rds")
#' @param respect_robots_txt Whether to follow robots.txt rules (default: TRUE)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item tokens - Data frame with doc_id and text columns
#'     }
#'   }
#'   \item{metadata}{Processing information including timing and success rates}
#'   \item{diagnostics}{Information about errors encountered during processing}
#'
#' @examples
#' \dontrun{
#' # First scrape the website
#' web_data <- scrape_web()
#' 
#' # Then extract PDF content from the tokens data path
#' pdf_data <- extract_pdfs(web_data$data$tokens)
#' }

extract_pdfs <- function(
    tokens_data, 
    pdf_dir = "data",
    output_path = "data/pdfs.rds",
    respect_robots_txt = TRUE
)  {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    download_errors = character(),
    extraction_errors = character(),
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "extract_pdfs")
  
  required_cols <- c("doc_id", "pdf_link")
  missing_cols <- setdiff(required_cols, names(tokens_data))
  
  if (!is.data.frame(tokens_data)) {
    error_msg <- "tokens_data must be a dataframe"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "extract_pdfs", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  if (nrow(tokens_data) == 0) {
    error_msg <- "tokens_data has no rows"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "extract_pdfs", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  if (length(missing_cols) > 0) {
    error_msg <- paste("tokens_data is missing required columns:", paste(missing_cols, collapse = ", "))
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "extract_pdfs", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  if (!is.character(pdf_dir)) {
    error_msg <- "pdf_dir must be a character string"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "extract_pdfs", "ERROR")
    
    return(create_result(
      data = list(
        tokens = NULL
      ),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Create PDF directory if needed -----------------------------------------
  if (!dir.exists(pdf_dir)) {
    dir.create(pdf_dir, recursive = TRUE)
    log_message(paste("Created directory:", pdf_dir), "extract_pdfs")
  }
  
  ## --- Initialize results tibble ----------------------------------------------
  results <- tibble::tibble(
    doc_id = character(),
    text = character()
  )
  
  ## --- Process each NAP -------------------------------------------------------
  successful_downloads <- 0
  successful_extractions <- 0
  
  log_message(paste("Processing", nrow(tokens_data), "NAP documents"), "extract_pdfs")
  
  for (i in 1:nrow(tokens_data)) {
    doc_id <- tokens_data$doc_id[i]
    log_message(paste(i, "of", nrow(tokens_data), ":", doc_id), "extract_pdfs")
    
    # Create safe filename
    safe_id <- tolower(gsub("[^a-zA-Z0-9]", "_", doc_id))
    pdf_path <- file.path(pdf_dir, paste0(safe_id, ".pdf"))
    
    # Get PDF URL and fix if it's a relative URL
    pdf_url <- tokens_data$pdf_link[i]
    if (!grepl("^http", pdf_url)) {
      pdf_url <- paste0("https://napcentral.org", pdf_url)
    }
    
    # Download PDF using polite
    log_message("Downloading PDF...", "extract_pdfs")
    download_success <- FALSE
    
    tryCatch({
      # Use polite to bow to the domain and check robots.txt
      session <- polite::bow(
        url = pdf_url,
        user_agent = "napr (nnrorstad@gmail.com)"
      )
      
      # Get the delay recommended by robots.txt
      delay <- session$delay
      
      # Wait according to the polite delay if needed
      Sys.sleep(delay)
      
      # Use httr for the actual download
      response <- httr::GET(
        url = pdf_url,
        httr::write_disk(pdf_path, overwrite = TRUE),
        httr::user_agent("napr (nnrorstad@gmail.com)")
      )
      
      # Check if download was successful
      if (httr::status_code(response) == 200 && file.exists(pdf_path) && file.size(pdf_path) > 0) {
        download_success <- TRUE
        successful_downloads <- successful_downloads + 1
        log_message("Download successful!", "extract_pdfs")
      } else {
        error_msg <- paste("Failed to download PDF for", doc_id)
        diagnostics$download_errors <- c(diagnostics$download_errors, error_msg)
        log_message(error_msg, "extract_pdfs", "WARNING")
      }
    }, error = function(e) {
      error_msg <- paste("Error downloading PDF for", doc_id, ":", e$message)
      diagnostics$download_errors <- c(diagnostics$download_errors, error_msg)
      log_message(paste("Download error:", e$message), "extract_pdfs", "WARNING")
    })
    
    # Extract text if download was successful
    if (download_success) {
      tryCatch({
        log_message("Extracting text...", "extract_pdfs")
        text <- pdftools::pdf_text(pdf_path)
        
        # Add to results
        results <- tibble::add_row(results,
                                   doc_id = doc_id,
                                   text = paste(text, collapse = "\n\n")
        )
        
        successful_extractions <- successful_extractions + 1
        log_message("Text extraction successful!", "extract_pdfs")
        
        # Delete PDF after extraction if needed
        if (file.remove(pdf_path)) {
          log_message("PDF deleted successfully", "extract_pdfs")
        } else {
          log_message("Could not delete PDF file: ", pdf_path, "extract_pdfs", "WARNING")
        }
      }, error = function(e) {
        error_msg <- paste("Error extracting text from PDF for", doc_id, ":", e$message)
        diagnostics$extraction_errors <- c(diagnostics$extraction_errors, error_msg)
        log_message(paste("Text extraction error:", e$message), "extract_pdfs", "WARNING")
      })
    }
  }
  
  ## --- Calculate processing time ----------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  ## --- Report errors ----------------------------------------------------------
  total_errors <- length(diagnostics$download_errors) + length(diagnostics$extraction_errors)
  
  if (total_errors > 0) {
    log_message(paste(total_errors, "errors encountered:"), "extract_pdfs", "WARNING")
    
    if (length(diagnostics$download_errors) > 0) {
      log_message(paste("Download errors:", length(diagnostics$download_errors)), "extract_pdfs", "WARNING")
      for (i in 1:min(5, length(diagnostics$download_errors))) {
        log_message(paste("  -", diagnostics$download_errors[i]), "extract_pdfs")
      }
      if (length(diagnostics$download_errors) > 5) {
        log_message(paste("  - ...and", length(diagnostics$download_errors) - 5, "more"), "extract_pdfs")
      }
    }
    
    if (length(diagnostics$extraction_errors) > 0) {
      log_message(paste("Extraction errors:", length(diagnostics$extraction_errors)), "extract_pdfs", "WARNING")
      for (i in 1:min(5, length(diagnostics$extraction_errors))) {
        log_message(paste("  -", diagnostics$extraction_errors[i]), "extract_pdfs")
      }
      if (length(diagnostics$extraction_errors) > 5) {
        log_message(paste("  - ...and", length(diagnostics$extraction_errors) - 5, "more"), "extract_pdfs")
      }
    }
  }
  
  ## --- Prepare and return final result ----------------------------------------
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    total_documents = nrow(tokens_data),
    successful_downloads = successful_downloads,
    successful_extractions = successful_extractions,
    download_success_rate = round(successful_downloads / nrow(tokens_data) * 100, 1),
    extraction_success_rate = if (successful_downloads > 0) round(successful_extractions / successful_downloads * 100, 1) else 0,
    overall_success_rate = round(successful_extractions / nrow(tokens_data) * 100, 1),
    success = nrow(results) > 0
  )
  
  # Log completion message
  log_message(paste("Successfully processed", successful_extractions, "of", nrow(tokens_data), "NAPs", 
                    sprintf("(%.1f%%)", metadata$overall_success_rate)), "extract_pdfs")
  
  # Return standardized result
  return(create_result(
    data = list(
      tokens = results
    ),
    metadata = metadata,
    diagnostics = diagnostics
  ))
}