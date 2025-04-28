# Purpose: Extracting the text from pdfs where the link is known

library(polite)
library(httr)
library(pdftools)


extract_pdfs <- function(
    scraped_website, 
    pdf_dir = "data",
    output_file = "data/pdfs.rds",
    respect_robots_txt = TRUE
)  {
  
  ## --- Input validation -------------------------------------------------------
  required_cols <- c("country_name", "pdf_link", "date_posted")
  missing_cols <- setdiff(required_cols, names(scraped_website))
  
  if (!is.data.frame(scraped_website)) stop("scraped_website must be a dataframe")
  if (nrow(scraped_website) == 0) stop("scraped_website has no rows")
  if (length(missing_cols) > 0) stop("scraped_website is missing required columns: ", paste(missing_cols, collapse = ", "))
  if (!is.character(pdf_dir)) stop("pdf_dir must be a character string")
  
  # Create PDF directory if it doesn't exist
  if (!dir.exists(pdf_dir)) {
    dir.create(pdf_dir, recursive = TRUE)
    message("Created directory: ", pdf_dir)
  }
  
  # Initialize results tibble and error collection
  results <- tibble::tibble(
    country_name = character(),
    date_posted = character(),
    pdf_text = character()
  )
  
  errors <- character()
  
  # Process each NAP
  for (i in 1:nrow(scraped_website)) {
    country <- scraped_website$country_name[i]
    message(i, " of ", nrow(scraped_website), ": ", country)
    
    # Create safe filename
    safe_country <- tolower(gsub("[^a-zA-Z0-9]", "_", country))
    pdf_path <- file.path(pdf_dir, paste0(safe_country, ".pdf"))
    
    # Get PDF URL and fix if it's a relative URL
    pdf_url <- scraped_website$pdf_link[i]
    if (!grepl("^http", pdf_url)) {
      pdf_url <- paste0("https://napcentral.org", pdf_url)
    }
    
    # Download PDF using polite
    message("Downloading PDF...")
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
        message("Download successful!")
      } else {
        errors <- c(errors, paste("Failed to download PDF for", country))
        message("Download failed")
      }
    }, error = function(e) {
      errors <- c(errors, paste("Error downloading PDF for", country, ":", e$message))
      message("Download error: ", e$message)
    })
    
    # Extract text if download was successful
    if (download_success) {
      tryCatch({
        message("Extracting text...")
        text <- pdf_text(pdf_path)
        
        # Add to results
        results <- rbind(results, tibble::tibble(
          country_name = country,
          date_posted = scraped_website$date_posted[i],
          pdf_text = paste(text, collapse = "\n\n")
        ))
        
        message("Text extraction successful!")
        
        # Delete PDF after extraction if needed
        if (file.remove(pdf_path)) {
          message("PDF deleted successfully")
        } else {
          warning("Could not delete PDF file: ", pdf_path)
        }
      }, error = function(e) {
        errors <- c(errors, paste("Error extracting text from PDF for", country, ":", e$message))
        message("Text extraction error: ", e$message)
      })
    }
  }
  
  # Report errors
  if (length(errors) > 0) {
    message("\n", length(errors), " errors encountered:")
    for (error in errors) {
      message("- ", error)
    }
  }
  
  message("Successfully processed ", nrow(results), " of ", nrow(scraped_website), " NAPs")
  message("Final results saved to ", output_file)
  
  return(results)
}