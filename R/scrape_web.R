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

url <- "https://napcentral.org/submitted-naps"
name_col <- 2
date_col <- 6
link_col <- 5
table_index <- 1
has_header <- TRUE

scrape_web <- function(
	url = url
) {

	# Start timing
	start_time <- Sys.time()

	library(dplyr)
	library(tidyr)
	library(stringr)
	library(tibble)
	library(polite)
	library(rvest)
	library(purrr)
	library(janitor)

	## --- Main pipeline -----------------------------------------------------------
	log_message(paste("Getting the table from", url), "scrape_web")
	
	html <- url |>
	  bow() |>
	  scrape()
	
	html_tbl <- html |>
	  html_table() |>
	  pluck(1) |>
	  clean_names() |>
	  get_pdf_links(html) |>
	  fix_dates()

	## --- get_pdf_links function --------------------------------------------------
	get_pdf_links <- function(html_tbl, html, link_col = 5) {
	  pdf_data <- html |>
	    html_elements("table tr") |>
	    tail(-1) |>
	    map_dfr(extract_pdfs_from_row, link_col = link_col)
	  
	  html_tbl |>
	    bind_cols(pdf_data)
	}
	
	## --- Helper: process one row ------------------------------------------------
	extract_pdfs_from_row <- function(row, link_col) {
	  cells <- html_nodes(row, "td")
	  
	  if (length(cells) < link_col) {
	    return(tibble())
	  }
	  
	  links <- cells[[link_col]] |>
	    extract_all_pdfs_by_language()
	  
	  if (nrow(links) == 0) {
	    return(tibble())
	  }
	  
	  links |>
	    pivot_wider(
	      names_from = language,
	      values_from = href,
	      names_prefix = "pdf_"
	    )
	}
	
	## --- Helper: extract all link data from a cell ------------------------------
	extract_link_data <- function(cell) {
	  cell |>
	    html_elements("a") |>
	    map_dfr(~{
	      tibble(
		href = html_attr(.x, "href"),
		text = html_text(.x, trim = TRUE),
		span_text = html_element(.x, "span") |> html_text(trim = TRUE) %||% ""
	      )
	    })
	}

	## --- Helper: extract PDFs and identify language -----------------------------
	extract_all_pdfs_by_language <- function(cell) {
	  cell |>
	    extract_link_data() |>
	    mutate(
	      display_text = if_else(text != "", text, span_text),
	      is_pdf = str_detect(href, regex("\\.pdf$", ignore_case = TRUE)) |
		       str_detect(display_text, regex("pdf|document", ignore_case = TRUE)),
	      language = display_text |>
		str_to_lower() |>
		str_trim() |>
		str_replace_all("[^a-z]", "_")
	    ) |>
	    filter(is_pdf, !is.na(href), href != "") |>
	    group_by(language) |>
	    slice(1) |>
	    ungroup() |>
	    select(language, href)
	}

	## --- Helper: standardize the dates ----------------------------
	library(lubridate)
	
	fix_dates <- function(tbl, date_col = "date_posted") {
	tbl |>
	    mutate(
	      date_clean = str_extract(.data[[date_col]], "^[^\n]+") |>
		str_trim() |>
		parse_date_time(orders = c("mdy", "dmy", "ymd", "Bdy", "BdY")) |>
		as.Date()
	    )
	}
	## --- Calculate processing time ----------------------------------------------
	end_time <- Sys.time()
	processing_time <- as.numeric(difftime(
		end_time,
		start_time,
		units = "secs"
	))

	## --- Prepare and return final result ----------------------------------------
	metadata <- list(
		url = url,
		timestamp = start_time,
		processing_time_sec = processing_time,
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
