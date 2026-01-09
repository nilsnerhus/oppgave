## --- Load packages ---
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(polite)
library(rvest)
library(purrr)
library(janitor)
library(countrycode)
library(httr2)
library(fs)
library(glue)
library(cli)
library(pdftools)
library(wbstats)
library(lubridate)

## --- Data pipeline ---
prep_data <- function(
	url = "https://napcentral.org/submitted-naps",
	freeze_date = "2025-06-24"
) {
	html <- url |>
		bow() |>
		scrape()

	html_tbl_unfiltered <- html |>
		html_table() |>
		pluck(1) |>
		clean_names() |>
		get_pdf_links(html = html) |>
		fix_country_names() |>
		fix_dates()

	html_tbl <- html_tbl_unfiltered |>
		filter(
			date_clean <= freeze_date,
			!is.na(pdf_english)
		) |>
		select(
			country = country_clean,
			country_iso = country_iso,
			date = date_clean,
			pdf_link = pdf_english
		)

	text_tbl_unfiltered <- html_tbl |>
		download_pdfs()
	extract_text()

	text_tbl <- text_tbl_unfiltered |>
		filter(
			!is.na(text)
		) |>
		select(
			country,
			country_iso,
			date,
			text
		)

	nap_tbl <- text_tbl |>
		assign_wb_data() |>
		assign_un_class()

	return(nap_tbl)
}
## --- Assign SIDS/LLDC status from the UN websites ---
assign_un_class <- function(tbl) {
	# Get SIDS list
	sids_url <- "https://www.un.org/ohrlls/content/list-sids"
	sids_list <- sids_url |>
		bow() |>
		scrape() |>
		html_table() |>
		pluck(1) |>
		slice(-1) |>
		pivot_longer(
			cols = everything(),
			names_to = NULL,
			values_to = "country"
		) |>
		mutate(
			sids = TRUE,
			country = str_extract(country, "[A-Za-z].*[A-Za-z]"),
			country_iso = countrycode(
				sourcevar = country,
				origin = "country.name",
				destination = "iso3c",
				warn = FALSE
			)
		) |>
		filter(!is.na(country_iso)) |>
		select(sids, country_iso)

	# Get LLDC list
	lldc_url <- "https://www.un.org/ohrlls/content/list-lldcs"
	lldc_list <- lldc_url |>
		bow() |>
		scrape() |>
		html_table() |>
		pluck(1) |>
		slice(-1) |>
		pivot_longer(
			cols = everything(),
			names_to = NULL,
			values_to = "country"
		) |>
		mutate(
			lldc = TRUE,
			country = str_extract(country, "[A-Za-z].*[A-Za-z]"),
			country_iso = countrycode(
				sourcevar = country,
				origin = "country.name",
				destination = "iso3c",
				warn = FALSE
			)
		) |>
		filter(!is.na(country_iso)) |>
		select(lldc, country_iso)

	# Join both classifications with the input table
	tbl |>
		left_join(sids_list, by = "country_iso") |>
		left_join(lldc_list, by = "country_iso") |>
		mutate(
			sids = if_else(is.na(sids), FALSE, sids),
			lldc = if_else(is.na(lldc), FALSE, lldc)
		)
}


## --- Helper: standardize country names ---
fix_country_names <- function(tbl) {
	tbl |>
		mutate(
			country_iso = countrycode(
				sourcevar = country,
				origin = "country.name",
				destination = "iso3c"
			),
			country_clean = countrycode(
				sourcevar = country_iso,
				origin = "iso3c",
				destination = "country.name"
			)
		)
}

## --- get_pdf_links function ---
get_pdf_links <- function(html_tbl, html, link_col = 5) {
	pdf_data <- html |>
		html_elements("table tr") |>
		tail(-1) |>
		map_dfr(extract_pdfs_from_row, link_col = link_col)
	html_tbl |>
		bind_cols(pdf_data)
}

## --- Helper: standardize the dates ---
fix_dates <- function(tbl, date_col = "date_posted") {
	tbl |>
		mutate(
			date_clean = str_extract(
				.data[[date_col]],
				"^[^\n]+"
			) |>
				str_trim() |>
				parse_date_time(
					orders = c(
						"mdy",
						"dmy",
						"ymd",
						"Bdy",
						"BdY"
					)
				) |>
				as.Date()
		)
}

## --- Helper: download pdfs ---
download_pdfs <- function(tbl) {
	tbl |>
		mutate(
			pdf_path = pmap_chr(
				list(pdf_link, country_iso, date),
				safe_download,
				.progress = "Downloading PDFs",
			)
		)
}


## --- Helper-helper: download just the one pdf ---
download_one_pdf <- function(link, country_iso, date, pdf_dir) {
	pdf_dir <- dir_create("_cache/pdfs")
	filename <- glue("{country_iso}_{format(date, '%Y%m%d')}.pdf")
	path <- path(pdf_dir, filename)

	if (file.exists(path)) {
		return(path)
	}

	request(link) |>
		req_retry(max_tries = 3) |>
		req_perform() |>
		resp_body_raw() |>
		writeBin(path)

	path
}

safe_download <- possibly(download_one_pdf, otherwise = NA_character_)

## --Helper: extract text from pdfs
extract_text <- function(tbl) {
	tbl |>
		mutate(
			text = map_chr(
				pdf_path,
				safe_extract,
				.progress = "Extracting text from the PDFs"
			)
		)
}

## --- Helper-helper: extract just the one ---
extract_one_text <- function(path) {
	suppressMessages(
		pdftools::pdf_text(path) |>
			str_flatten(collapse = " ")
	)
}

safe_extract <- possibly(extract_one_text, otherwise = NA_character_)

## --- Assign the data from the wbstats package ---
assign_wb_data <- function(tbl) {
	wb_stats <- wb_countries() |>
		select(
			country_iso = iso3c,
			region,
			income_level
		) |>
		left_join(text_tbl, wb_stats, by = "country_iso") |>
		filter(
			!is.na(country)
		)
}

## --- Helper-helper: process one row ---
extract_pdfs_from_row <- function(row, link_col) {
	cells <- html_nodes(row, "td")
	links <- cells[[link_col]] |>
		extract_all_pdfs_by_language() |>
		pivot_wider(
			names_from = language,
			values_from = href,
			names_prefix = "pdf_"
		)
}

## --- Helper-helper: extract all link data from a cell ---
extract_link_data <- function(cell) {
	cell |>
		html_elements("a") |>
		map_dfr(
			~ {
				tibble(
					href = html_attr(.x, "href"),
					text = html_text(.x, trim = TRUE),
					span_text = html_element(.x, "span") |>
						html_text(trim = TRUE) %||%
						""
				)
			}
		)
}

## --- Helper-helper: extract PDFs and identify language ---
extract_all_pdfs_by_language <- function(cell) {
	cell |>
		extract_link_data() |>
		mutate(
			display_text = if_else(text != "", text, span_text),
			is_pdf = str_detect(
				href,
				regex("\\.pdf$", ignore_case = TRUE)
			) |
				str_detect(
					display_text,
					regex(
						"pdf|document",
						ignore_case = TRUE
					)
				),
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
