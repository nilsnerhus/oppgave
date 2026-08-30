# =============================================================================
# Stage 1 — Scrape NAP Central into the NAP table
#
# Run from the repo root:  Rscript R/scrape.R
#
# Writes:
#   csv/naps.csv  one row per English NAP with complete metadata
#                          and full text (sorted by date)
#   csv/metadata.csv      scrape counts (overwritten on every scrape)
#
# PDFs are cached in pdf/{iso}_{yyyymmdd}.pdf and never re-downloaded.
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(tibble)
library(glue)
library(lubridate)
library(polite)
library(rvest)
library(httr2)
library(pdftools)
library(countrycode)
library(wbstats)
library(janitor)
library(fs)

napcentral_url <- "https://napcentral.org/submitted-naps"
sids_url <- "https://www.un.org/ohrlls/content/list-sids"
lldc_url <- "https://www.un.org/ohrlls/content/list-lldcs"
bot_id <- "napr (nnrorstad@gmail.com)"
pdf_dir <- "pdf"
csv_dir <- "csv"
min_doc_words <- 1000

## --- Data pipeline ------------------------------------------------------------

prep_naps <- function(url = napcentral_url) {
  ## Scrape the NAP Central table, fix country names and dates
  message("Scraping NAP Central ...")
  link_tbl_unfiltered <- scrape_napcentral(url) |>
    fix_country_names() |>
    fix_dates()

  ## Keep the rows with an English PDF link
  link_tbl <- link_tbl_unfiltered |>
    select(country, country_iso, date, pdf_english) |>
    filter(!is.na(pdf_english))

  ## Download the PDFs and extract the text
  message("Downloading PDFs (cached files are skipped) ...")
  text_tbl_unfiltered <- link_tbl |>
    download_pdfs() |>
    extract_text()

  ## Keep the rows with text, add the UN and World Bank classifications
  nap_tbl <- text_tbl_unfiltered |>
    assign_un_class() |>
    assign_wb_data()

  ## Clean into the NAP table and write both files
  naps <- nap_tbl |>
    filter(
      str_count(text, "[A-Za-z]+") >= min_doc_words,
      !is.na(date),
      !is.na(income_level),
      !is.na(region)
    ) |>
    arrange(date) |>
    select(country, date, income_level, region, geography, text)

  write_csv(naps, path(csv_dir, "naps.csv"))

  metadata <- tibble(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    n_initial = nrow(link_tbl_unfiltered),
    n_english = nrow(link_tbl),
    n_final = nrow(naps)
  )

  write_csv(metadata, path(csv_dir, "metadata.csv"))
}

## --- Scrape -------------------------------------------------------------------

scrape_napcentral <- function(
  url,
  user_agent = bot_id,
  delay = 2,
  link_col = 5
) {
  page <- bow(url, user_agent = user_agent, delay = delay) |>
    scrape()

  html_tbl <- html_table(page) |>
    pluck(1) |>
    clean_names()

  english_links <- page |>
    html_elements("table tr") |>
    tail(-1) |>
    map_chr(extract_english_link, link_col = link_col)

  html_tbl |> mutate(pdf_english = english_links)
}

## English PDF link from one table row (NA when the row has none)
extract_english_link <- function(row, link_col) {
  cells <- html_elements(row, "td")
  if (length(cells) < link_col) {
    return(NA_character_)
  }

  cells[[link_col]] |>
    html_elements("a") |>
    map_dfr(
      ~ tibble(
        href = html_attr(.x, "href"),
        display = html_text(.x, trim = TRUE)
      )
    ) |>
    mutate(
      display = if_else(display == "", NA_character_, display),
      is_pdf = str_detect(href, regex("\\.pdf$", ignore_case = TRUE)) |
        str_detect(display, regex("pdf|document", ignore_case = TRUE)),
      language = str_to_lower(display) |> str_extract("[a-z]+")
    ) |>
    filter(is_pdf, !is.na(href), href != "", language == "english") |>
    slice(1) |>
    pull(href) |>
    altna()
}

altna <- function(x) if (length(x) == 0) NA_character_ else x

## --- Fix ----------------------------------------------------------------------

fix_country_names <- function(tbl) {
  tbl |>
    mutate(
      country_iso = countrycode(country, "country.name", "iso3c", warn = FALSE),
      country = coalesce(
        countrycode(country_iso, "iso3c", "country.name", warn = FALSE),
        country
      )
    )
}

fix_dates <- function(tbl, date_col = "date_posted") {
  tbl |>
    mutate(
      date = str_extract(.data[[date_col]], "^[^\n]+") |>
        str_trim() |>
        parse_date_time(orders = c("mdy", "dmy", "ymd", "Bdy", "BdY")) |>
        as.Date()
    )
}

## --- Download and extract -----------------------------------------------------

absolute_url <- function(href) {
  if (str_starts(href, "/")) paste0("https://napcentral.org", href) else href
}

download_one_pdf <- function(link, country_iso, date) {
  day <- if_else(is.na(date), "nodate", format(date, "%Y%m%d"))
  path <- path(pdf_dir, glue("{country_iso}_{day}.pdf"))

  if (file.exists(path)) {
    return(path)
  }

  request(absolute_url(link)) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_raw() |>
    writeBin(path)

  path
}

download_pdfs <- function(tbl) {
  tbl |>
    mutate(
      pdf_path = pmap_chr(
        list(pdf_english, country_iso, date),
        safe_download,
        .progress = "Downloading PDFs"
      )
    )
}

safe_download <- possibly(download_one_pdf, otherwise = NA_character_)

extract_one_text <- function(path) {
  suppressMessages(pdftools::pdf_text(path) |> str_flatten(collapse = " "))
}

safe_extract <- possibly(extract_one_text, otherwise = NA_character_)

extract_text <- function(tbl) {
  tbl |>
    mutate(
      text = map_chr(
        pdf_path,
        safe_extract,
        .progress = "Extracting text"
      )
    )
}

## --- Classifications ----------------------------------------------------------

scrape_un_list <- function(url, user_agent = bot_id, delay = 2) {
  html_table(bow(url, user_agent = user_agent, delay = delay) |> scrape()) |>
    pluck(1) |>
    slice(-1) |>
    pivot_longer(everything(), names_to = NULL, values_to = "country") |>
    mutate(
      country = str_extract(country, "[A-Za-z].*[A-Za-z]"),
      country_iso = countrycode(country, "country.name", "iso3c", warn = FALSE)
    ) |>
    filter(!is.na(country_iso)) |>
    pull(country_iso)
}

assign_un_class <- function(tbl) {
  sids <- scrape_un_list(sids_url)
  lldc <- scrape_un_list(lldc_url)

  overlap <- intersect(sids, lldc)
  if (length(overlap) > 0) {
    stop("SIDS and LLDC lists overlap for: ", paste(overlap, collapse = ", "))
  }

  tbl |>
    mutate(
      geography = case_when(
        country_iso %in% sids ~ "SIDS",
        country_iso %in% lldc ~ "LLDC",
        TRUE ~ "Other"
      )
    )
}

assign_wb_data <- function(tbl) {
  wb <- wb_countries() |>
    select(country_iso = iso3c, region, income_level) |>
    mutate(across(c(region, income_level), str_trim))

  left_join(tbl, wb, by = "country_iso")
}

## --- Run ----------------------------------------------------------------------

prep_naps()
