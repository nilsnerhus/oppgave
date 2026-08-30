# =============================================================================
# Stage 2a — Fit the topic model and save it
#
# Run from the repo root:  Rscript R/model.R
#
# Reads:
#   csv/naps.csv
# Writes:
#   rds/model.rds   the fitted STM model with the metadata and topic
#                   proportions the results stage needs
#
# The NAP table is cut by freeze_date before modeling, so bumping the
# cutoff and re-running gives comparable results over time.
# =============================================================================

library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(forcats)
library(tidytext)
library(lubridate)
library(stm)
library(SnowballC)
library(fs)

freeze_date <- "2025-06-24"
k <- 8
target_segments <- 200
whole_doc_min <- 50
min_seg_len <- 100
min_word_freq <- 0.1
max_word_freq <- 0.8
iterations <- 200
seed <- 12345
furniture_words <- c("pdf", "http", "https", "php", "www", "com", "gov", "html")
csv_dir <- "csv"
rds_dir <- "rds"

## --- Data pipeline ------------------------------------------------------------

analyze_naps <- function(cutoff = freeze_date) {
  ## Read the NAP table; one document per country
  naps <- read_csv(path(csv_dir, "naps.csv"), show_col_types = FALSE)

  if (anyDuplicated(naps$country) > 0) {
    stop(
      "The NAP table has more than one document for: ",
      paste(unique(naps$country[duplicated(naps$country)]), collapse = ", ")
    )
  }

  no_text <- naps$country[is.na(naps$text) | str_length(str_squish(naps$text)) == 0]
  if (length(no_text) > 0) {
    stop("naps.csv contains documents without text: ", paste(no_text, collapse = ", "))
  }

  ## Add the model covariates and cut by freeze_date
  naps <- naps |>
    prep_covariates() |>
    filter(date <= cutoff)
  n_documents <- nrow(naps)

  ## Tokenize everything into one long table
  message("Tokenizing ", n_documents, " NAPs ...")
  token_tbl_unfiltered <- naps |>
    unnest_tokens(word, text)
  n_tokens_raw <- nrow(token_tbl_unfiltered)

  ## Clean the tokens: stopwords, short words, numbers, then stemming
  message("Cleaning ", n_tokens_raw, " tokens ...")
  token_tbl <- token_tbl_unfiltered |>
    clean_tokens()

  ## Group the cleaned tokens back into segments
  message("Grouping into segments ...")
  segment_tbl <- make_segments(token_tbl)

  empty <- setdiff(naps$country, unique(segment_tbl$country))
  if (length(empty) > 0) {
    message("No surviving tokens for: ", paste(empty, collapse = ", "))
  }

  ## Keep the words that are neither too rare nor too common across segments
  word_tbl <- prune_words(segment_tbl, min_word_freq, max_word_freq)

  ## Cast to a sparse matrix and align the segment metadata with its rows
  meta_segment <- word_tbl |>
    distinct(seg_key, country) |>
    left_join(naps, by = "country")
  meta_segment <- meta_segment[order(meta_segment$seg_key), ]

  segment_matrix <- cast_sparse(
    word_tbl |> mutate(seg_key = factor(seg_key, levels = meta_segment$seg_key)),
    seg_key,
    word
  )
  stopifnot(identical(rownames(segment_matrix), meta_segment$seg_key))

  documents <- as_stm_documents(segment_matrix)
  vocab <- colnames(segment_matrix)

  ## Drop factor levels that lost their documents along the way
  meta_segment <- meta_segment |>
    mutate(across(where(is.factor), fct_drop))

  ## Fit the model
  check_prevalence(meta_segment)
  message("Fitting STM with k = ", k, " on ", length(documents), " segments ...")
  model <- stm(
    documents = documents,
    vocab = vocab,
    K = k,
    data = meta_segment,
    prevalence = ~ geography + income_level + region + time_period,
    max.em.its = iterations,
    seed = seed,
    verbose = FALSE
  )

  ## Average the topic proportions across each country's segments
  theta_seg <- model$theta |>
    as_tibble(.name_repair = ~ paste0("topic_", seq_len(k)))

  theta_doc <- meta_segment |>
    select(country) |>
    bind_cols(theta_seg) |>
    group_by(country) |>
    summarise(across(starts_with("topic_"), mean), .groups = "drop")

  meta_doc <- meta_segment |>
    select(-seg_key) |>
    group_by(country) |>
    slice(1) |>
    ungroup()

  ## Save everything the results stage needs
  fit_info <- list(
    freeze_date = cutoff,
    k = k,
    iterations = model$convergence$its,
    converged = model$convergence$converged,
    n_documents = n_documents,
    n_segments = length(documents),
    n_tokens_raw = n_tokens_raw,
    n_tokens = nrow(token_tbl),
    avg_segment_length = make_segment_length(nrow(token_tbl))
  )

  dir_create(rds_dir)
  saveRDS(
    list(
      model = model,
      meta = meta_segment,
      theta_doc = theta_doc,
      meta_doc = meta_doc,
      fit_info = fit_info
    ),
    path(rds_dir, "model.rds")
  )

  message(
    "Model saved to rds/model.rds: k = ", k, ", ",
    length(documents), " segments from ", n_documents, " NAPs, ",
    model$convergence$its, " iterations, ",
    ifelse(model$convergence$converged, "converged", "NOT converged")
  )
}

## --- Prep ---------------------------------------------------------------------

## Model covariates: factors are made here with forcats, never in the CSV
prep_covariates <- function(tbl) {
  tbl |>
    mutate(
      geography = factor(geography, levels = c("Other", "LLDC", "SIDS")),
      income_level = factor(
        income_level,
        levels = c(
          "Low income",
          "Lower middle income",
          "Upper middle income",
          "High income"
        )
      ),
      region = factor(region),
      time_period = case_when(
        year(date) <= 2019 ~ "Early",
        year(date) <= 2022 ~ "Middle",
        TRUE ~ "Late"
      ) |>
        factor(levels = c("Early", "Middle", "Late"))
    )
}

## --- Clean --------------------------------------------------------------------

clean_tokens <- function(tbl) {
  tbl |>
    anti_join(stop_words, by = "word") |>
    anti_join(geo_stopwords(), by = "word") |>
    mutate(word = str_remove_all(word, "\\d")) |>
    filter(str_length(word) >= 3, !word %in% furniture_words) |>
    mutate(word = wordStem(word))
}

## Words from country names, so country mentions don't become topics
geo_stopwords <- function(min_length = 4) {
  common_terms <- c(
    "united",
    "republic",
    "democratic",
    "kingdom",
    "island",
    "islands",
    "states"
  )

  countrycode::codelist$country.name.en |>
    na.omit() |>
    str_split(" ") |>
    unlist() |>
    str_to_lower() |>
    str_subset(str_c("^[a-z]{", min_length, ",}$")) |>
    setdiff(common_terms) |>
    unique() |>
    as_tibble() |>
    rename(word = value)
}

## --- Segments -----------------------------------------------------------------

## Segment length that lands near the target number of segments
make_segment_length <- function(n_tokens) {
  max(min_seg_len, ceiling(n_tokens / target_segments))
}

## Short documents stay whole; long ones are cut into word-count windows
make_segments <- function(tbl) {
  segment_length <- make_segment_length(nrow(tbl))

  tbl |>
    group_by(country) |>
    mutate(
      seg_id = if (n() < whole_doc_min) {
        0L
      } else {
        as.integer((row_number() - 1) %/% segment_length)
      }
    ) |>
    ungroup() |>
    mutate(seg_key = paste0(country, "_", seg_id))
}

## Drop words that appear in too few or too many segments
prune_words <- function(tbl, min_freq, max_freq) {
  n_segments <- n_distinct(tbl$seg_key)
  min_docs <- max(1, ceiling(n_segments * min_freq))
  max_docs <- max(min_docs + 1, floor(n_segments * max_freq))

  word_docs <- tbl |>
    group_by(word) |>
    summarise(n_seg = n_distinct(seg_key), .groups = "drop")

  tbl |>
    semi_join(
      word_docs |> filter(n_seg >= min_docs, n_seg <= max_docs),
      by = "word"
    )
}

## --- Model --------------------------------------------------------------------

## Turn a sparse document matrix into the list of 2-row matrices stm expects:
## the first row holds vocabulary indices, the second the counts
as_stm_documents <- function(m) {
  triplets <- as.data.frame(Matrix::summary(m))
  triplets <- triplets[order(triplets$i, triplets$j), ]

  lapply(seq_len(nrow(m)), function(idx) {
    rows <- which(triplets$i == idx)
    rbind(as.integer(triplets$j[rows]), as.integer(triplets$x[rows]))
  })
}

## A missing covariate must stop the run, not silently drop out of the model
check_prevalence <- function(meta) {
  covariates <- c("geography", "income_level", "region", "time_period")
  missing <- setdiff(covariates, names(meta))

  if (length(missing) > 0) {
    stop(
      "Covariates missing from the segment metadata: ",
      paste(missing, collapse = ", ")
    )
  }
}

## --- Run ----------------------------------------------------------------------

analyze_naps()
