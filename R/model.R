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
library(purrr)
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

  ## Segment the raw text before cleaning, as in the handed-in pipeline
  n_tokens_raw <- sum(map_int(
    naps$text,
    ~ length(str_split(.x, "\\s+")[[1]])
  ))
  segment_length <- make_segment_length(n_tokens_raw)
  message(
    "Segmenting ", n_documents, " NAPs into raw-text windows of about ",
    segment_length, " words ..."
  )
  segment_tbl <- make_segments(naps, segment_length)

  ## Align segment metadata with the segment text
  meta_segment <- segment_tbl |>
    select(seg_key, country) |>
    left_join(naps |> select(-text), by = "country") |>
    arrange(match(seg_key, segment_tbl$seg_key))

  ## Clean and stem the segments with STM's text processor
  message("Cleaning ", nrow(segment_tbl), " segments ...")
  processed <- stm::textProcessor(
    documents = segment_tbl$text,
    metadata = meta_segment,
    lowercase = TRUE,
    removestopwords = TRUE,
    removenumbers = TRUE,
    removepunctuation = TRUE,
    stem = TRUE,
    wordLengths = c(3, Inf),
    customstopwords = geo_stopwords() |> pull(word),
    verbose = FALSE
  )

  ## Prune terms by the share of segments in which they appear
  min_docs <- max(1, ceiling(nrow(segment_tbl) * min_word_freq))
  max_docs <- max(min_docs + 1, floor(nrow(segment_tbl) * max_word_freq))
  prepped <- stm::prepDocuments(
    documents = processed$documents,
    vocab = processed$vocab,
    meta = processed$meta,
    lower.thresh = min_docs,
    upper.thresh = max_docs,
    verbose = FALSE
  )

  documents <- prepped$documents
  vocab <- prepped$vocab
  ## Drop factor levels that lost their documents along the way
  meta_segment <- prepped$meta |>
    mutate(across(where(is.factor), fct_drop))

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
    n_tokens = sum(vapply(documents, function(doc) sum(doc[2, ]), numeric(1))),
    avg_segment_length = segment_length,
    min_docs = min_docs,
    max_docs = max_docs
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
    str_split("\\s+") |>
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

## Short documents stay whole; long ones are cut into raw-word windows
segment_document <- function(country, text, doc_order, segment_length) {
  words <- str_split(text, "\\s+")[[1]]
  n_words <- length(words)
  seg_id <- if (n_words < whole_doc_min) {
    rep(0L, n_words)
  } else {
    as.integer((seq_len(n_words) - 1) %/% segment_length)
  }

  tibble(word = words, seg_id = seg_id) |>
    group_by(seg_id) |>
    summarise(text = str_c(word, collapse = " "), .groups = "drop") |>
    mutate(
      country = country,
      doc_order = doc_order,
      seg_key = paste0(country, "_", seg_id)
    )
}

make_segments <- function(tbl, segment_length) {
  pmap(
    list(
      country = tbl$country,
      text = tbl$text,
      doc_order = seq_len(nrow(tbl))
    ),
    function(country, text, doc_order) {
      segment_document(country, text, doc_order, segment_length)
    }
  ) |>
    bind_rows() |>
    arrange(doc_order, seg_id) |>
    select(-doc_order)
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
