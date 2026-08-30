# =============================================================================
# Stage 2b — Write the results tables from the fitted model
#
# Run from the repo root:  Rscript R/results.R
#
# Reads:
#   rds/model.rds
#   csv/metadata.csv
# Writes:
#   csv/topics.csv    one row per topic: terms, share, and its countries
#   csv/metrics.csv   dominance and estimated effects per country group
#   csv/metadata.csv  the run facts appended to the scrape metadata
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(tibble)
library(stm)
library(fs)

n_frex <- 5
n_top_countries <- 3
top_n_topics <- 3
meaningful_threshold <- 0.07
min_group_size <- 2
csv_dir <- "csv"
rds_dir <- "rds"

## --- Data pipeline ------------------------------------------------------------

write_results <- function() {
  ## Load the fitted model, its topic proportions, and their metadata
  results <- readRDS(path(rds_dir, "model.rds"))
  fit <- results$fit_info
  topic_cols <- paste0("topic_", seq_len(fit$k))
  theta <- as.matrix(results$theta_doc[topic_cols])

  ## Topics: strongest terms, corpus share, and the countries that use them
  frex <- stm::labelTopics(results$model, n = n_frex)$frex

  topic_tbl <- tibble(
    topic_id = seq_len(fit$k),
    frex_terms = apply(frex, 1, paste, collapse = ", "),
    topic_proportion = colMeans(theta),
    effective_documents = unlist(lapply(topic_cols, function(col) {
      sum(results$theta_doc[[col]] > meaningful_threshold)
    }))
  ) |>
    left_join(
      rank_countries(results$theta_doc, n_top_countries),
      by = "topic_id"
    ) |>
    select(
      topic_id,
      frex_terms,
      topic_proportion,
      effective_documents,
      c1_country,
      c1_score,
      c2_country,
      c2_score,
      c3_country,
      c3_score
    )

  ## Dominance and estimated effects for every country group
  metrics_tbl <- calculate_metrics(
    theta,
    results$meta_doc,
    results$model,
    results$meta
  )

  ## Append the run facts to the scrape metadata
  metadata <- read_csv(path(csv_dir, "metadata.csv"), show_col_types = FALSE)
  run_facts <- build_run_facts(fit)

  write_csv(topic_tbl, path(csv_dir, "topics.csv"))
  write_csv(metrics_tbl, path(csv_dir, "metrics.csv"))
  write_csv(bind_rows(metadata, run_facts), path(csv_dir, "metadata.csv"))

  message(
    "Wrote ",
    nrow(topic_tbl),
    " topics to csv/topics.csv, ",
    nrow(metrics_tbl),
    " group metrics to csv/metrics.csv, and appended ",
    nrow(run_facts),
    " run facts to csv/metadata.csv"
  )
}

## --- Topics -------------------------------------------------------------------

## Top countries per topic by mean NAP-level topic proportion
rank_countries <- function(theta_doc, n_top) {
  theta_doc |>
    pivot_longer(-country, names_to = "topic", values_to = "score") |>
    group_by(topic) |>
    slice_max(score, n = n_top, with_ties = FALSE) |>
    mutate(
      rank = row_number(),
      topic = as.integer(str_remove(topic, "topic_"))
    ) |>
    ungroup() |>
    complete(
      topic = seq_len(max(topic)),
      rank = seq_len(n_top),
      fill = list(country = NA_character_, score = NA_real_)
    ) |>
    pivot_wider(
      names_from = rank,
      values_from = c(country, score),
      names_glue = "c{rank}_{.value}"
    ) |>
    rename(topic_id = topic)
}

## --- Dominance and effects ----------------------------------------------------

## How concentrated a group's mean topic proportions are in its top topics,
## normalized so 0 = uniform spread and 1 = fully concentrated
find_dominance <- function(theta, doc_indices) {
  props <- colMeans(theta[doc_indices, , drop = FALSE])
  top <- order(props, decreasing = TRUE)[seq_len(min(
    top_n_topics,
    ncol(theta)
  ))]

  raw <- sum(props[top])
  baseline <- length(top) / ncol(theta)

  list(
    dominance = pmin(1, pmax(0, (raw - baseline) / (1 - baseline))),
    top_topics = top
  )
}

## Effect of belonging to a group on its top topics, via estimateEffect on
## the segment-level model; the strongest topic's estimate is the group's
find_variance <- function(
  stm_model,
  stm_meta,
  col_name,
  col_value,
  top_topics
) {
  empty <- list(
    effect_size = NA_real_,
    std_error = NA_real_,
    significant = FALSE
  )

  test_group <- as.factor(stm_meta[[col_name]] == col_value)
  if (length(unique(test_group)) <= 1) {
    return(empty)
  }

  temp_meta <- stm_meta |> mutate(test_group = test_group)
  formula <- as.formula(paste0(
    "c(",
    paste(top_topics, collapse = ","),
    ") ~ test_group"
  ))

  effects <- tryCatch(
    stm::estimateEffect(
      formula,
      stmobj = stm_model,
      metadata = temp_meta,
      uncertainty = "None"
    ),
    error = function(e) NULL
  )
  if (is.null(effects)) {
    return(empty)
  }

  estimates <- lapply(seq_along(top_topics), function(i) {
    if (length(effects$parameters) < i) {
      return(NULL)
    }

    coefs <- effects$parameters[[i]][[1]]$est
    vcov <- effects$parameters[[i]][[1]]$vcov
    if (length(coefs) < 2 || nrow(vcov) < 2) {
      return(NULL)
    }

    effect_size <- unname(coefs[2])
    std_error <- unname(sqrt(vcov[2, 2]))
    t_stat <- effect_size / std_error
    p_value <- 2 * pt(abs(t_stat), nrow(temp_meta) - 2, lower.tail = FALSE)

    list(effect_size = effect_size, std_error = std_error, p_value = p_value)
  })
  estimates <- Filter(Negate(is.null), estimates)

  if (length(estimates) == 0) {
    return(empty)
  }

  best <- estimates[[which.min(sapply(estimates, function(x) x$p_value))]]
  list(
    effect_size = best$effect_size,
    std_error = best$std_error,
    significant = best$p_value < 0.05
  )
}

## One row per category x subcategory, plus an Overall row per category
calculate_metrics <- function(theta, meta_doc, stm_model, stm_meta) {
  categories <- c(
    Geography = "geography",
    Income = "income_level",
    Region = "region",
    Time = "time_period"
  )

  metrics <- list()

  for (category in names(categories)) {
    column <- categories[[category]]
    values <- unique(as.character(meta_doc[[column]]))
    values <- values[!is.na(values)]

    dominance <- c()
    effect_sizes <- c()
    documents <- c()

    for (value in values) {
      doc_indices <- which(as.character(meta_doc[[column]]) == value)
      if (length(doc_indices) < min_group_size) {
        next
      }

      fit <- find_dominance(theta, doc_indices)
      variance <- find_variance(
        stm_model,
        stm_meta,
        column,
        value,
        fit$top_topics
      )

      metrics[[length(metrics) + 1]] <- tibble(
        category = category,
        subcategory = value,
        documents = length(doc_indices),
        dominance = fit$dominance,
        effect_size = variance$effect_size,
        std_error = variance$std_error,
        significant = variance$significant
      )

      dominance <- c(dominance, fit$dominance)
      effect_sizes <- c(effect_sizes, variance$effect_size)
      documents <- c(documents, length(doc_indices))
    }

    if (length(dominance) > 0) {
      metrics[[length(metrics) + 1]] <- tibble(
        category = category,
        subcategory = "Average",
        documents = sum(documents),
        dominance = mean(dominance),
        effect_size = mean(effect_sizes, na.rm = TRUE),
        std_error = NA_real_,
        significant = NA
      )
    }
  }

  bind_rows(metrics)
}

## --- Metadata -----------------------------------------------------------------

build_run_facts <- function(fit_info) {
  tibble(
    variable = c(
      "freeze_date",
      "k",
      "n_documents",
      "n_tokens_raw",
      "n_final_documents",
      "avg_segment_length",
      "iterations",
      "converged"
    ),
    value = c(
      fit_info$freeze_date,
      as.character(fit_info$k),
      as.character(fit_info$n_documents),
      as.character(fit_info$n_tokens_raw),
      as.character(fit_info$n_segments),
      as.character(fit_info$avg_segment_length),
      as.character(fit_info$iterations),
      as.character(fit_info$converged)
    )
  )
}

## --- Run ----------------------------------------------------------------------

write_results()
