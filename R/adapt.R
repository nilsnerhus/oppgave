adapt_to_docs <- function(data) {
  doc_ids <- paste0("doc_", seq_len(nrow(data)))

  tokens <- tibble(
    doc_id = doc_ids,
    text = data$text
  )

  counts <- attr(data, "pipeline_counts")
  n_initial <- counts$n_initial
  n_english <- counts$n_english
  n_final <- counts$n_final

  english_rate <- round(n_english / n_initial * 100, 1)
  extract_rate <- round(n_final / n_english * 100, 1)
  overall_rate <- round(n_final / n_initial * 100, 1)

  create_result(
    data = list(tokens = tokens),
    metadata = list(
      timestamp = Sys.time(),
      processing_time_sec = NA,
      total_documents = n_final,
      successful_downloads = n_english,
      successful_extractions = n_final,
      download_success_rate = english_rate,
      extraction_success_rate = extract_rate,
      overall_success_rate = overall_rate,
      success = n_final > 0
    ),
    diagnostics = list()
  )
}

adapt_to_metadata <- function(data) {
  doc_ids <- paste0("doc_", seq_len(nrow(data)))

  metadata <- tibble(
    doc_id = doc_ids,
    country = data$country,
    country_iso = data$country_iso,
    date = data$date,
    sids = data$sids,
    lldc = data$lldc,
    income_level = data$income_level,
    region = data$region
  )

  create_result(
    data = list(metadata = metadata),
    metadata = list(processing_time_sec = NA),
    diagnostics = list()
  )
}

