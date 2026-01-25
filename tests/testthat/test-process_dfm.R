test_that("process_dfm produces consistent results", {
  web_data <- readRDS("data/extract_pdfs.rds")
  metadata <- readRDS("data/add_metadata.rds")

  result <- process_dfm(
    docs = web_data,
    metadata = metadata,
    remove_stopwords = TRUE,
    stem = TRUE,
    segment = TRUE,
    target_segments = 200
  )

  expect_snapshot(result)
})