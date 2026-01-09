adapt <- function(prep_data_results) {
        # Extract text data
        text_data <- prep_data_result$text
        doc_ids <- paste0("doc_", seq_along(text_data))

        # Create docs structure that process_dfm expects
        docs <- list(
                tokens = tibble(
                        doc_id = doc_ids,
                        text = text_data
                ),
                vocab = NULL, # Add expected fields
                meta = NULL
        )

        # Create metadata structure
        metadata <- list(
                metadata = tibble(
                        doc_id = doc_ids,
                        country = prep_data_result$country,
                        country_iso = prep_data_result$country_iso,
                        date = prep_data_result$date,
                        sids = prep_data_result$sids,
                        lldc = prep_data_result$lldc,
                        income_level = prep_data_result$income_level,
                        region = prep_data_result$region
                )
        )

        # Return in create_result structure
        list(
                data = list(tokens = docs, metadata = metadata),
                metadata = list(success = TRUE)
        )
}
