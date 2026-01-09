## This might be better of (or only possible) as three seperate functions, as I have to create three different objects to pass along.

## And, it might also be better off if I manage to rewrite it by actually defining the inputs and outputs, and understand how to make those differences meet up. And write a test of the function at the end, such as the scrape_web function. I am still not sure if that is the best approach though. But testing, and TDD, is something I want to get good at.

## This is perhaps the most challenging part of the whole approach.

adapt_to_docs <- function(data) {
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

        create_result()
}

adapt_to_metadata <- function(data) {
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
        create_result()
}
