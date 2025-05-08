# Add this right before the prepare_corpus call
test_data <- nap_data$data[1:3, ]

# Instead of this
# stm_data <- auto_cache(prepare_corpus, nap_data$data)

# Use this
stm_data <- prepare_corpus(test_data)  # Skip caching for testing

stm_data <- prepare_corpus(test_data, min_word_count = 1, max_doc_proportion = 1.1)

options(error = recover)

debugonce(stm::prepDocuments)

stm_data <- prepare_corpus(nap_data$data, min_word_count = 0, max_doc_proportion = 2.0)
