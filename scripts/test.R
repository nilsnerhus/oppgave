# Load required package
library(stm)

# Get sample document
sample_doc <- tokens$data$documents$text[1]
cat("Original document length:", length(unlist(strsplit(sample_doc, " "))), "words\n")

# Process the document with textProcessor
processed <- stm::textProcessor(
  documents = sample_doc,
  metadata = data.frame(id = 1),
  lowercase = FALSE,          # Already done
  removestopwords = TRUE,
  removenumbers = FALSE,      # Already done
  removepunctuation = FALSE,  # Already done
  stem = TRUE,
  wordLengths = c(3, Inf),
  verbose = TRUE
)

# Examine result
cat("\nProcessed document has", 
    if(is.null(processed$documents[[1]])) 0 else nrow(processed$documents[[1]]), 
    "tokens\n")

# Check vocabulary
cat("Vocabulary size:", length(processed$vocab), "\n")

# Look at the first few tokens
if(!is.null(processed$documents[[1]]) && nrow(processed$documents[[1]]) > 0) {
  cat("\nFirst few tokens in processed document:\n")
  print(head(processed$documents[[1]], 10))
} else {
  cat("\nNo tokens in processed document!\n")
}

# If the document is very small, inspect stopwords being removed
if(!is.null(processed$documents[[1]]) && nrow(processed$documents[[1]]) < 5) {
  cat("\nChecking stopwords...\n")
  stopwords <- tm::stopwords("en")
  words <- unlist(strsplit(sample_doc, " "))
  stopword_count <- sum(words %in% stopwords)
  cat("Document contains", stopword_count, "stopwords out of", length(words), "total words\n")
  cat("Stopword percentage:", round(stopword_count/length(words)*100, 1), "%\n")
  
  # Print the first few stopwords found in the document
  stopwords_in_doc <- words[words %in% stopwords]
  if(length(stopwords_in_doc) > 0) {
    cat("Sample stopwords found:", paste(head(stopwords_in_doc, 10), collapse=", "), "\n")
  }
}