# =========================================================================
# NAP TOPIC MODELING PIPELINE
# =========================================================================
# Purpose: Orchestrate the processing of NAP documents through the entire pipeline

# Create output directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Source function files if not already loaded
if(!exists("prepare_corpus")) source("script/prepare_corpus.R")
if(!exists("find_best_k")) source("script/find_best_k.R")
if(!exists("topic_model")) source("script/extract_topic_props.R")

cat("Starting NAP processing pipeline...\n")

# Step 1: Load NAP data
library(napr)
data(nap_data)
cat("Loaded NAP data with", nrow(nap_data), "documents\n")

# Step 1.1: Load stopwords for nap_data
nap_stops <- c("mr", "https", "la", "yet", "de", "i.e", "yr", "tion", "des", "8.5", "svg")

# Step 2: Preprocess NAP documents
cat("\n=== PREPROCESSING ===\n")
corpus <- prepare_corpus(nap_data, custom_stopwords = nap_stops)
cat("Preprocessing complete\n")

# Step 3: Find optimal topic count
cat("\n=== FINDING OPTIMAL TOPIC COUNT ===\n")
best_k <- find_best_k(corpus)
cat("Optimal topic analysis complete. Best k:", best_k$best_k, "\n")

# Step 4: Generate final topic model
cat("\n=== GENERATING FINAL TOPIC MODEL ===\n")
topic_props <- extract_topic_props(corpus, k = 15)

cat("\nNAP processing pipeline completed successfully!\n")
cat("Results saved to the 'data' directory\n")
cat("Use readRDS() to load any of the saved files for further analysis\n")