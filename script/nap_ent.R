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

# Step 2: Preprocess NAP documents
cat("\n=== PREPROCESSING ===\n")
corpus_dfm <- prepare_corpus(
  nap_data, 
  text_column = "pdf_text",
)
cat("Preprocessing complete\n")

# Step 3: Find optimal topic count
cat("\n=== FINDING OPTIMAL TOPIC COUNT ===\n")
optimal_model <- find_best_k(corpus_dfm)
cat("Optimal topic analysis complete. Best k:", optimal_model$best_k, "\n")

# Step 4: Generate final topic model
cat("\n=== GENERATING FINAL TOPIC MODEL ===\n")
topic_results <- extract_topic_props(optimal_model)

cat("\nNAP processing pipeline completed successfully!\n")
cat("Results saved to the 'data' directory\n")
cat("Use readRDS() to load any of the saved files for further analysis\n")