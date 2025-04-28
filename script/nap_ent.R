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

message("Starting pipeline...")

# Step 1: Load NAP data
library(napr)
data(nap_data)
message("Loaded NAP data with", nrow(nap_data), "documents")

# Step 1.1: Load stopwords for nap_data
nap_stops <- c("mr", "https", "la", "yet", "de", "i.e", "yr", "tion", "des", "8.5", "svg")

# Step 2: Prepare corpus
message("Preparing corpus...")
corpus <- prepare_corpus(nap_data, custom_stopwords = nap_stops)
message("Corpus prepared!")

# Step 3: Find optimal topic count
message("Finding the best k...")
best_k <- find_best_k(corpus)
message("Found best k! It is", best_k$best_k, " ")

# Step 4: Extract topic proportions
message("Extracting topic proportions...")
topic_props <- extract_topic_props(best_k)
message("Topic proportions extracted!")

message(" NAP processing pipeline completed successfully and results saved to the 'data' directory ")