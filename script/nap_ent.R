# nap_ent.R - NAP Entropy Analysis Script
# This script runs the complete NAP analysis pipeline

# Create output directories
dir.create("results/data", recursive = TRUE, showWarnings = FALSE)
dir.create("results/plots", recursive = TRUE, showWarnings = FALSE)

# Run preprocessing
cat("Preprocessing NAP documents...\n")

processed_text <- preprocess(nap_data)

saveRDS(processed_text, "results/data/processed_nap.rds")

# Find optimal topic count
cat("Finding optimal topic count...\n")

optimal_result <- optimal_topics(processed_text)

saveRDS(optimal_result, "results/data/optimal_topics.rds")

# Extract topic proportions
cat("Extracting topic proportions...\n")

nap_ent <- topic_model(optimal_result)

saveRDS(topic_props, "results/data/topic_proportions.rds")


cat("Analysis complete.\n")

