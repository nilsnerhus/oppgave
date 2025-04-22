# nap_ent.R - NAP Topic Modeling Script
# This script runs the topic modeling pipeline with wide k-value testing

# Create output directories
dir.create("results/data", recursive = TRUE, showWarnings = FALSE)
dir.create("results/plots", recursive = TRUE, showWarnings = FALSE)

# Source function files if not already loaded
if(!exists("preprocess")) source("script/preprocess.R")
if(!exists("optimal_topics")) source("script/optimal_topics.R")
if(!exists("topic_model")) source("script/topic_model.R")

cat("Starting NAP processing pipeline...\n")

# Step 1: Preprocess NAP documents
cat("Preprocessing NAP documents...\n")
processed_data <- preprocess(nap_data)
saveRDS(processed_data, "results/data/processed_nap.rds")
cat("Preprocessing complete. Results saved.\n")

# Step 2: Find optimal topic count across wide range
cat("Finding optimal topic count across wide range...\n")
optimal_result <- optimal_topics(
  processed_data, 
  k_min = 5,        # Start with 5 topics
  k_max = 100,      # Test up to 100 topics
  k_step = 5,       # Test every 5 topics
  final_iterations = 150  # More iterations for final model
)
saveRDS(optimal_result, "results/data/optimal_topics.rds")
cat("Optimal topics analysis complete. Best k:", optimal_result$best_k, "\n")

# Step 3: Extract topic proportions from optimal model
cat("Processing optimal topic model...\n")
topic_result <- topic_model(optimal_result)
saveRDS(topic_result, "results/data/topic_model.rds")

cat("Topic modeling complete. Results saved to results/data/\n")
cat("NAP processing pipeline completed successfully.\n")