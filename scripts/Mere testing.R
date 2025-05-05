# Load the saved results
find_best_k <- readRDS("data/find_best_k.rds")

# View the plot
find_best_k$data$plot

# Check convergence info
model$convergence

# Load the topic proportions data
topic_props <- readRDS("data/extract_topic_props.rds")

# Extract just the data component if using your standardized result structure
topic_data <- topic_props$data$data

# Calculate dominance
dominance <- find_dominance(
  data = topic_data,
  value_col = "Proportion"
)
dominance

dom_africa <- find_dominance(topic_data, filter_col = "region")