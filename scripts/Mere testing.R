if(!exists("utils")) source("scripts/utils.R")
if(!exists("find_dominance")) source("scripts/find_dominance.R")
if(!exists("bullseye")) source("scripts/bullseye.R")


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

