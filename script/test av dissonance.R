
# Source the function
if(!exists("find_entropy")) source("script/find_entropy.R")

topic_props <- readRDS("data/topic_props.rds")

topic_data <- topic_results$data

topic_data

# For calculating entropy by country
country_dissonance <- find_entropy(topic_data, 
                                 by = "country_name")

country

# For calculating entropy by region
region_dissonance <- find_entropy(topic_data, 
                                value_col = "Proportion", 
                                by = "region")

region_dissonance

# For calculating entropy by income level (if available)
income_dissonance <- find_entropy(topic_data, 
                                value_col = "Proportion", 
                                by = "Income")
find_entropy()

# Create a formatted table for display
formatted_table <- topic_data %>%
  select(country_name, Topic, Proportion) %>%
  mutate(Proportion = sprintf("%.4f", Proportion))

formatted_table

# Overall topic entropy
topic_entropy <- find_entropy(topic_data, value_col = "Proportion")
topic_entropy

# Regional entropy (how diverse are topic distributions within regions?)
regional_entropy <- find_entropy(topic_data, value_col = "Proportion", group_by_col = "region")
regional_entropy

# Country-level entropy
country_entropy <- find_entropy(topic_data, value_col = "Proportion", group_by_col = "country_name")
country_entropy

load(topic_props.rds)


country_entropy <- find_entropy(topic_data, value_col = "Proportion", group_by_col = "country_name")

country_entropy

