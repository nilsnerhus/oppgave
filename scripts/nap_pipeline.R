# =========================================================================
# NAP TOPIC MODELING PIPELINE
# =========================================================================
# Purpose: Orchestrate the processing of NAP documents through the entire pipeline

# Create output directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Step 1: Corpus collection and preparation

## Load scripts
source("scripts/utils.R")
source("scripts/scrape_web.R")
source("scripts/extract_pdfs.R")
source("scripts/get_un_classifications.R")
source("scripts/add_metadata.R")
source("scripts/process_dfm.R")

## Set parameters
category_map <- list(
  Global = "global_category",    
  Income = "income_level", 
  Region = "region", 
  Geography = "geography",
  Time = "time_period"
)
time_groups <- c("Early" = 2019, "Middle" = 2022, "Late" = Inf)

## Run functions
web <- web_cache(scrape_web)
docs <- auto_cache(extract_pdfs, web)

un_classifications <- auto_cache(get_un_classifications)
metadata <- auto_cache(add_metadata, web, un_classifications, time_groups)

dfm <- auto_cache(process_dfm, docs, metadata)

# Step 2: Structural topic modeling
## Load scripts
source("scripts/fit_model.R")

# Run functions
model <- auto_cache(fit_model, dfm, k = 8, category_map)

# Step 3: Analysis
## Load scripts
source("scripts/name_topics.R")
source("scripts/find_dominance.R")
source("scripts/find_variance.R")
source("scripts/calculate_metrics.R")
source("scripts/load_variables.R")

# Run functions
topic_names <- c("napa", 
                 "cyclone", 
                 "mountain", 
                 "island", 
                 "office", 
                 "transit",
                 "rcp",
                 "mainstream"
                 )
topics <- auto_cache(name_topics, model, topic_names)
metrics <- auto_cache(calculate_metrics, model, topics, dfm)
variables <- auto_cache(load_variables, topics, metrics, web, dfm, model, digits = 0, overwrite = TRUE)