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
time_groups <- c("Early" = 2016, "Middle" = 2021, "Late" = Inf)

## Run functions
web <- web_cache(scrape_web)
docs <- auto_cache(extract_pdfs, web)

un_classifications <- auto_cache(get_un_classifications)
metadata <- auto_cache(add_metadata, web, un_classifications, time_groups, overwrite = TRUE)

dfm <- auto_cache(process_dfm, docs, metadata)

# Step 2: Structural topic modeling
## Load scripts
source("scripts/find_k.R")
source("scripts/fit_model.R")

# Run functions
k <- auto_cache(find_k, dfm, overwrite = TRUE)
model <- auto_cache(fit_model, dfm, k, category_map)

# Step 3: Analysis
## Load scripts
source("scripts/name_topics.R")
source("scripts/find_dominance.R")
source("scripts/find_variance.R")
source("scripts/calculate_metrics.R")

# Run functions
topic_names <- c("poverty", 
                 "sea_rise", 
                 "mountain", 
                 "costal", 
                 "office", 
                 "transit", 
                 "rainfall", 
                 "mainstream"
                 )
metrics <- auto_cache(calculate_metrics, model, topics, dfm, overwrite = TRUE)
topics <- auto_cache(name_topics, model, topic_names, overwrite = TRUE)

knitr::kable(topics$data)
knitr::kable(metrics$data)

