# =========================================================================
# NAP TOPIC MODELING PIPELINE
# =========================================================================
# Purpose: Orchestrate the processing of NAP documents through
# the entire pipeline

library(memoise)

# Create output directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Set up caching on disk
memoise_dir <- dir_create("_cache/memoise")
cache <- cachem::cache_disk(
        dir = memoise_dir,
        max_size = 1024^3,
        max_age = Inf
)

## Step 1: Assemble corpus (would be better to just source the whole R/-directory)
source("R/scrape_web.R")

mem_prep_data <- memoise(prep_data, cache = cache)
data <- mem_prep_data()

## Use the adapter to make the result fit

docs <- adapt_to_docs(data)

metadata <- adapt_to_metadata(data)

## Step 2: Prepare and tune model
dfm <- auto_cache(process_dfm, docs, metadata)


# For the next step maybe? I'll have to make an adapter to the rest of the pipeline.
category_map <- list(
        Global = "global_category",
        Income = "income_level",
        Region = "region",
        Geography = "geography",
        Time = "time_period"
)

time_groups <- c("Early" = 2019, "Middle" = 2022, "Late" = Inf)
# Step 2: Structural topic modeling
## Load scripts
source("R/fit_model.R")

# Run functions
model <- auto_cache(fit_model, dfm, k = 8, category_map)

# Step 3: Analysis
## Load scripts
source("R/name_topics.R")
source("R/find_dominance.R")
source("R/find_variance.R")
source("R/calculate_metrics.R")
source("R/load_variables.R")

# Run functions
topic_names <- c(
        "napa",
        "cyclone",
        "mountain",
        "hurricane",
        "office",
        "transit",
        "rcp",
        "mainstream"
)
topics <- auto_cache(name_topics, model, topic_names)
metrics <- auto_cache(calculate_metrics, model, topics, dfm)
variables <- auto_cache(
        load_variables,
        topics,
        metrics,
        web,
        dfm,
        model,
        digits = 0,
)
