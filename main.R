# =========================================================================
# NAP TOPIC MODELING PIPELINE
# =========================================================================

# Step 0: Load everything for the pipeline
## Load packages

library(memoise)
library(fs)

## Create output directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)

## Set up caching on disk
memoise_dir <- dir_create("_cache/memoise")
cache <- cachem::cache_disk(
  dir = memoise_dir,
  max_size = 1024^3,
  max_age = Inf
)

## Load scripts in the R/ directory
dir_ls("R", glob = "*.R") |> walk(source)

# Step 1: Prep the data corpus
mem_prep_data <- memoise(prep_data, cache = cache)
data <- mem_prep_data()

# Step 1.1: Use the adapter to make the result fit the old pipeline

docs <- adapt_to_docs(data)

metadata <- adapt_to_metadata(data)

# Step 2: Prepare and tune model
dfm <- auto_cache(process_dfm, docs, metadata)

category_map <- list(
  Global = "global_category",
  Income = "income_level",
  Region = "region",
  Geography = "geography",
  Time = "time_period"
)

time_groups <- c("Early" = 2019, "Middle" = 2022, "Late" = Inf)

# Step 2: Structural topic modeling

model <- auto_cache(fit_model, dfm, k = 8, category_map)

# Step 3: Analysis

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
