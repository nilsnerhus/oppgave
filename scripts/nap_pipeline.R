# =========================================================================
# NAP TOPIC MODELING PIPELINE
# =========================================================================
# Purpose: Orchestrate the processing of NAP documents through the entire pipeline

# Create output directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Step 1: Corpus collection and preparation
source("scripts/utils.R")
source("scripts/scrape_web.R")
source("scripts/extract_pdfs.R")
source("scripts/get_un_classifications.R")
source("scripts/add_metadata.R")
source("scripts/process_dfm.R")

web <- web_cache(scrape_web)
docs <- auto_cache(extract_pdfs, web)

category_map <- list(
  Global = "global_category",    
  Income = "wb_income_level", 
  Region = "region", 
  Geography = c("is_sids", "is_lldc"),
  Time = "time_period"
)

un_classifications <- auto_cache(get_un_classifications)
time_groups <- c("Early" = 2018, "Middle" = 2021, "Late" = Inf)
metadata <- auto_cache(add_metadata, web, un_classifications, time = time_groups)
dfm <- auto_cache(process_dfm, docs, metadata, min_docs = 0.1, max_docs = 0.8)

# Step 2: Structural topic modeling
source("scripts/find_k.R")
source("scripts/fit_model.R")

k <- auto_cache(find_k, dfm, range = c(5, 20, 5))
model <- auto_cache(fit_model, dfm, k, category_map = category_map)

# Step 3: Analysis
source("scripts/name_topics.R")
source("scripts/find_dominance.R")
source("scripts/find_variance.R")
source("scripts/calculate_metrics.R")

context <- " Follow these rules: Each label must be completely unique and do not 
name a topic after a country or city. These are National Adaptation Plan documents 
from the UNFCCC covering climate adaptation strategies. 

Earlier analysis has found the themes to be either security related (disasters or risks etc.), 
geographical (rangeland or coastal etc.) or sectoral (agriculture, fisheries, tourism)."

topics <- auto_cache(name_topics, model, context = context)
metrics <- auto_cache(calculate_metrics, model, topics, dfm)