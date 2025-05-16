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
source("scripts/add_metadata.R")
source("scripts/validate_tokens.R")

web <- web_cache(scrape_web, overwrite = TRUE)
docs <- auto_cache(extract_pdfs, web$data$tokens, overwrite = TRUE)

lldc <- c("AFG", "ARM", "AZE", "BFA", "BDI", "CAF", "TCD", "ETH", "KAZ", 
          "KGZ", "LAO", "LSO", "MWI", "MLI", "MDA", "MNG", "NPL", "NER", 
          "PRY", "RWA", "SSD", "TJK", "MKD", "TKM", "UGA", "UZB", "ZMB", 
          "ZWE", "BOL", "BWA")

sids <- c("ATG", "BHS", "BRB", "BLZ", "CPV", "COM", "CUB", "DMA", "DOM", 
          "FJI", "GRD", "GNB", "GUY", "HTI", "JAM", "KIR", "MDV", "MHL", 
          "MUS", "FSM", "NRU", "PLW", "PNG", "KNA", "LCA", "VCT", "WSM", 
          "STP", "SYC", "SGP", "SLB", "SUR", "TLS", "TON", "TTO", "TUV", 
          "VUT")

category_map <- list(
  Income = "wb_income_level", 
  Region = "region", 
  Geography = c("is_sids", "is_lldc")
)

metadata <- auto_cache(add_metadata, docs$data, overwrite = TRUE)
tokens <- auto_cache(validate_tokens, nap_data$data$documents, overwrite = TRUE)

# Step 2: Structural topic modeling
source("scripts/prepare_stm_data.R")
source("scripts/search_k")
source("scripts/fit_model.R")

stm_ data <- auto_cache(prepare_stm_data, tokens$data, nap_data$data$config, overwrite = TRUE)
k <- auto_cache(search_k, stm_data, overwrite = TRUE)
model <- auto_cache(fit_model, k = k, overwrite = TRUE)

# Step 3: Analysis
source("scripts/name_topics.R")
source("scripts/find_dominance.R")

names <- auto_cache(name_topics, model)
dominance <- auto_cache(find_dominance, model, n = 3, overwrite = TRUE)
effects <- auto_cache(estimate_effects, model)

# Step 4: Findings
source("scripts/create_tables.R")

tables <- create_tables(names, dominance, effects, overwrite = TRUE)
