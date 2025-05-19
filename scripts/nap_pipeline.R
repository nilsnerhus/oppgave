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

web <- web_cache(scrape_web)
docs <- auto_cache(extract_pdfs, web$data$tokens)

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

metadata <- auto_cache(add_metadata, web$data$metadata)
tokens <- auto_cache(validate_tokens, docs$data$tokens, overwrite = TRUE)

# Step 2: Structural topic modeling
source("scripts/process_text.R")
source("scripts/prep_documents.R")
source("scripts/search_k.R")
source("scripts/find_k.R")
source("scripts/fit_model.R")

text <- auto_cache(process_text, tokens, metadata, overwrite = TRUE)
documents <- auto_cache(prep_documents, text, overwrite = TRUE)
metrics <- auto_cache(search_k, documents, overwrite = TRUE)
k <- auto_cache(find_k, metrics, overwrite = TRUE)
model <- auto_cache(fit_model, documents, k = k$data$best_k, overwrite = TRUE)

# Step 3: Analysis
source("scripts/name_topics.R")
source("scripts/find_dominance.R")
source("scripts/estimate_effect-R")

names <- auto_cache(name_topics, model)
dominance <- auto_cache(find_dominance, model, n = 3, overwrite = TRUE)
effects <- auto_cache(estimate_effect, model)