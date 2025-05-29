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

geo_config <- list(
  sids_list = sids,
  lldc_list = lldc
)

category_map <- list(
  Income = "wb_income_level", 
  Region = "region", 
  Geography = c("is_sids", "is_lldc")
)

metadata <- auto_cache(add_metadata, web$data$metadata, geo_config = geo_config, category_map = category_map)
tokens <- auto_cache(validate_tokens, docs$data$tokens)

# Step 2: Structural topic modeling
source("scripts/process_dfm.R")
source("scripts/find_k.R")
source("scripts/fit_model.R")

dfm <- auto_cache(process_dfm, tokens, metadata, min_docs = 2, exclusivity_docs = 3)
k_result <- auto_cache(find_k, 
                       dfm, 
                       range = c(5, 8, 10, 12, 15, 18, 20),
                       iterations = 100,
                       coherence = 0.40,    # Interpretability
                       exclusivity = 0.35,  # Distinctiveness  
                       residual = 0.25,     # Model fit
                       penalty = 0          # Penalty for complexity
                       )      
k_value <- k_result$data$best_k
model <- auto_cache(fit_model, 
                    dfm, 
                    k = k_value, 
                    category_map = category_map, 
                    original_docs = tokens$data$documents
                    )

# Step 3: Analysis
source("scripts/name_topics.R")
source("scripts/calculate_dominance.R")
source("scripts/find_dominance.R")
source("scripts/find_variance.R")
source("scripts/calculate_variance.R")

topics <- auto_cache(name_topics, model, mode = "auto")
dominance <- auto_cache(calculate_dominance, model, topics, overwrite = TRUE)
variance <- auto_cache(calculate_variance, model, dominance, overwrite = TRUE)
