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
source("scripts/prepare_corpus.R")

web <- web_cache(scrape_web)
pdfs <- auto_cache(extract_pdfs,web$data)

lldc <- c("AFG", "ARM", "AZE", "BFA", "BDI", "CAF", "TCD", "ETH", "KAZ", 
          "KGZ", "LAO", "LSO", "MWI", "MLI", "MDA", "MNG", "NPL", "NER", 
          "PRY", "RWA", "SSD", "TJK", "MKD", "TKM", "UGA", "UZB", "ZMB", 
          "ZWE", "BOL", "BWA")

sids <- c("ATG", "BHS", "BRB", "BLZ", "CPV", "COM", "CUB", "DMA", "DOM", 
          "FJI", "GRD", "GNB", "GUY", "HTI", "JAM", "KIR", "MDV", "MHL", 
          "MUS", "FSM", "NRU", "PLW", "PNG", "KNA", "LCA", "VCT", "WSM", 
          "STP", "SYC", "SGP", "SLB", "SUR", "TLS", "TON", "TTO", "TUV", 
          "VUT")

nap_data <- auto_cache(add_metadata, pdfs$data, sids_list = sids, lldc_list = lldc)
corpus <- auto_cache(prepare_corpus, nap_data$data)

# Step 2: Structural topic modeling
source("scripts/fit_model.R")

prevalence <- ~ region + wb_income_level + is_sids + is_lldc
model <- auto_cache(fit_model, corpus, k = 20, prevalence = prevalence)

# Step 3: Analysis
source("scripts/name_topics.R")
source("scripts/find_dominance.R")
source("scripts/stm_effects.R")

names <- auto_cache(name_topics, model)
dominance <- auto_cache(find_all_dominance, model, n = 3)
effects <- auto_cache(stm_effects, model)

# Step 4: Findings
source("scripts/create_tables.R")
source("scripts/helper_dominance_table.R")
source("scripts/helper_effects_table.R")

tables <- create_tables(names, dominance, effects)
