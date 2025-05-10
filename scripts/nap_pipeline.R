# =========================================================================
# NAP TOPIC MODELING PIPELINE
# =========================================================================
# Purpose: Orchestrate the processing of NAP documents through the entire pipeline

# Create output directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Source function files if not already loaded¨
source("scripts/utils.R")
source("scripts/scrape_web.R")
source("scripts/extract_pdfs.R")
source("scripts/add_metadata.R")
source("scripts/prepare_corpus.R")
source("scripts/fit_model.R")

# Step 1: Scrape the UNFCCC website
exclude_countries <- c("Uruguay", "Israel", "Kuwait", "Trinidad and Tobago") # Uruguay has no national plan, just sectoral, and the others are high income countries
web <- web_cache(scrape_web, exclude_countries = exclude_countries)

# Step 2: Extract the pdfs from the web
pdfs <- auto_cache(extract_pdfs,web$data)

# Step 3: Add metadata
## Define lists of SIDS and LLDC countries, since no public repositories keep the information
lldc <- c("AFG", "ARM", "AZE", "BFA", "BDI", "CAF", "TCD", "ETH", "KAZ", 
          "KGZ", "LAO", "LSO", "MWI", "MLI", "MDA", "MNG", "NPL", "NER", 
          "PRY", "RWA", "SSD", "TJK", "MKD", "TKM", "UGA", "UZB", "ZMB", 
          "ZWE", "BOL", "BWA")

sids <- c("ATG", "BHS", "BRB", "BLZ", "CPV", "COM", "CUB", "DMA", "DOM", 
          "FJI", "GRD", "GNB", "GUY", "HTI", "JAM", "KIR", "MDV", "MHL", 
          "MUS", "FSM", "NRU", "PLW", "PNG", "KNA", "LCA", "VCT", "WSM", 
          "STP", "SYC", "SGP", "SLB", "SUR", "TLS", "TON", "TTO", "TUV", 
          "VUT")

ldc <- c("AFG", "AGO", "BGD", "BEN", "BFA", "BDP", "BOL", "BIH", "BWA", "CAF", 
         "CMR", "CHD", "COM", "COG", "COD", "DJI", "DMA", "EGY", "GNQ", "ERI", 
         "ETH", "GMB", "GHA", "GIN", "GMB", "GNB", "GUM", "HND", "IND", "KIR", 
         "KWT", "LKA", "LES", "LAO", "LBR", "LUX", "MAD", "MLI", "MNG", "MWI", 
         "MYS", "MOZ", "NPL", "NER", "NGA", "PRY", "PNG", "SGP", "SOM", "SDN", 
         "TGO", "TON", "TJK", "UGA", "UZB", "VUT", "YEM", "ZMB", "ZWE")

nap_data <- auto_cache(add_metadata, pdfs$data, sids_list = sids, lldc_list = lldc, ldc_list = ldc)

# Step 4: Prepare corpus
corpus <- auto_cache(prepare_corpus, nap_data$data)

# Step 5: Running the model
prevalence <- ~ region + wb_income_level + is_sids + is_ldc + is_lldc
model <- auto_cache(fit_model, corpus)