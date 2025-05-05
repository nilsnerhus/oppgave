# =========================================================================
# NAP TOPIC MODELING PIPELINE
# =========================================================================
# Purpose: Orchestrate the processing of NAP documents through the entire pipeline

# Create output directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Source function files if not already loaded¨
if(!exists("utils")) source("scripts/utils.R")
if(!exists("scrape_web")) source("scripts/scrape_web.R")
if(!exists("extract_pdfs")) source("scripts/extract_pdfs.R")
if(!exists("add_metadata")) source("scripts/add_metadata.R")
if(!exists("prepare_corpus")) source("scripts/prepare_corpus.R")
if(!exists("find_best_k")) source("scripts/find_best_k.R")
if(!exists("topic_model")) source("scripts/extract_topic_props.R")

# Step 1: Scrape the UNFCCC website
exclude_countries <- c("Uruguay") # Does not have a national plan, just many sectoral
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
nap_stops <- c("mr", "https", "http", "la", "yet", "de", "i.e", "yr", "tion", "des", "svg")
corpus <- auto_cache(prepare_corpus, nap_data$data, custom_stopwords = nap_stops)

# Step 5: Find optimal topic count
best_k <- auto_cache(find_best_k, corpus)

# Step 6: Extract topic proportions
topic_props <- auto_cache(extract_topic_props, best_k$data)
