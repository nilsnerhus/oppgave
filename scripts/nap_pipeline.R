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
source("scripts/validate_tokens.R")

web <- web_cache(scrape_web)
docs <- auto_cache(extract_pdfs, web$data$tokens)

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
                       coherence = 0.60,    # Interpretability
                       exclusivity = 0.40,  # Distinctiveness  
                       residual = 0.20,     # Model fit
                       penalty = 0          # Penalty for complexity
                       )      
k_value <- k_result$data$best_k
model <- auto_cache(fit_model, 
                    dfm, 
                    k = k_value, 
                    category_map = category_map, 
                    original_docs = tokens$data$documents)

# Step 3: Analysis
source("scripts/name_topics.R")
source("scripts/find_dominance.R")
source("scripts/find_variance.R")
source("scripts/calculate_metrics.R")

context <- "These are National Adaptation Plan documents from the UNFCCC covering 
climate adaptation strategies. Generate exactly ONE WORD topic labels with 
capitalized first letter that capture the core theme. CRITICAL: Each label must 
be completely unique - no two topics can have the same name. Earlier analysis has
found the themes to be either security related (disasters or risks etc.), geographical
(rangeland or coastal etc.) or sectoral (agriculture, fisheries, tourism)."

topics <- auto_cache(name_topics, model, context = context, overwrite = TRUE)
metrics <- auto_cache(calculate_metrics, model, topics)