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

dfm <- auto_cache(process_dfm, tokens, metadata, min_docs = 2, exclusivity_docs = 4)
k_result <- auto_cache(find_k, 
                       dfm, 
                       range = c(5, 7, 10, 12, 15, 17, 20),
                       iterations = 100,
                       coherence = 0.30,    # Interpretability
                       exclusivity = 0.50,  # Distinctiveness  
                       residual = 0.25,     # Model fit
                       penalty = 0          # Penalty for complexity
                       )      
k_value <- k_result$data$best_k
model <- auto_cache(fit_model, dfm, k = k_value, category_map = category_map)

# Step 3: Analysis
source("scripts/name_topics.R")
source("scripts/calculate_dominance.R")
source("scripts/find_dominance.R")
source("scripts/estimate_variance.R")

topics <- auto_cache(name_topics, model, mode = "auto")
dominance <- auto_cache(calculate_dominance, model, topics, min_group_size = 8)
variance <- auto_cache(estimate_variance, model)

corpus_level <- dominance$data[dominance$data$level_type == "corpus", ]

# Create the main plot
p1 <- ggplot(corpus_level, aes(x = reorder(subcategory, dominance), y = dominance)) +
  # Add shaded confidence interval first
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = category), 
              alpha = 0.2) +
  # Add the main line
  geom_point(aes(color = category), size = 3) +
  # Add value labels
  geom_text(aes(label = sprintf("%.3f", dominance)), 
            vjust = -1, size = 3, color = "#495057") +
  # Facet by category
  facet_wrap(~ category, scales = "free_x", ncol = 1) +
  # Color scheme matching thesis
  scale_fill_manual(values = c("Income" = "#0d6efd", 
                               "Region" = "#6f42c1", 
                               "Geography" = "#198754")) +
  scale_color_manual(values = c("Income" = "#0d6efd", 
                                "Region" = "#6f42c1", 
                                "Geography" = "#198754")) +
  # Styling
  scale_y_continuous(limits = c(0.92, 1.0), 
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Discourse Centralization by Country Groupings",
    subtitle = "With 95% Confidence Intervals",
    x = NULL,
    y = "Dominance Index",
    caption = "Note: Shaded areas represent 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "#212529"),
    plot.subtitle = element_text(size = 11, color = "#6c757d"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, color = "#495057"),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.caption = element_text(size = 9, color = "#6c757d", hjust = 0)
  )

# Alternative: Single panel with all groups
p2 <- ggplot(corpus_level, aes(x = reorder(subcategory, dominance), y = dominance)) +
  # Shaded confidence intervals
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = category, group = category), 
              alpha = 0.25) +
  # Error bars for clearer visualization
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = category), 
                width = 0.2, alpha = 0.7) +
  # Points for actual values
  geom_point(aes(color = category), size = 4) +
  # Value labels
  geom_text(aes(label = sprintf("%.3f", dominance)), 
            vjust = -1.5, size = 3, color = "#212529") +
  # Colors
  scale_fill_manual(values = c("Income" = "#0d6efd", 
                               "Region" = "#6f42c1", 
                               "Geography" = "#198754")) +
  scale_color_manual(values = c("Income" = "#0d6efd", 
                                "Region" = "#6f42c1", 
                                "Geography" = "#198754")) +
  # Styling
  scale_y_continuous(limits = c(0.90, 1.0), 
                     labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0.90, 1.0, 0.02)) +
  labs(
    title = "Dominance Index Across All Groupings",
    subtitle = "Shaded areas show 95% confidence intervals",
    x = NULL,
    y = "Dominance Index",
    fill = "Category",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "#212529"),
    plot.subtitle = element_text(size = 11, color = "#6c757d"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, color = "#495057"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )