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
dominance <- auto_cache(calculate_dominance, model, topics, min_group_size = 8, overwrite = TRUE)
variance <- auto_cache(estimate_variance, model, overwrite = TRUE)

# Filter and select specific columns
corpus_only <- dominance$data[dominance$data$level_type == "corpus", 
                              c("category", "subcategory", "documents", 
                                "normalized_dominance", "ci_lower", "ci_upper")]

# Print the table
knitr::kable(corpus_only, 
             caption = "Corpus-level dominance values",
             col.names = c("Category", "Subcategory", "N", 
                           "Dominance", "CI Lower", "CI Upper"),
             digits = 3)

# Prepare data
plot_data <- dominance$data[dominance$data$level_type == "corpus" & 
                              dominance$data$category != "Overall", ]

# Order by dominance within categories
plot_data <- plot_data %>%
  dplyr::group_by(category) %>%
  dplyr::mutate(subcategory = forcats::fct_reorder(subcategory, normalized_dominance)) %>%
  dplyr::ungroup()

# Create plot with bars and shaded uncertainty
dominance_bar_plot <- ggplot2::ggplot(plot_data, 
                                      ggplot2::aes(x = normalized_dominance, 
                                                   y = subcategory,
                                                   fill = category)) +
  # Shaded uncertainty range (behind bars)
  ggplot2::geom_rect(
    ggplot2::aes(xmin = ci_lower, xmax = ci_upper, 
                 ymin = as.numeric(subcategory) - 0.35, 
                 ymax = as.numeric(subcategory) + 0.35),
    alpha = 0.3
  ) +
  
  # Main bars
  ggplot2::geom_col(width = 0.7, alpha = 0.8) +
  
  # Value labels at the end of bars
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", normalized_dominance)),
    hjust = -0.1,
    size = 3.5,
    color = "#212529"
  ) +
  
  # Facet without strip headers
  ggplot2::facet_grid(category ~ ., 
                      scales = "free_y", 
                      space = "free_y") +
  
  # Color scheme
  ggplot2::scale_fill_manual(values = c("Income" = "#0d6efd", 
                                        "Region" = "#6f42c1", 
                                        "Geography" = "#198754"),
                             guide = "none") +
  
  # X-axis with extra space for labels
  ggplot2::scale_x_continuous(
    limits = c(0, 0.75),
    expand = c(0, 0)
  ) +
  
  # Labels
  ggplot2::labs(
    title = "Discourse Centralization by Country Groupings",
    subtitle = "Bars show dominance index with shaded 95% confidence intervals",
    x = "Dominance Index",
    y = NULL
  ) +
  
  # Theme with no strip text
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "bold", color = "#212529"),
    plot.subtitle = ggplot2::element_text(size = 11, color = "#6c757d"),
    axis.text = ggplot2::element_text(size = 10, color = "#495057"),
    axis.title = ggplot2::element_text(size = 11, color = "#495057"),
    strip.text = ggplot2::element_blank(),  # Remove facet labels
    strip.background = ggplot2::element_blank(),  # Remove facet background
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = "#dee2e6", size = 0.5),
    panel.spacing = ggplot2::unit(0.5, "lines"),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  )

print(dominance_bar_plot)
