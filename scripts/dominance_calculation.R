# =============================================================================
# Streamlined Dominance Analysis Script
# =============================================================================
# This script analyzes discourse dominance patterns across different dimensions in NAP data
# and saves all results to a single RDS file

# Load required libraries
library(dplyr)
library(ggplot2)

# Source required functions
source("scripts/utils.R")
source("scripts/find_dominance.R")

# -----------------------------------------------------------------------------
# Step 1: Load data
# -----------------------------------------------------------------------------
log_message("Loading topic proportions data", "dominance_script")

# Load the topic proportions data
topic_props <- readRDS("data/extract_topic_props.rds")

# Extract the data frame (handling different possible structures)
if ("data" %in% names(topic_props) && "data" %in% names(topic_props$data)) {
  topic_data <- topic_props$data$data
  topic_labels <- topic_props$data$topic_labels
} else if ("data" %in% names(topic_props)) {
  topic_data <- topic_props$data
  topic_labels <- NULL
} else {
  topic_data <- topic_props
  topic_labels <- NULL
}

# -----------------------------------------------------------------------------
# Step 2: Set parameters
# -----------------------------------------------------------------------------

# Dimensions to analyze
dimensions <- c("region", "wb_income_level", "is_sids", "is_lldc", "is_ldc")

# Different values of n to test
n_values <- c(3, 5, 8)

# Create output directory if needed
output_dir <- "data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Step 3: Create results tables for each value of n
# -----------------------------------------------------------------------------
# Initialize a list to store results for each n value
all_results <- list()
all_plots <- list()

for (n_topics in n_values) {
  log_message(paste("Analyzing with n =", n_topics), "dominance_script")
  
  # Initialize results table for this n value
  results <- data.frame(
    dimension = character(),
    category = character(),
    docs = integer(),
    raw_dominance = numeric(),
    norm_dominance = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate overall dominance
  log_message("Calculating overall dominance", "dominance_script")
  overall_result <- find_dominance(
    topic_data, 
    value_col = "Proportion", 
    n = n_topics
  )
  
  # Handle return value appropriately (check if it's a list or atomic)
  if (is.list(overall_result)) {
    doc_count <- overall_result$doc_count
    if (is.null(doc_count)) doc_count <- nrow(topic_data)
    
    # Add overall result to table
    results <- rbind(results, data.frame(
      dimension = "Overall",
      category = "All documents",
      docs = doc_count,
      raw_dominance = overall_result$raw_dominance,
      norm_dominance = overall_result$norm_dominance,
      ci_lower = overall_result$ci_lower,
      ci_upper = overall_result$ci_upper,
      stringsAsFactors = FALSE
    ))
  } else {
    # If overall_result is atomic (just a single value)
    results <- rbind(results, data.frame(
      dimension = "Overall",
      category = "All documents",
      docs = length(unique(topic_data$doc_id)),
      raw_dominance = overall_result,
      norm_dominance = NA,
      ci_lower = NA,
      ci_upper = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Calculate dominance for each dimension
  for (dim in dimensions) {
    if (dim %in% names(topic_data)) {
      log_message(paste("Analyzing dimension:", dim), "dominance_script")
      
      # Get unique values in this dimension
      unique_vals <- unique(topic_data[[dim]])
      unique_vals <- unique_vals[!is.na(unique_vals)]
      
      # Process each category
      for (val in unique_vals) {
        # Format category label
        if (is.logical(val)) {
          category_label <- if (val) paste("Yes", dim) else paste("No", dim)
        } else {
          category_label <- as.character(val)
        }
        
        # Count documents for this category
        doc_count <- length(unique(topic_data$doc_id[topic_data[[dim]] == val]))
        
        # Skip categories with too few documents
        if (doc_count < min_doc_count) {
          log_message(paste("Skipping", category_label, "- only", doc_count, "documents"), 
                      "dominance_script")
          
          # Add to results with NA values
          results <- rbind(results, data.frame(
            dimension = dim,
            category = category_label,
            docs = doc_count,
            raw_dominance = NA,
            norm_dominance = NA,
            ci_lower = NA,
            ci_upper = NA,
            stringsAsFactors = FALSE
          ))
          
          next
        }
        
        # Calculate dominance
        log_message(paste("Calculating dominance for", category_label), "dominance_script")
        dominance_result <- find_dominance(
          topic_data,
          value_col = "Proportion",
          n = n_topics,
          filter_col = dim,
          filter_value = val
        )
        
        # Handle return value appropriately (check if it's a list or atomic)
        if (is.list(dominance_result)) {
          # Add to results table
          results <- rbind(results, data.frame(
            dimension = dim,
            category = category_label,
            docs = dominance_result$doc_count,
            raw_dominance = dominance_result$raw_dominance,
            norm_dominance = dominance_result$norm_dominance,
            ci_lower = dominance_result$ci_lower,
            ci_upper = dominance_result$ci_upper,
            stringsAsFactors = FALSE
          ))
        } else {
          # If dominance_result is atomic (just a single value)
          results <- rbind(results, data.frame(
            dimension = dim,
            category = category_label,
            docs = doc_count,
            raw_dominance = dominance_result,
            norm_dominance = NA,
            ci_lower = NA,
            ci_upper = NA,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Round numeric columns for display
  results$raw_dominance <- round(results$raw_dominance, 3)
  results$norm_dominance <- round(results$norm_dominance, 3)
  results$ci_lower <- round(results$ci_lower, 3)
  results$ci_upper <- round(results$ci_upper, 3)
  
  # Add n value column
  results$n_topics <- n_topics
  
  # Store in results list
  all_results[[as.character(n_topics)]] <- results
}

# -----------------------------------------------------------------------------
# Step 4: Create a combined summary table across all n values
# -----------------------------------------------------------------------------
log_message("Creating combined summary table", "dominance_script")

# Initialize combined table
combined_table <- data.frame(
  dimension = character(),
  category = character(),
  docs = integer(),
  stringsAsFactors = FALSE
)

# Add columns for each n value
for (n_topics in n_values) {
  combined_table[[paste0("n", n_topics)]] <- numeric()
}

# Fill in combined table
for (i in 1:length(all_results)) {
  n_topics <- n_values[i]
  results <- all_results[[as.character(n_topics)]]
  
  if (i == 1) {
    # First n value - set up base table
    combined_table <- data.frame(
      dimension = results$dimension,
      category = results$category,
      docs = results$docs,
      stringsAsFactors = FALSE
    )
    
    # Add first n value (prefer normalized, but use raw if normalized not available)
    combined_table[[paste0("n", n_topics)]] <- ifelse(
      is.na(results$norm_dominance),
      results$raw_dominance,
      results$norm_dominance
    )
  } else {
    # Add this n value (prefer normalized, but use raw if normalized not available)
    combined_table[[paste0("n", n_topics)]] <- ifelse(
      is.na(results$norm_dominance),
      results$raw_dominance,
      results$norm_dominance
    )
  }
}

# -----------------------------------------------------------------------------
# Step 5: Create a change analysis table
# -----------------------------------------------------------------------------
log_message("Creating change analysis", "dominance_script")

# Calculate differences between consecutive n values
change_table <- combined_table

for (i in 2:length(n_values)) {
  prev_n <- n_values[i-1]
  curr_n <- n_values[i]
  
  # Column names
  prev_col <- paste0("n", prev_n)
  curr_col <- paste0("n", curr_n)
  change_col <- paste0("change_", prev_n, "_to_", curr_n)
  
  # Calculate change
  change_table[[change_col]] <- change_table[[curr_col]] - change_table[[prev_col]]
  
  # Round for display
  change_table[[change_col]] <- round(change_table[[change_col]], 3)
}

# -----------------------------------------------------------------------------
# Step 6: Create bar plots for each n value
# -----------------------------------------------------------------------------
log_message("Creating bar plots", "dominance_script")

# Function to create dominance bar plot
create_dominance_barplot <- function(
    dominance_data,    # Data frame with dominance results
    n_value = 8,       # Which n value to plot
    exclude_no = TRUE, # Whether to exclude "No" categories
    sort_by = "value", # How to sort bars: "value" or "category"
    color_by = "dimension", # What to color by: "dimension" or "docs"
    title = NULL,      # Custom title
    use_norm = TRUE    # Use normalized values if available
) {
  # Clone the data to avoid modifying the original
  plot_data <- dominance_data
  
  # Add column for the value to plot (normalized if available, otherwise raw)
  if (use_norm) {
    value_col <- ifelse(is.na(plot_data$norm_dominance), 
                        plot_data$raw_dominance, 
                        plot_data$norm_dominance)
  } else {
    value_col <- plot_data$raw_dominance
  }
  
  plot_data$value <- value_col
  
  # Filter out "No" categories if requested
  if (exclude_no) {
    plot_data <- plot_data %>%
      filter(!grepl("^No ", category))
  }
  
  # Filter out NA values
  plot_data <- plot_data %>%
    filter(!is.na(value))
  
  # Sort the data
  if (sort_by == "value") {
    plot_data <- plot_data %>%
      arrange(desc(value))
  } else {
    plot_data <- plot_data %>%
      arrange(dimension, category)
  }
  
  # Force category to be a factor with levels in the desired order
  plot_data$category <- factor(plot_data$category, levels = plot_data$category)
  
  # Set up colors
  if (color_by == "dimension") {
    # Color by dimension
    color_mapping <- aes(fill = dimension)
    legend_title <- "Dimension"
  } else {
    # Color by document count
    color_mapping <- aes(fill = docs)
    legend_title <- "Documents"
  }
  
  # Set up default title if none provided
  if (is.null(title)) {
    if (use_norm) {
      title_text <- paste("Normalized Dominance Index (n =", n_value, ")")
    } else {
      title_text <- paste("Raw Dominance Index (n =", n_value, ")")
    }
  } else {
    title_text <- title
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = category, y = value)) +
    geom_col(color_mapping) +
    geom_text(aes(label = round(value, 2)), hjust = -0.2, size = 3.5) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(plot_data$value) * 1.15)) +
    labs(
      title = title_text,
      subtitle = "Higher values indicate more centralized/concentrated discourse",
      x = NULL,
      y = "Dominance Index (0-1)",
      caption = paste("Analysis based on top", n_value, "topics")
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Return the plot
  return(p)
}

# Create plots for each n value and store them
for (n_topics in n_values) {
  log_message(paste("Creating bar plot for n =", n_topics), "dominance_script")
  
  # Get results for this n value
  results <- all_results[[as.character(n_topics)]]
  
  # Create regular bar plot excluding "No" categories
  plot <- create_dominance_barplot(
    results, 
    n_value = n_topics,
    exclude_no = TRUE,
    sort_by = "value",
    color_by = "dimension"
  )
  
  # Create a plot colored by document count
  doc_plot <- create_dominance_barplot(
    results, 
    n_value = n_topics,
    exclude_no = TRUE,
    sort_by = "value",
    color_by = "docs",
    title = paste("Dominance by Document Count (n =", n_topics, ")")
  )
  
  # Store plots in list
  all_plots[[paste0("barplot_n", n_topics)]] <- plot
  all_plots[[paste0("docplot_n", n_topics)]] <- doc_plot
}

# Create and store a plot of the "best" n value (n = 8)
if ("8" %in% names(all_results)) {
  best_results <- all_results[["8"]]
  best_plot <- create_dominance_barplot(
    best_results,
    n_value = 8,
    exclude_no = TRUE,
    sort_by = "value",
    title = "Discourse Dominance Index"
  )
  
  all_plots[["best_plot"]] <- best_plot
}

# -----------------------------------------------------------------------------
# Step 7: Save everything to a single RDS file
# -----------------------------------------------------------------------------
log_message("Saving all results to RDS file", "dominance_script")

# Create a comprehensive results object
dominance_analysis <- list(
  # Raw results
  results_by_n = all_results,
  
  # Processed tables
  combined_table = combined_table,
  change_table = change_table,
  
  # Plots
  plots = all_plots,
  
  # Metadata
  metadata = list(
    timestamp = Sys.time(),
    n_values = n_values,
    dimensions = dimensions,
    min_doc_count = min_doc_count
  )
)

# Save to RDS
saveRDS(dominance_analysis, file.path(output_dir, "dominance_analysis.rds"))

log_message("Dominance analysis complete and saved", "dominance_script")

# Return the analysis invisibly, but don't print anything
invisible(dominance_analysis)
