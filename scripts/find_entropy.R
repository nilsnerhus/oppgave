# Purpose: Create an entropy value for any given distribution

# Load required packages
library(dplyr)

find_entropy <- function(data, value_col = "Proportion", group_by_col = NULL) {
  
  # If no grouping, calculate overall entropy
  if (is.null(group_by_col)) {
    # Normalize values to ensure they sum to 1
    total <- sum(data[[value_col]])
    p <- data[[value_col]] / total
    
    # Remove zeros
    p <- p[p > 0]
    
    # If only one value, entropy is 0
    if (length(p) <= 1) return(0)
    
    # Calculate Shannon entropy
    raw_entropy <- -sum(p * log(p))
    
    # Normalize by maximum possible entropy
    max_entropy <- log(length(p))
    normalized_entropy <- raw_entropy / max_entropy
    
    return(normalized_entropy)
  }
  
  # With grouping, average the topic distributions first
  # Then calculate entropy across these aggregated distributions
  result <- data %>%
    # Group by the requested metadata column
    group_by(!!sym(group_by_col)) %>%
    # Calculate mean proportions across groups (e.g., mean topic prevalence per country)
    summarize(entropy = {
      # Normalize values within group
      p <- .data[[value_col]] / sum(.data[[value_col]])
      p <- p[p > 0]
      if (length(p) <= 1) return(0)
      # Calculate entropy
      raw <- -sum(p * log(p))
      raw / log(length(p))
    }, .groups = "drop")
  
  # Return mean entropy across all groups
  return(mean(result$entropy))
}