# =========================================================================
# NAP Topic Modeling Function
# =========================================================================
# Purpose: Create an entropy value for any distribution given

# Load required packages
library(dplyr)
library(rlang)

dissonance <- function(data, value_col = "Proportion", 
                       filter = NULL, 
                       by = NULL) {
  value_sym <- sym(value_col)
  
  if (!is.null(filter)) {
    for (f in names(filter)) {
      data <- data %>% filter(.data[[f]] %in% filter[[f]])
    }
  }
  
  if (!is.null(by)) {
    by_sym <- sym(by)
    
    data %>%
      group_by(!!by_sym) %>%
      summarise(
        entropy = {
          p <- .data[[value_col]]
          p <- p[p > 0]
          if (length(p) <= 1) 0 else {
            raw <- -sum(p * log(p))
            raw / log(length(p))
          }
        },
        .groups = "drop"
      ) %>%
      rename(Group = !!by_sym)
    
  } else {
    p <- data %>% pull(!!value_sym)
    p <- p[p > 0]
    if (length(p) <= 1) return(0)
    raw <- -sum(p * log(p))
    raw / log(length(p))
  }
}
