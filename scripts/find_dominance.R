#' @title Calculate discourse dominance index
#' @description Measures discourse centralization by calculating the proportion of
#'   representation captured by the most dominant topics/categories.
#'
#' @param data Data frame containing proportions or values to analyze
#' @param value_col Name of column containing the proportion values (default: "Proportion")
#' @param n Number of top categories to include when calculating dominance (default: 8)
#' @param filter_col Optional column name to filter by
#' @param filter_value Optional value to filter for in filter_col
#'
#' @return Numeric value between 0 and 1, where higher values indicate greater
#'   discourse centralization (more concentration among top n categories)
#'
#' @examples
#' \dontrun{
#' # Calculate overall dominance
#' overall_dominance <- find_dominance(topic_data, value_col = "Proportion")
#' 
#' # Calculate dominance for Africa region
#' africa_dominance <- find_dominance(
#'   topic_data, 
#'   value_col = "Proportion",
#'   filter_col = "region", 
#'   filter_value = "Africa"
#' )
#' }

find_dominance <- function(
    data, 
    value_col = "Proportion", 
    n = 8, 
    filter_col = NULL,
    filter_value = NULL
) {
  # Start timing
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    issues = character(),
    method_info = list(
      n = n,
      filter_applied = !is.null(filter_col) && !is.null(filter_value)
    )
  )
  
  ## --- Input validation -----------------------------------------------------
  log_message("Validating input parameters", "find_dominance")
  
  tryCatch({
    validate_input(data, c(value_col), "find_dominance")
    
    if (!is.numeric(n) || n < 1) {
      stop("n must be a positive integer")
    }
    
    if (!is.null(filter_col)) {
      if (!filter_col %in% names(data)) {
        stop("filter_col '", filter_col, "' not found in data")
      }
      if (is.null(filter_value)) {
        stop("filter_value must be provided when filter_col is specified")
      }
    }
    
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "find_dominance", "ERROR")
    diagnostics$issues <- c(diagnostics$issues, e$message)
    stop(e$message)
  })
  
  ## --- Apply filtering if requested -----------------------------------------
  if (!is.null(filter_col) && !is.null(filter_value)) {
    log_message(paste("Filtering data where", filter_col, "=", filter_value), "find_dominance")
    
    # Filter the data
    filtered_data <- data[data[[filter_col]] == filter_value, ]
    
    if (nrow(filtered_data) == 0) {
      log_message(paste("No data matches filter", filter_col, "=", filter_value), 
                  "find_dominance", "WARNING")
      return(NA_real_)
    }
    
    # Use filtered data for subsequent calculations
    data <- filtered_data
  }
  
  ## --- Calculate dominance --------------------------------------------------
  result <- time_operation({
    # Ensure we're working with numeric values
    values <- as.numeric(data[[value_col]])
    
    # Remove NAs
    values <- values[!is.na(values)]
    
    if (length(values) == 0) {
      log_message("No valid values found", "find_dominance", "WARNING")
      diagnostics$issues <- c(diagnostics$issues, "No valid values found")
      return(NA_real_)
    }
    
    # Normalize values to ensure they sum to 1
    total <- sum(values)
    if (total <= 0) {
      log_message("Sum of values is zero or negative", "find_dominance", "WARNING")
      diagnostics$issues <- c(diagnostics$issues, "Sum of values is zero or negative")
      return(NA_real_)
    }
    
    p <- values / total
    
    # Sort values in descending order
    sorted_p <- sort(p, decreasing = TRUE)
    
    # Take top n values
    n_actual <- min(length(sorted_p), n)
    
    # Add diagnostic info about actual n used
    if (n_actual < n) {
      log_message(paste("Using", n_actual, "values instead of requested", n,
                        "(not enough data)"), "find_dominance", "INFO")
    }
    
    # Calculate dominance as sum of top n proportions
    sum(sorted_p[1:n_actual])
  }, "find_dominance")
  
  return(result)
}