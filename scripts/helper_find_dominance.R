#' @title Calculate discourse dominance index for a specific n
#' @description Measures discourse centralization by calculating the proportion of
#'   representation captured by the most dominant topics/categories, with optional
#'   filtering to analyze specific groups or individual documents.
#'
#' @param data Data frame containing proportions or values to analyze
#' @param value_col Name of column containing the proportion values (default: "Proportion")
#' @param doc_id_col Name of column containing document identifiers (default: "doc_id")
#' @param n Number of top categories to include (default: 5)
#' @param filter_col Optional column name to filter by
#' @param filter_value Optional value to filter for in filter_col
#'
#' @return A list containing:
#'   \item{raw_dominance}{Dominance value (0-1)}
#'   \item{doc_count}{Number of documents analyzed}
#'
#' @examples
#' # Calculate dominance for all documents
#' find_dominance(topic_data, n = 5)
#' 
#' # Calculate dominance for a specific region
#' find_dominance(topic_data, n = 5, filter_col = "region", filter_value = "Africa")
#' 
#' # Calculate dominance for a single document
#' find_dominance(topic_data, n = 5, filter_col = "doc_id", filter_value = "doc_1")

find_dominance <- function(
    data, 
    value_col = "Proportion", 
    doc_id_col = "doc_id",
    n = 5, 
    filter_col = NULL,
    filter_value = NULL
) {
  ## --- Input validation -------------------------------------------------------
  # Validate data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }
  
  # Check required columns exist
  required_cols <- c(value_col, doc_id_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate n is a positive integer
  if (!is.numeric(n) || n < 1) {
    stop("n must be a positive integer")
  }
  
  # Validate filter column if provided
  if (!is.null(filter_col)) {
    if (!filter_col %in% names(data)) {
      stop("filter_col '", filter_col, "' not found in data")
    }
    if (is.null(filter_value)) {
      stop("filter_value must be provided when filter_col is specified")
    }
  }
  
  ## --- Filtering -------------------------------------------------------------
  if (!is.null(filter_col) && !is.null(filter_value)) {
    # Filter the data
    filtered_data <- data[data[[filter_col]] == filter_value, ]
    
    if (nrow(filtered_data) == 0) {
      return(list(
        raw_dominance = NA,
        doc_count = 0
      ))
    }
    
    # Use filtered data for subsequent calculations
    data <- filtered_data
  }
  
  ## --- Count documents -------------------------------------------------------
  if (doc_id_col %in% names(data)) {
    unique_docs <- unique(data[[doc_id_col]])
    doc_count <- length(unique_docs)
  } else {
    doc_count <- NA
  }
  
  ## --- Calculate dominance ---------------------------------------------------
  # Sum proportions by topic across all documents
  topic_sums <- aggregate(data[[value_col]], by=list(Topic=data$Topic), FUN=sum)
  
  # Sort topics by their total proportion
  topic_sums <- topic_sums[order(topic_sums$x, decreasing = TRUE), ]
  
  # Calculate the proportion of the top n topics
  total_proportion <- sum(topic_sums$x)
  top_n_proportion <- sum(topic_sums$x[1:min(n, nrow(topic_sums))])
  
  # Calculate dominance as the proportion of discourse in top n topics
  raw_dominance <- top_n_proportion / total_proportion
  
  # Return the result
  return(list(
    raw_dominance = raw_dominance,
    doc_count = doc_count
  ))
}