#' @title Calculate discourse dominance index with optional normalization
#' @description Measures discourse centralization by calculating the proportion of
#'   representation captured by the most dominant topics/categories, with optional
#'   normalization to account for different document counts. Higher values indicate
#'   more centralized discourse.
#'
#' @param data Data frame containing proportions or values to analyze
#' @param value_col Name of column containing the proportion values (default: "Proportion")
#' @param doc_id_col Name of column containing document identifiers (default: "doc_id")
#' @param n Number of top categories to include (default: 5)
#' @param normalize Sample size for normalization (default: NULL for automatic)
#'                  Set to 0 to skip normalization
#' @param iterations Number of subsampling iterations (default: NULL for automatic)
#' @param min_iterations Minimum number of iterations when auto-calculating (default: 20)
#' @param max_iterations Maximum number of iterations when auto-calculating (default: 100)
#' @param filter_col Optional column name to filter by
#' @param filter_value Optional value to filter for in filter_col
#' @param confidence Confidence level for interval calculation (default: 0.95)
#'
#' @return A list containing:
#'   \item{raw_dominance}{Unnormalized dominance value (0-1)}
#'   \item{norm_dominance}{Normalized dominance value (if normalize > 0)}
#'   \item{ci_lower}{Lower bound of confidence interval (if normalize > 0)}
#'   \item{ci_upper}{Upper bound of confidence interval (if normalize > 0)}
#'   \item{doc_count}{Number of documents analyzed}
#'
#' @examples
#' # Calculate raw dominance (no normalization)
#' find_dominance(topic_data, normalize = 0)
#' 
#' # Calculate normalized dominance with automatic sample size and iterations
#' find_dominance(topic_data)
#' 
#' # Calculate normalized dominance with specific sample size
#' find_dominance(topic_data, normalize = 10)

find_dominance <- function(
    data, 
    value_col = "Proportion", 
    doc_id_col = "doc_id",
    n = 5, 
    normalize = NULL,
    iterations = NULL,
    min_iterations = 20,
    max_iterations = 100,
    filter_col = NULL,
    filter_value = NULL,
    confidence = 0.95
) {
  # Input validation with minimal logging
  tryCatch({
    if (!is.data.frame(data)) {
      stop("data must be a dataframe")
    }
    
    if (!value_col %in% names(data)) {
      stop("value_col '", value_col, "' not found in data")
    }
    
    if (!is.null(normalize) && normalize != 0 && !doc_id_col %in% names(data)) {
      stop("doc_id_col '", doc_id_col, "' not found in data (required for normalization)")
    }
    
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
    stop(e$message)
  })
  
  ## --- Filtering -----------------------------------------
  if (!is.null(filter_col) && !is.null(filter_value)) {
    # Filter the data
    filtered_data <- data[data[[filter_col]] == filter_value, ]
    
    if (nrow(filtered_data) == 0) {
      log_message(paste("No data matches filter", filter_col, "=", filter_value), 
                  "find_dominance", "WARNING")
      
      return(list(
        raw_dominance = NA,
        norm_dominance = NA,
        ci_lower = NA,
        ci_upper = NA,
        doc_count = 0
      ))
    }
    
    # Use filtered data for subsequent calculations
    data <- filtered_data
  }
  
  ## --- Count documents ------------------------------------------------------
  if (doc_id_col %in% names(data)) {
    unique_docs <- unique(data[[doc_id_col]])
    doc_count <- length(unique_docs)
  } else {
    doc_count <- NA
  }
  
  ## --- Calculate raw dominance ----------------------------------------------
  # Sum proportions by topic across all documents
  topic_sums <- aggregate(data[[value_col]], by=list(Topic=data$Topic), FUN=sum)
  
  # Sort topics by their total proportion
  topic_sums <- topic_sums[order(topic_sums$x, decreasing = TRUE), ]
  
  # Calculate the proportion of the top n topics
  total_proportion <- sum(topic_sums$x)
  top_n_proportion <- sum(topic_sums$x[1:min(n, nrow(topic_sums))])
  
  # Calculate raw dominance as the proportion of discourse in top n topics
  raw_dominance <- top_n_proportion / total_proportion
  
  # If normalization is explicitly disabled, return the raw value
  if (!is.null(normalize) && normalize == 0) {
    return(list(
      raw_dominance = raw_dominance,
      norm_dominance = NA,
      ci_lower = NA,
      ci_upper = NA,
      doc_count = doc_count
    ))
  }
  
  ## --- Normalize --------------------------------------------
  ## Three possibilities, NULL:automatic, 0:disable, >0:manual
  # Determine sample size if automatic
  if (is.null(normalize)) {
    # Default to 80% of document count, with minimum of 5
    sample_size <- max(5, floor(doc_count * 0.8))
  } else {
    sample_size <- normalize
    # Check if we have enough documents
    if (doc_count < sample_size) {
      sample_size <- doc_count
      log_message(paste("Requested sample size (", normalize, 
                        ") exceeds available documents (", doc_count, 
                        "). Using all available documents.", sep = ""), 
                  "find_dominance", "WARNING")
    }
  }
  
  # Determine number of iterations if automatic
  if (is.null(iterations)) {
    # Calculate iterations based on document/sample ratio
    ratio <- doc_count / sample_size
    auto_iterations <- ceiling(30 * sqrt(ratio))
    
    # Keep within reasonable bounds
    iterations <- min(max(min_iterations, auto_iterations), max_iterations)
  }
  
  # Check if we have enough documents for normalization
  if (doc_count >= 5) {
    # Initialize storage for subsampling results
    subsample_results <- numeric(iterations)
    
    # Perform iterations
    for (i in 1:iterations) {
      # Sample document IDs
      sampled_ids <- sample(unique_docs, size = sample_size, replace = FALSE)
      
      # Filter data to just these documents
      sample_data <- data[data[[doc_id_col]] %in% sampled_ids, ]
      
      # Calculate dominance for this sample
      sample_topic_sums <- aggregate(sample_data[[value_col]], 
                                     by=list(Topic=sample_data$Topic), FUN=sum)
      sample_topic_sums <- sample_topic_sums[order(sample_topic_sums$x, decreasing = TRUE), ]
      sample_total <- sum(sample_topic_sums$x)
      sample_top_n <- sum(sample_topic_sums$x[1:min(n, nrow(sample_topic_sums))])
      
      # Store this iteration's dominance value
      subsample_results[i] <- sample_top_n / sample_total
    }
    
    # Calculate normalized dominance and confidence intervals
    norm_dominance <- mean(subsample_results)
    
    # Calculate confidence intervals
    ci_alpha <- (1 - confidence) / 2
    ci_bounds <- stats::quantile(subsample_results, c(ci_alpha, 1 - ci_alpha))
    ci_lower <- ci_bounds[1]
    ci_upper <- ci_bounds[2]
    
    # Return the result
    return(list(
      raw_dominance = raw_dominance,
      norm_dominance = norm_dominance,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      doc_count = doc_count
    ))
  } else {
    # Not enough documents for normalization
    log_message("Too few documents for reliable normalization", "find_dominance", "WARNING")
    
    return(list(
      raw_dominance = raw_dominance,
      norm_dominance = NA,
      ci_lower = NA,
      ci_upper = NA,
      doc_count = doc_count
    ))
  }
}