#' @title Calculate discourse dominance index with optional normalization
#' @description Measures discourse centralization by calculating the proportion of
#'   representation captured by the most dominant topics/categories, with optional
#'   normalization to account for different document counts. Higher values indicate
#'   more centralized discourse.
#'
#' @param data Data frame containing proportions or values to analyze
#' @param value_col Name of column containing the proportion values (default: "Proportion")
#' @param doc_id_col Name of column containing document identifiers (default: "doc_id")
#' @param n Number of top categories to include (default: 8)
#' @param normalize Sample size for normalization (default: NULL for automatic)
#'                  Set to 0 to skip normalization
#' @param iterations Number of subsampling iterations (default: NULL for automatic)
#' @param min_iterations Minimum number of iterations when auto-calculating (default: 20)
#' @param max_iterations Maximum number of iterations when auto-calculating (default: 100)
#' @param filter_col Optional column name to filter by
#' @param filter_value Optional value to filter for in filter_col
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
    n = 8, 
    normalize = NULL,
    iterations = NULL,
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
      normalize = normalize,
      filter_applied = !is.null(filter_col) && !is.null(filter_value)
    )
  )
  
  ## --- Input validation -----------------------------------------------------
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
    if (exists("log_message")) {
      log_message(paste("Validation error:", e$message), "find_dominance", "ERROR")
    } else {
      message(paste("ERROR:", e$message))
    }
    diagnostics$issues <- c(diagnostics$issues, e$message)
    stop(e$message)
  })
  
  ## --- Filtering -----------------------------------------
  if (!is.null(filter_col) && !is.null(filter_value)) {
    # Filter the data
    filtered_data <- data[data[[filter_col]] == filter_value, ]
    
    if (nrow(filtered_data) == 0) {
      warning(paste("No data matches filter", filter_col, "=", filter_value))
      return(list(
        raw_dominance = NA,
        norm_dominance = NA,
        ci_lower = NA,
        ci_upper = NA,
        doc_count = 0,
        error = "No data after filtering"
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
  raw_dominance <- calculate_raw_dominance(data, value_col, n)
  
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
    if (exists("log_message")) {
      log_message(paste("Auto-calculated sample size:", sample_size), "find_dominance")
    } else {
      message(paste("INFO: Auto-calculated sample size:", sample_size))
    }
  } else {
    sample_size <- normalize
    # Check if we have enough documents
    if (doc_count < sample_size) {
      sample_size <- doc_count
      warning(paste("Requested sample size (", normalize, ") exceeds available documents (", 
                    doc_count, "). Using all available documents.", sep = ""))
    }
  }
  
  # Determine number of iterations if automatic
  if (is.null(iterations)) {
    # Calculate iterations based on document/sample ratio
    ratio <- doc_count / sample_size
    auto_iterations <- ceiling(30 * sqrt(ratio))
    
    # Keep within reasonable bounds
    iterations <- min(max(20, auto_iterations), 100)
    
    if (exists("log_message")) {
      log_message(paste("Auto-calculated iterations:", iterations), "find_dominance")
    } else {
      message(paste("INFO: Auto-calculated iterations:", iterations))
    }
  }
  
  # Calculate normalized dominance via subsampling
  if (doc_count >= 5) {
    norm_result <- normalize_via_subsampling(
      data, 
      value_col, 
      doc_id_col, 
      n, 
      iterations, 
      sample_size
    )
    
    # Return the result
    return(list(
      raw_dominance = raw_dominance,
      norm_dominance = norm_result$norm_dominance,
      ci_lower = norm_result$ci_lower,
      ci_upper = norm_result$ci_upper,
      doc_count = doc_count
    ))
  } else {
    # Not enough documents for normalization
    warning("Too few documents for reliable normalization")
    return(list(
      raw_dominance = raw_dominance,
      norm_dominance = NA,
      ci_lower = NA,
      ci_upper = NA,
      doc_count = doc_count,
      error = "Too few documents for normalization"
    ))
  }
}
