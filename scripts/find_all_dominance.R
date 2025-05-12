#' @title Calculate dominance across all dimensions and values
#' @description Calculates discourse dominance for all combinations of dimensions
#'   and their values, returning a comprehensive analysis of discourse centralization
#'   patterns across different document groups.
#'
#' @param topic_data Data frame containing topic proportions
#' @param value_col Name of column containing proportion values (default: "Proportion")
#' @param doc_id_col Name of column containing document identifiers (default: "doc_id")
#' @param dimensions Vector of dimension columns to analyze (default: c("region", "wb_income_level", "is_sids", "is_lldc", "is_ldc"))
#' @param n_values Vector of top-n values to calculate dominance for (default: c(3, 5, 8))
#' @param normalize Sample size for normalization in find_dominance (default: NULL for automatic)
#' @param min_doc_count Minimum number of documents required for analysis (default: 3)
#' @param output_path Path to save results (default: "data/dominance_analysis.rds")
#'
#' @return A list containing:
#'   \item{results}{Nested list with full dominance results for each dimension-category-n combination}
#'   \item{summary}{Flat dataframe with all results for easy analysis}
#'   \item{rankings}{Analysis of which dimensions explain the most variation}
#'   \item{metadata}{Processing information including timing and document counts}
#'   \item{diagnostics}{Processing details and potential issues}
#'
#' @examples
#' \dontrun{
#' # Calculate dominance across all dimensions with default settings
#' dominance_analysis <- find_all_dominance(topic_data)
#' 
#' # Calculate for specific dimensions and n-values
#' dominance_analysis <- find_all_dominance(
#'   topic_data, 
#'   dimensions = c("region", "wb_income_level"),
#'   n_values = c(5, 10)
#' )
#' 
#' # Access results for visualization
#' bullseye(dominance_analysis$results$region$"South Asia"$n5)
#' }

find_all_dominance <- function(
    topic_data, 
    value_col = "Proportion", 
    doc_id_col = "doc_id",
    dimensions = c("region", "wb_income_level", "is_sids", "is_lldc", "is_ldc"),
    n_values = c(3, 5, 8),
    normalize = NULL,
    min_doc_count = 3,
    output_path = "data/dominance_analysis.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    skipped_categories = list(),
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating inputs and preparing for analysis", "find_all_dominance")
  
  tryCatch({
    if (!is.data.frame(topic_data)) {
      stop("topic_data must be a dataframe")
    }
    
    if (!value_col %in% names(topic_data)) {
      stop("value_col '", value_col, "' not found in topic_data")
    }
    
    if (!doc_id_col %in% names(topic_data)) {
      stop("doc_id_col '", doc_id_col, "' not found in topic_data")
    }
    
    # Validate dimensions exist in data
    missing_dims <- setdiff(dimensions, names(topic_data))
    if (length(missing_dims) > 0) {
      warning("The following dimensions were not found in the data and will be skipped: ", 
              paste(missing_dims, collapse = ", "))
      dimensions <- dimensions[!dimensions %in% missing_dims]
    }
    
    if (length(dimensions) == 0) {
      stop("No valid dimensions found in the data")
    }
    
    # Validate n_values
    if (!is.numeric(n_values) || any(n_values < 1)) {
      stop("n_values must be positive integers")
    }
    
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "find_all_dominance", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, e$message)
    stop(e$message)
  })
  
  ## --- Initialize results structures ------------------------------------------
  # Initialize nested list for full results
  results <- list()
  
  # Initialize flat dataframe for summary
  summary_results <- data.frame(
    dimension = character(),
    category = character(),
    n = integer(),
    docs = integer(),
    raw_dominance = numeric(),
    norm_dominance = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  ## --- Process each n-value --------------------------------------------------
  for (n_val in n_values) {
    log_message(paste("Calculating dominance values for n =", n_val, "..."), "find_all_dominance")
    
    # Count successful calculations for this n-value
    n_calculations <- 0
    
    # 1. Calculate overall dominance for this n-value
    results$overall <- results$overall %||% list()
    
    overall_result <- find_dominance(
      topic_data, 
      value_col = value_col, 
      doc_id_col = doc_id_col, 
      n = n_val, 
      normalize = normalize
    )
    
    # Store full result in nested structure
    results$overall[[paste0("n", n_val)]] <- overall_result
    
    # Also add to summary dataframe
    summary_results <- rbind(summary_results, data.frame(
      dimension = "Overall",
      category = "All documents",
      n = n_val,
      docs = overall_result$doc_count,
      raw_dominance = overall_result$raw_dominance,
      norm_dominance = overall_result$norm_dominance,
      ci_lower = overall_result$ci_lower,
      ci_upper = overall_result$ci_upper,
      stringsAsFactors = FALSE
    ))
    
    n_calculations <- n_calculations + 1
    
    # 2. Process each dimension for this n-value
    for (dim in dimensions) {
      if (dim %in% names(topic_data)) {
        results[[dim]] <- results[[dim]] %||% list()
        
        # Get unique values for this dimension
        unique_vals <- unique(topic_data[[dim]])
        unique_vals <- unique_vals[!is.na(unique_vals)]
        
        # Track skipped categories for this dimension
        skipped_in_dim <- 0
        
        # Process each category within this dimension
        for (val in unique_vals) {
          # Format category name
          if (is.logical(val)) {
            cat_name <- if(val) paste("Yes", dim) else paste("No", dim)
          } else {
            cat_name <- as.character(val)
          }
          
          # Count documents for this category
          doc_count <- length(unique(topic_data$doc_id[topic_data[[dim]] == val]))
          
          # Skip categories with too few documents
          if (doc_count < min_doc_count) {
            log_message(paste("Skipping", cat_name, "- only", doc_count, "documents (min:", min_doc_count, ")"),
                        "find_all_dominance", "WARNING")
            
            # Track skipped categories
            if (!dim %in% names(diagnostics$skipped_categories)) {
              diagnostics$skipped_categories[[dim]] <- character()
            }
            diagnostics$skipped_categories[[dim]] <- c(
              diagnostics$skipped_categories[[dim]], 
              cat_name
            )
            
            skipped_in_dim <- skipped_in_dim + 1
            next
          }
          
          # Initialize category in results if needed
          results[[dim]][[cat_name]] <- results[[dim]][[cat_name]] %||% list()
          
          # Calculate dominance
          dom_result <- find_dominance(
            topic_data, 
            value_col = value_col,
            doc_id_col = doc_id_col,
            n = n_val,
            normalize = normalize,
            filter_col = dim,
            filter_value = val
          )
          
          # Store full result in nested structure
          results[[dim]][[cat_name]][[paste0("n", n_val)]] <- dom_result
          
          # Also add to summary dataframe
          summary_results <- rbind(summary_results, data.frame(
            dimension = dim,
            category = cat_name,
            n = n_val,
            docs = dom_result$doc_count,
            raw_dominance = dom_result$raw_dominance,
            norm_dominance = dom_result$norm_dominance,
            ci_lower = dom_result$ci_lower,
            ci_upper = dom_result$ci_upper,
            stringsAsFactors = FALSE
          ))
          
          n_calculations <- n_calculations + 1
        }
        
        # Log dimension summary
        categories_analyzed <- length(unique_vals) - skipped_in_dim
        log_message(paste("Processed dimension:", dim, "-", categories_analyzed, "categories analyzed", 
                          ifelse(skipped_in_dim > 0, paste0("(", skipped_in_dim, " skipped)"), "")),
                    "find_all_dominance")
      }
    }
    
    # Log n-value completion
    log_message(paste("Completed dominance calculations for n =", n_val, ":", 
                      n_calculations, "total calculations"),
                "find_all_dominance")
  }
  
  ## --- Calculate dimension rankings -------------------------------------------
  log_message("Calculating dimension rankings", "find_all_dominance")
  
  # Define the rank_dimensions function inside
  rank_dimensions <- function(dominance_results) {
    # Define dimension categories
    dimension_categories <- list(
      "region" = "administrative",
      "wb_income_level" = "economic",
      "is_sids" = "geography",
      "is_lldc" = "geography",
      "is_ldc" = "economic"  # LDC is considered economic rather than purely geographic
    )
    
    # Add category column to the results
    results_with_categories <- dominance_results %>%
      dplyr::filter(dimension != "Overall") %>%
      dplyr::mutate(
        dimension_category = sapply(dimension, function(d) {
          if (d %in% names(dimension_categories)) {
            dimension_categories[[d]]
          } else {
            "other"
          }
        })
      )
    
    # Calculate dimension-level statistics
    dimension_stats <- results_with_categories %>%
      dplyr::group_by(dimension, n) %>%
      dplyr::summarize(
        min_dominance = min(norm_dominance, na.rm = TRUE),
        max_dominance = max(norm_dominance, na.rm = TRUE),
        range = max_dominance - min_dominance,
        std_dev = stats::sd(norm_dominance, na.rm = TRUE),
        mean_dominance = mean(norm_dominance, na.rm = TRUE),
        coef_var = if(mean_dominance > 0) std_dev / mean_dominance else NA,
        count = dplyr::n(),
        analysis_level = "dimension",
        .groups = "drop"
      ) %>%
      dplyr::arrange(n, dplyr::desc(range))
    
    # Calculate category-level statistics
    category_stats <- results_with_categories %>%
      dplyr::group_by(dimension_category, n) %>%
      dplyr::summarize(
        min_dominance = min(norm_dominance, na.rm = TRUE),
        max_dominance = max(norm_dominance, na.rm = TRUE),
        range = max_dominance - min_dominance,
        std_dev = stats::sd(norm_dominance, na.rm = TRUE),
        mean_dominance = mean(norm_dominance, na.rm = TRUE),
        coef_var = if(mean_dominance > 0) std_dev / mean_dominance else NA,
        count = dplyr::n(),
        analysis_level = "category",
        .groups = "drop"
      ) %>%
      dplyr::rename(dimension = dimension_category) %>%  # Rename for consistent column structure
      dplyr::arrange(n, dplyr::desc(range))
    
    # Combine both levels of analysis
    combined_stats <- dplyr::bind_rows(dimension_stats, category_stats) %>%
      dplyr::arrange(n, analysis_level, dplyr::desc(range))
    
    return(combined_stats)
  }
  
  dimension_rankings <- rank_dimensions(summary_results)
  
  ## --- Finalize and return results --------------------------------------------
  # Round numerical columns for easier reading
  summary_results <- summary_results %>%
    dplyr::mutate(
      raw_dominance = round(raw_dominance, 3),
      norm_dominance = round(norm_dominance, 3),
      ci_lower = round(ci_lower, 3),
      ci_upper = round(ci_upper, 3)
    )
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    dimensions_analyzed = dimensions,
    n_values = n_values,
    total_categories = nrow(summary_results),
    min_doc_count = min_doc_count
  )
  
  # Create final result
  final_result <- list(
    results = results,                 # Nested list with full find_dominance output
    summary = summary_results,         # Flat dataframe for easy analysis
    rankings = dimension_rankings,     # Dimension rankings
    metadata = metadata,
    diagnostics = diagnostics
  )
  
  # Log completion
  log_message(paste("Dominance analysis complete:", length(dimensions), "dimensions,", 
                    length(n_values), "n-values,", nrow(summary_results), "total calculations",
                    "in", round(processing_time, 2), "seconds"), 
              "find_all_dominance")
  
  # Save results if output_path is provided
  if (!is.null(output_path)) {
    log_message(paste("Saving results to", output_path), "find_all_dominance")
    saveRDS(final_result, output_path)
  }
  
  return(final_result)
}