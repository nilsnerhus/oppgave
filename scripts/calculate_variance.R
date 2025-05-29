#' @title Calculate Variance Explained by Metadata Categories with Bootstrap CIs
#' @description Replicates the functionality of estimate_variance.R while adding
#'   bootstrap confidence intervals. Maintains exact compatibility with original results.
#'   
#' @param model Result from fit_model() containing the STM model and metadata
#' @param dominance Optional result from calculate_dominance() containing top topic IDs
#' @param n_bootstrap Number of bootstrap iterations (default: 1000, set to 0 to disable)
#' @param conf_level Confidence level for intervals (default: 0.95)
#'
#' @return A list containing variance metrics with optional confidence intervals
calculate_variance <- function(model, dominance = NULL, n_bootstrap = 1000, conf_level = 0.95) {
  ## --- Setup & Initialization -------------------------------------------------
  log_message("Starting variance calculation with bootstrap CIs", "calculate_variance")
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    topic_level_data = NULL,
    variance_summary = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "calculate_variance")
  
  if (!validate_model_structure(model, diagnostics)) {
    return(create_error_result(diagnostics))
  }
  
  ## --- Extract components -----------------------------------------------------
  theta <- model$data$model$theta  
  meta_data <- model$data$aligned_meta  
  category_map <- model$data$category_map
  k <- ncol(theta)
  
  log_message(paste("Estimating variance for", k, "topics across", 
                    length(category_map), "category dimensions"), "calculate_variance")
  
  if (n_bootstrap > 0) {
    log_message(paste("Bootstrap confidence intervals enabled with", n_bootstrap, "iterations"), "calculate_variance")
  }
  
  ## --- Get top topics for each category (if dominance provided) ---------------
  top_topics_map <- extract_top_topics(dominance)
  
  ## --- Main processing --------------------------------------------------------
  results <- data.frame(
    level_type = character(),
    category = character(),
    subcategory = character(),
    variance_explained = numeric(),  # Will store as proportions (0-1)
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  detailed_results <- list()
  
  # Process each category
  for (category_name in names(category_map)) {
    log_message(paste("Processing category:", category_name), "calculate_variance")
    
    category_results <- process_category_with_bootstrap(
      category_name = category_name,
      dimensions = category_map[[category_name]],
      theta = theta,
      meta_data = meta_data,
      top_topics_map = top_topics_map,
      k = k,
      n_bootstrap = n_bootstrap,
      conf_level = conf_level
    )
    
    if (!is.null(category_results$results)) {
      results <- rbind(results, category_results$results)
      detailed_results <- c(detailed_results, list(category_results$details))
    }
    
    # Log category summary with percentages for clarity
    if (!is.null(category_results$category_avg)) {
      ci_text <- if (n_bootstrap > 0 && !is.na(category_results$ci_lower)) {
        sprintf(" (95%% CI: %.1f%% - %.1f%%)", 
                category_results$ci_lower * 100, category_results$ci_upper * 100)
      } else {
        ""
      }
      
      log_message(paste(category_name, "explains an average of", 
                        sprintf("%.1f%%", category_results$category_avg * 100), 
                        "of topic variance", ci_text), "calculate_variance")
    }
  }
  
  ## --- Calculate overall summary ----------------------------------------------
  if (nrow(results) > 0) {
    dimension_rows <- results[results$level_type == "dimension", ]
    if (nrow(dimension_rows) > 0) {
      overall_avg <- mean(dimension_rows$variance_explained)
      
      # For overall CI, use the range of dimension CIs
      if (n_bootstrap > 0) {
        overall_ci_lower <- min(dimension_rows$ci_lower, na.rm = TRUE)
        overall_ci_upper <- max(dimension_rows$ci_upper, na.rm = TRUE)
        
        # If all CIs are NA, set overall to NA
        if (is.infinite(overall_ci_lower)) {
          overall_ci_lower <- NA
          overall_ci_upper <- NA
        }
      } else {
        overall_ci_lower <- NA
        overall_ci_upper <- NA
      }
      
      results <- rbind(results, 
                       data.frame(
                         level_type = "overall",
                         category = "ALL_CATEGORIES", 
                         subcategory = "AVERAGE",
                         variance_explained = overall_avg,
                         ci_lower = overall_ci_lower,
                         ci_upper = overall_ci_upper,
                         stringsAsFactors = FALSE
                       ))
      
      # Log overall summary
      ci_text <- if (n_bootstrap > 0 && !is.na(overall_ci_lower)) {
        sprintf(" (95%% CI: %.1f%% - %.1f%%)", 
                overall_ci_lower * 100, overall_ci_upper * 100)
      } else {
        ""
      }
      
      log_message(paste("Overall, metadata explains", 
                        sprintf("%.1f%%", overall_avg * 100), 
                        "of topic variance on average", ci_text), "calculate_variance")
    }
  }
  
  ## --- Sort and store detailed results ----------------------------------------
  results <- sort_results(results)
  diagnostics$topic_level_data <- do.call(rbind, detailed_results)
  
  # Add summary statistics to diagnostics
  if (nrow(results) > 0) {
    category_rows <- results[results$level_type == "category", ]
    diagnostics$variance_summary <- list(
      max_category = category_rows$category[which.max(category_rows$variance_explained)],
      max_variance = max(category_rows$variance_explained),
      min_category = category_rows$category[which.min(category_rows$variance_explained)],
      min_variance = min(category_rows$variance_explained),
      n_dimensions_tested = nrow(results[results$level_type == "dimension", ])
    )
  }
  
  ## --- Create result ----------------------------------------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_message("Variance estimation with bootstrap CIs complete", "calculate_variance")
  
  return(create_result(
    data = results,
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      n_topics = k,
      n_documents = nrow(theta),
      n_categories = length(category_map),
      n_bootstrap = n_bootstrap,
      conf_level = conf_level,
      success = nrow(results) > 0
    ),
    diagnostics = diagnostics
  ))
}

## --- Helper functions --------------------------------------------------------

#' Process a single category with bootstrap
process_category_with_bootstrap <- function(category_name, dimensions, theta, meta_data, 
                                            top_topics_map, k, n_bootstrap, conf_level) {
  
  results <- data.frame(
    level_type = character(),
    category = character(),
    subcategory = character(),
    variance_explained = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  detailed_results <- list()
  dimension_variances <- numeric()
  dimension_cis <- list()
  
  # Determine which topics to analyze
  topics_to_analyze <- if (category_name %in% names(top_topics_map)) {
    top_topics_map[[category_name]]
  } else {
    1:k
  }
  
  # Process each dimension
  for (dimension in dimensions) {
    if (!dimension %in% names(meta_data)) {
      log_message(paste("Dimension", dimension, "not found in metadata"), 
                  "calculate_variance", "WARNING")
      next
    }
    
    dim_results <- process_dimension_with_bootstrap(
      dimension = dimension,
      category_name = category_name,
      category_var = meta_data[[dimension]],
      theta = theta,
      topics_to_analyze = topics_to_analyze,
      n_bootstrap = n_bootstrap,
      conf_level = conf_level
    )
    
    if (!is.null(dim_results)) {
      results <- rbind(results, dim_results$summary)
      detailed_results <- c(detailed_results, list(dim_results$details))
      
      if (!is.na(dim_results$avg_variance)) {
        dimension_variances <- c(dimension_variances, dim_results$avg_variance)
        dimension_cis[[length(dimension_cis) + 1]] <- c(dim_results$ci_lower, dim_results$ci_upper)
        
        ci_text <- if (n_bootstrap > 0 && !is.na(dim_results$ci_lower)) {
          sprintf(" (95%% CI: %.1f%% - %.1f%%)", 
                  dim_results$ci_lower * 100, dim_results$ci_upper * 100)
        } else {
          ""
        }
        
        log_message(paste("  ", dimension, "explains", 
                          sprintf("%.1f%%", dim_results$avg_variance * 100), 
                          "of variance", ci_text), "calculate_variance")
      }
    }
  }
  
  # Add category summary
  category_avg <- NULL
  category_ci_lower <- NA
  category_ci_upper <- NA
  
  if (length(dimension_variances) > 0) {
    category_avg <- mean(dimension_variances)
    
    # For category CI, use the range of dimension CIs
    if (n_bootstrap > 0 && length(dimension_cis) > 0) {
      all_lowers <- sapply(dimension_cis, function(x) x[1])
      all_uppers <- sapply(dimension_cis, function(x) x[2])
      
      if (!all(is.na(all_lowers))) {
        category_ci_lower <- min(all_lowers, na.rm = TRUE)
        category_ci_upper <- max(all_uppers, na.rm = TRUE)
      }
    }
    
    results <- rbind(results,
                     data.frame(
                       level_type = "category",
                       category = category_name,
                       subcategory = "Overall",
                       variance_explained = category_avg,
                       ci_lower = category_ci_lower,
                       ci_upper = category_ci_upper,
                       stringsAsFactors = FALSE
                     ))
  }
  
  return(list(
    results = results,
    details = do.call(rbind, detailed_results),
    category_avg = category_avg,
    ci_lower = category_ci_lower,
    ci_upper = category_ci_upper
  ))
}

#' Process a single dimension with bootstrap
process_dimension_with_bootstrap <- function(dimension, category_name, category_var, 
                                             theta, topics_to_analyze, n_bootstrap, conf_level) {
  
  # Ensure categorical variables are factors
  if (is.character(category_var)) {
    category_var <- as.factor(category_var)
  }
  
  # Calculate variance using the new find_variance function
  result <- find_variance(theta, category_var, topics_to_analyze, n_bootstrap, conf_level)
  
  if (is.null(result)) {
    return(NULL)
  }
  
  # Create summary results
  summary_results <- data.frame(
    level_type = "dimension",
    category = category_name,
    subcategory = dimension,
    variance_explained = result$estimate,
    ci_lower = result$ci_lower,
    ci_upper = result$ci_upper,
    stringsAsFactors = FALSE
  )
  
  # Add subcategory results for factors
  if (is.factor(category_var) && nlevels(category_var) > 1) {
    subcategory_results <- calculate_subcategory_variance_with_bootstrap(
      category_var, theta, topics_to_analyze, category_name, dimension,
      n_bootstrap, conf_level
    )
    summary_results <- rbind(summary_results, subcategory_results)
  }
  
  # Add binary variable subcategories
  if (is.logical(category_var) || 
      (is.factor(category_var) && nlevels(category_var) == 2)) {
    
    subcategory_name <- if (grepl("^is_", dimension)) {
      toupper(gsub("^is_", "", dimension))
    } else if (is.factor(category_var)) {
      levels(category_var)[2]
    } else {
      "TRUE"
    }
    
    summary_results <- rbind(summary_results,
                             data.frame(
                               level_type = "subcategory",
                               category = category_name,
                               subcategory = subcategory_name,
                               variance_explained = result$estimate,
                               ci_lower = result$ci_lower,
                               ci_upper = result$ci_upper,
                               stringsAsFactors = FALSE
                             ))
  }
  
  # Create detailed results (topic-level breakdown)
  detailed_results <- data.frame(
    category = category_name,
    dimension = dimension,
    topic = topics_to_analyze,
    variance_explained = NA,  # Could calculate individual topic variances if needed
    stringsAsFactors = FALSE
  )
  
  return(list(
    summary = summary_results,
    details = detailed_results,
    avg_variance = result$estimate,
    ci_lower = result$ci_lower,
    ci_upper = result$ci_upper
  ))
}

#' Calculate variance for subcategories with bootstrap
calculate_subcategory_variance_with_bootstrap <- function(category_var, theta, topics_to_analyze, 
                                                          category_name, dimension, n_bootstrap, conf_level) {
  results <- list()
  
  for (level in levels(category_var)) {
    binary_indicator <- category_var == level
    
    # Use find_variance for each subcategory
    subcat_result <- find_variance(theta, binary_indicator, topics_to_analyze, n_bootstrap, conf_level)
    
    if (!is.null(subcat_result)) {
      results[[length(results) + 1]] <- data.frame(
        level_type = "subcategory",
        category = category_name,
        subcategory = level,
        variance_explained = subcat_result$estimate,
        ci_lower = subcat_result$ci_lower,
        ci_upper = subcat_result$ci_upper,
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(do.call(rbind, results))
}

## --- Shared helper functions ------------------------------------------------

#' Extract top topics from dominance data (matching original function exactly)
extract_top_topics <- function(dominance) {
  if (is.null(dominance) || !"data" %in% names(dominance)) {
    return(list())
  }
  
  top_topics_map <- list()
  
  # Process dominance data to get top topics for each category/subcategory
  for (i in 1:nrow(dominance$data)) {
    if (dominance$data$level_type[i] == "corpus") {
      category <- dominance$data$category[i]
      subcategory <- dominance$data$subcategory[i]
      top_ids_str <- dominance$data$top_topic_ids[i]
      
      if (!is.na(top_ids_str) && nchar(top_ids_str) > 0) {
        top_ids <- as.numeric(unlist(strsplit(top_ids_str, ",")))
        
        if (category == "Overall" && subcategory == "Overall") {
          top_topics_map[["Overall"]] <- top_ids
        } else {
          top_topics_map[[category]] <- top_ids
        }
      }
    }
  }
  
  return(top_topics_map)
}

#' Validate model structure (unchanged)
validate_model_structure <- function(model, diagnostics) {
  if (!is.list(model) || 
      !"data" %in% names(model) || 
      !"model" %in% names(model$data) || 
      !"category_map" %in% names(model$data)) {
    error_msg <- "Model must contain 'data$model' and 'data$category_map'"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "calculate_variance", "ERROR")
    return(FALSE)
  }
  return(TRUE)
}

#' Create error result (unchanged)
create_error_result <- function(diagnostics) {
  create_result(
    data = NULL,
    metadata = list(
      timestamp = Sys.time(),
      success = FALSE
    ),
    diagnostics = diagnostics
  )
}

#' Sort results by category and level (unchanged)
sort_results <- function(results) {
  if (nrow(results) == 0) return(results)
  
  # Create sort order
  level_order <- c("subcategory", "dimension", "category", "overall")
  results$level_order <- match(results$level_type, level_order)
  
  # Sort
  results <- results[order(results$category, results$level_order, 
                           -results$variance_explained), ]
  
  # Remove temporary column
  results$level_order <- NULL
  
  return(results)
}