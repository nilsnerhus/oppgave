#' @title Estimate Variance Explained by Metadata Categories
#' @description Calculates how much variance in topic proportions is explained by 
#'   metadata categories using variance decomposition. Stores results as proportions
#'   (0-1) but logs them as percentages for clarity.
#'   
#' @param model Result from fit_model() containing the STM model and metadata
#' @param dominance Optional result from calculate_dominance() containing top topic IDs
#'
#' @return A list containing:
#'   \item{data}{Data frame with variance explained (as proportions) by categories and dimensions}
#'   \item{metadata}{Processing information}
#'   \item{diagnostics}{Processing issues and detailed topic-level data}
estimate_variance <- function(model, dominance = NULL) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics
  diagnostics <- list(
    processing_issues = character(),
    topic_level_data = NULL,
    variance_summary = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "estimate_variance")
  
  if (!validate_model_structure(model, diagnostics)) {
    return(create_error_result(diagnostics))
  }
  
  ## --- Extract components -----------------------------------------------------
  theta <- model$data$model$theta  
  meta_data <- model$data$aligned_meta  
  category_map <- model$data$category_map
  k <- ncol(theta)
  
  log_message(paste("Estimating variance for", k, "topics across", 
                    length(category_map), "category dimensions"), "estimate_variance")
  
  ## --- Get top topics for each category (if dominance provided) ---------------
  top_topics_map <- extract_top_topics(dominance)
  
  ## --- Main processing --------------------------------------------------------
  results <- data.frame(
    level_type = character(),
    category = character(),
    subcategory = character(),
    variance_explained = numeric(),  # Will store as proportions (0-1)
    stringsAsFactors = FALSE
  )
  
  detailed_results <- list()
  
  # Process each category
  for (category_name in names(category_map)) {
    log_message(paste("Processing category:", category_name), "estimate_variance")
    
    category_results <- process_category(
      category_name = category_name,
      dimensions = category_map[[category_name]],
      theta = theta,
      meta_data = meta_data,
      top_topics_map = top_topics_map,
      k = k
    )
    
    if (!is.null(category_results$results)) {
      results <- rbind(results, category_results$results)
      detailed_results <- c(detailed_results, list(category_results$details))
    }
    
    # Log category summary with percentages for clarity
    if (!is.null(category_results$category_avg)) {
      log_message(paste(category_name, "explains an average of", 
                        sprintf("%.1f%%", category_results$category_avg * 100), 
                        "of topic variance"), "estimate_variance")
    }
  }
  
  ## --- Calculate overall summary ----------------------------------------------
  if (nrow(results) > 0) {
    dimension_rows <- results[results$level_type == "dimension", ]
    if (nrow(dimension_rows) > 0) {
      overall_avg <- mean(dimension_rows$variance_explained)
      
      results <- rbind(results, 
                       data.frame(
                         level_type = "overall",
                         category = "ALL_CATEGORIES", 
                         subcategory = "AVERAGE",
                         variance_explained = overall_avg,
                         stringsAsFactors = FALSE
                       ))
      
      log_message(paste("Overall, metadata explains", 
                        sprintf("%.1f%%", overall_avg * 100), 
                        "of topic variance on average"), "estimate_variance")
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
  log_message("Variance estimation complete", "estimate_variance")
  
  return(create_result(
    data = results,
    metadata = list(
      timestamp = start_time,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      n_topics = k,
      n_documents = nrow(theta),
      n_categories = length(category_map),
      success = nrow(results) > 0
    ),
    diagnostics = diagnostics
  ))
}

## --- Helper functions --------------------------------------------------------

#' Validate model structure
validate_model_structure <- function(model, diagnostics) {
  if (!is.list(model) || 
      !"data" %in% names(model) || 
      !"model" %in% names(model$data) || 
      !"category_map" %in% names(model$data)) {
    error_msg <- "Model must contain 'data$model' and 'data$category_map'"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "estimate_variance", "ERROR")
    return(FALSE)
  }
  return(TRUE)
}

#' Create error result
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

#' Extract top topics from dominance data
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

#' Calculate variance explained by a categorical variable
calculate_variance_explained <- function(topic_props, category_var) {
  # Skip if variable has only one level or all NAs
  if (length(unique(na.omit(category_var))) <= 1) {
    return(NA)
  }
  
  # Total variance
  total_mean <- mean(topic_props, na.rm = TRUE)
  ss_total <- sum((topic_props - total_mean)^2, na.rm = TRUE)
  
  if (ss_total == 0) return(0)
  
  # Between-group variance
  group_stats <- aggregate(topic_props, 
                           by = list(category = category_var), 
                           FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                               n = sum(!is.na(x))))
  
  ss_between <- sum(group_stats$x[,"n"] * 
                      (group_stats$x[,"mean"] - total_mean)^2, 
                    na.rm = TRUE)
  
  # Return proportion (not percentage)
  return(ss_between / ss_total)
}

#' Process a single category
process_category <- function(category_name, dimensions, theta, meta_data, 
                             top_topics_map, k) {
  
  results <- data.frame(
    level_type = character(),
    category = character(),
    subcategory = character(),
    variance_explained = numeric(),
    stringsAsFactors = FALSE
  )
  
  detailed_results <- list()
  dimension_variances <- numeric()
  
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
                  "estimate_variance", "WARNING")
      next
    }
    
    dim_results <- process_dimension(
      dimension = dimension,
      category_name = category_name,
      category_var = meta_data[[dimension]],
      theta = theta,
      topics_to_analyze = topics_to_analyze
    )
    
    if (!is.null(dim_results)) {
      results <- rbind(results, dim_results$summary)
      detailed_results <- c(detailed_results, list(dim_results$details))
      
      if (!is.na(dim_results$avg_variance)) {
        dimension_variances <- c(dimension_variances, dim_results$avg_variance)
        
        log_message(paste("  ", dimension, "explains", 
                          sprintf("%.1f%%", dim_results$avg_variance * 100), 
                          "of variance"), "estimate_variance")
      }
    }
  }
  
  # Add category summary
  category_avg <- NULL
  if (length(dimension_variances) > 0) {
    category_avg <- mean(dimension_variances)
    results <- rbind(results,
                     data.frame(
                       level_type = "category",
                       category = category_name,
                       subcategory = "Overall",
                       variance_explained = category_avg,
                       stringsAsFactors = FALSE
                     ))
  }
  
  return(list(
    results = results,
    details = do.call(rbind, detailed_results),
    category_avg = category_avg
  ))
}

#' Process a single dimension
process_dimension <- function(dimension, category_name, category_var, 
                              theta, topics_to_analyze) {
  
  # Ensure categorical variables are factors
  if (is.character(category_var)) {
    category_var <- as.factor(category_var)
  }
  
  # Calculate variance for each topic
  topic_variances <- numeric()
  detailed_results <- list()
  
  for (topic_id in topics_to_analyze) {
    topic_props <- theta[, topic_id]
    var_explained <- calculate_variance_explained(topic_props, category_var)
    
    if (!is.na(var_explained)) {
      topic_variances <- c(topic_variances, var_explained)
      
      detailed_results[[length(detailed_results) + 1]] <- data.frame(
        category = category_name,
        dimension = dimension,
        topic = topic_id,
        variance_explained = var_explained,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Calculate average variance
  avg_variance <- if (length(topic_variances) > 0) {
    mean(topic_variances)
  } else {
    NA
  }
  
  # Create summary results
  summary_results <- data.frame(
    level_type = "dimension",
    category = category_name,
    subcategory = dimension,
    variance_explained = avg_variance,
    stringsAsFactors = FALSE
  )
  
  # Add subcategory results for factors
  if (is.factor(category_var) && nlevels(category_var) > 1) {
    subcategory_results <- calculate_subcategory_variance(
      category_var, theta, topics_to_analyze, category_name, dimension
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
                               variance_explained = avg_variance,
                               stringsAsFactors = FALSE
                             ))
  }
  
  return(list(
    summary = summary_results,
    details = do.call(rbind, detailed_results),
    avg_variance = avg_variance
  ))
}

#' Calculate variance for subcategories
calculate_subcategory_variance <- function(category_var, theta, topics_to_analyze, 
                                           category_name, dimension) {
  results <- list()
  
  for (level in levels(category_var)) {
    binary_indicator <- category_var == level
    level_variances <- numeric()
    
    for (topic_id in topics_to_analyze) {
      topic_props <- theta[, topic_id]
      var_explained <- calculate_variance_explained(topic_props, binary_indicator)
      
      if (!is.na(var_explained)) {
        level_variances <- c(level_variances, var_explained)
      }
    }
    
    if (length(level_variances) > 0) {
      results[[length(results) + 1]] <- data.frame(
        level_type = "subcategory",
        category = category_name,
        subcategory = level,
        variance_explained = mean(level_variances),
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(do.call(rbind, results))
}

#' Sort results by category and level
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