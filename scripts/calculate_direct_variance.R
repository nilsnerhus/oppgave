#' @title Calculate Variance Explained Directly from Topic Proportions
#' @description Calculates how much variance in topic proportions is explained by 
#'   metadata categories using direct statistical calculations.
#'   
#' @param model Result from fit_model() containing the STM model and metadata
#' @param dominance Result from calculate_dominance() containing top topic IDs
#'
#' @return A list containing:
#'   \item{data}{Data frame with variance explained by categories and dimensions}
#'   \item{metadata}{Processing information}
#'   \item{diagnostics}{Processing issues and detailed topic-level data}
calculate_direct_variance <- function(model, dominance = NULL) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    topic_level_data = NULL  # We'll store the detailed per-topic data here
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "calculate_direct_variance")
  
  # Validate model structure
  if (!is.list(model) || 
      !"data" %in% names(model) || 
      !"model" %in% names(model$data) || 
      !"category_map" %in% names(model$data)) {
    error_msg <- "Model must contain 'data$model' and 'data$category_map'"
    log_message(error_msg, "calculate_direct_variance", "ERROR")
    return(NULL)
  }
  
  ## --- Extract needed components ----------------------------------------------
  theta <- model$data$model$theta  # Topic proportions matrix
  meta_data <- model$data$aligned_meta  # Metadata
  category_map <- model$data$category_map  # Category definitions
  
  # Initialize results table in requested format
  results <- data.frame(
    level_type = character(),
    category = character(),
    subcategory = character(),
    variance_explained = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Initialize detailed results (for diagnostics)
  detailed_results <- data.frame(
    category = character(),
    dimension = character(),
    topic = numeric(),
    variance_explained = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Helper function to calculate variance explained
  calculate_cat_variance <- function(topic_props, category_var) {
    # Skip if variable has only one level or all NAs
    if (length(unique(na.omit(category_var))) <= 1) {
      return(NA)
    }
    
    # Calculate total variance (SS_total)
    total_mean <- mean(topic_props, na.rm = TRUE)
    ss_total <- sum((topic_props - total_mean)^2, na.rm = TRUE)
    
    if (ss_total == 0) {
      return(0)  # No variance to explain
    }
    
    # Calculate between-group variance (SS_between)
    # For each category level, get mean and count
    aggregated <- stats::aggregate(topic_props, by = list(category_var), 
                                   function(x) c(mean = mean(x, na.rm = TRUE), 
                                                 count = sum(!is.na(x))))
    
    # Calculate SS_between
    ss_between <- sum(aggregated$x[,2] * (aggregated$x[,1] - total_mean)^2, na.rm = TRUE)
    
    # Return proportion of variance explained
    return(ss_between / ss_total * 100)
  }
  
  ## --- Process all topics and dimensions --------------------------------------
  log_message("Calculating direct variance explained", "calculate_direct_variance")
  
  # Get number of topics
  k <- ncol(theta)
  
  # If dominance data is provided, use it to focus on top topics
  top_topics_by_category <- list()
  
  if (!is.null(dominance) && "data" %in% names(dominance) && 
      "top_topic_ids" %in% names(dominance$data)) {
    log_message("Using dominance data to identify top topics", "calculate_direct_variance")
    
    # Process dominance data to get top topics for each category/subcategory
    for (i in 1:nrow(dominance$data)) {
      if (dominance$data$level_type[i] == "corpus") {
        category <- dominance$data$category[i]
        subcategory <- dominance$data$subcategory[i]
        top_ids_str <- dominance$data$top_topic_ids[i]
        
        if (!is.na(top_ids_str) && nchar(top_ids_str) > 0) {
          top_ids <- as.numeric(unlist(strsplit(top_ids_str, ",")))
          
          # Store for overall, category and subcategory levels
          if (category == "Overall" && subcategory == "Overall") {
            top_topics_by_category[["Overall"]] <- top_ids
          } else if (subcategory == paste0(category, "_TOTAL")) {
            top_topics_by_category[[category]] <- top_ids
          } else {
            top_topics_by_category[[paste(category, subcategory, sep = "_")]] <- top_ids
          }
        }
      }
    }
    
    log_message(paste("Found top topics for", length(top_topics_by_category), "categories/subcategories"), 
                "calculate_direct_variance")
  }
  
  # Process each category in the category map
  all_dimension_variances <- list()  # To store dimension variances for overall calculation
  
  for (category in names(category_map)) {
    dimensions <- category_map[[category]]
    category_variances <- numeric()  # To store all variance values for this category
    
    # Determine which topics to process for this category
    if (category %in% names(top_topics_by_category)) {
      topics_to_process <- top_topics_by_category[[category]]
      log_message(paste("Using top topics for", category, ":", 
                        paste(topics_to_process, collapse = ", ")), 
                  "calculate_direct_variance")
    } else {
      topics_to_process <- 1:k
      log_message(paste("Using all topics for", category), "calculate_direct_variance")
    }
    
    # Process each dimension in this category
    for (dim in dimensions) {
      if (!dim %in% names(meta_data)) {
        log_message(paste("Dimension", dim, "not found in metadata"), 
                    "calculate_direct_variance", "WARNING")
        next
      }
      
      # Ensure categorical variables are factors
      if (is.character(meta_data[[dim]])) {
        meta_data[[dim]] <- as.factor(meta_data[[dim]])
      }
      
      # Process each topic
      dimension_topic_variances <- numeric()
      
      for (topic_id in topics_to_process) {
        # Get topic proportions for this topic
        topic_props <- theta[, topic_id]
        
        # Calculate overall variance explained by this dimension for this topic
        var_explained <- calculate_cat_variance(topic_props, meta_data[[dim]])
        
        if (!is.na(var_explained)) {
          dimension_topic_variances <- c(dimension_topic_variances, var_explained)
          
          # Add to detailed results (for diagnostics)
          detailed_results <- rbind(detailed_results, 
                                    data.frame(
                                      category = category,
                                      dimension = dim,
                                      topic = topic_id,
                                      variance_explained = var_explained,
                                      stringsAsFactors = FALSE
                                    ))
        }
        
        # For categorical variables, calculate subcategory-specific variance
        if (is.factor(meta_data[[dim]]) && length(levels(meta_data[[dim]])) > 2) {
          # Get the factor levels
          levels_vec <- levels(meta_data[[dim]])
          
          # Calculate variance explained by each level
          for (level in levels_vec) {
            # Create a binary indicator for this level
            binary_indicator <- meta_data[[dim]] == level
            
            # Calculate variance explained by this specific level
            level_var <- calculate_cat_variance(topic_props, binary_indicator)
            
            if (!is.na(level_var)) {
              # Add to detailed results
              detailed_results <- rbind(detailed_results, 
                                        data.frame(
                                          category = category,
                                          dimension = paste0(dim, "_", level),
                                          topic = topic_id,
                                          variance_explained = level_var,
                                          stringsAsFactors = FALSE
                                        ))
            }
          }
        }
      }
      
      # Calculate average variance explained by this dimension across topics
      if (length(dimension_topic_variances) > 0) {
        dim_avg_var <- mean(dimension_topic_variances, na.rm = TRUE)
        category_variances <- c(category_variances, dim_avg_var)
        all_dimension_variances[[dim]] <- dim_avg_var
        
        # Add dimension-level row
        results <- rbind(results, 
                         data.frame(
                           level_type = "dimension",
                           category = category,
                           subcategory = dim,
                           variance_explained = dim_avg_var,
                           stringsAsFactors = FALSE
                         ))
        
        log_message(paste(dim, "explains an average of", 
                          round(dim_avg_var, 2), "% of topic variance"), 
                    "calculate_direct_variance")
        
        # For categorical variables, add subcategory-specific rows
        if (is.factor(meta_data[[dim]]) && length(levels(meta_data[[dim]])) > 2) {
          levels_vec <- levels(meta_data[[dim]])
          
          for (level in levels_vec) {
            # Get subcategory-specific rows for this level
            level_rows <- which(detailed_results$dimension == paste0(dim, "_", level) & 
                                  detailed_results$category == category)
            
            if (length(level_rows) > 0) {
              level_avg_var <- mean(detailed_results$variance_explained[level_rows], na.rm = TRUE)
              
              # Add subcategory-level row
              results <- rbind(results, 
                               data.frame(
                                 level_type = "subcategory",
                                 category = category,
                                 subcategory = level,
                                 variance_explained = level_avg_var,
                                 stringsAsFactors = FALSE
                               ))
              
              log_message(paste("Subcategory", level, "explains an average of", 
                                round(level_avg_var, 2), "% of topic variance"), 
                          "calculate_direct_variance")
            }
          }
        }
        
        # For binary variables, add them as subcategories too (like is_sids)
        if (is.logical(meta_data[[dim]]) || 
            (is.factor(meta_data[[dim]]) && length(levels(meta_data[[dim]])) == 2)) {
          
          # For is_X type variables, use the X as subcategory name
          if (grepl("^is_", dim)) {
            subcategory_name <- gsub("^is_", "", dim)
            subcategory_name <- toupper(subcategory_name)  # Convert to uppercase
          } else {
            # For other binary variables, use TRUE value
            subcategory_name <- if (is.factor(meta_data[[dim]])) levels(meta_data[[dim]])[2] else "TRUE"
          }
          
          # Add subcategory-level row with same value as dimension
          results <- rbind(results, 
                           data.frame(
                             level_type = "subcategory",
                             category = category,
                             subcategory = subcategory_name,
                             variance_explained = dim_avg_var,
                             stringsAsFactors = FALSE
                           ))
        }
      }
    }
    
    # Calculate category average
    if (length(category_variances) > 0) {
      cat_avg_var <- mean(category_variances, na.rm = TRUE)
      
      # Add category-level summary row
      results <- rbind(results, 
                       data.frame(
                         level_type = "category",
                         category = category,
                         subcategory = "Overall",
                         variance_explained = cat_avg_var,
                         stringsAsFactors = FALSE
                       ))
      
      log_message(paste(category, "overall explains", 
                        round(cat_avg_var, 2), "% of topic variance"), 
                  "calculate_direct_variance")
    }
  }
  
  # Calculate overall average across all dimensions
  if (length(all_dimension_variances) > 0) {
    overall_avg <- mean(unlist(all_dimension_variances), na.rm = TRUE)
    
    # Add overall summary row
    results <- rbind(results, 
                     data.frame(
                       level_type = "overall",
                       category = "OVERALL",
                       subcategory = "TOTAL",
                       variance_explained = overall_avg,
                       stringsAsFactors = FALSE
                     ))
    
    log_message(paste("Overall, metadata explains", 
                      round(overall_avg, 2), "% of topic variance"), 
                "calculate_direct_variance")
  }
  
  ## --- Sort results -----------------------------------------------------------
  # Sort by category, then level_type, then variance_explained
  if (nrow(results) > 0) {
    # Create sort order for level_type
    level_order <- c("subcategory", "dimension", "category", "overall")
    results$level_order <- match(results$level_type, level_order)
    
    # Sort the results
    results <- results[order(results$category, results$level_order, -results$variance_explained), ]
    
    # Remove the temporary ordering column
    results$level_order <- NULL
  }
  
  ## --- Store detailed results in diagnostics ----------------------------------
  diagnostics$topic_level_data <- detailed_results
  
  ## --- Create result ----------------------------------------------------------
  log_message("Direct variance calculation complete", "calculate_direct_variance")
  
  # Return standardized result
  return(create_result(
    data = results,  # Main results table directly in data
    metadata = list(
      timestamp = start_time,
      processing_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      success = nrow(results) > 0
    ),
    diagnostics = diagnostics
  ))
}