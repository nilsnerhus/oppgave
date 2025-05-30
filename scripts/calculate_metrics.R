#' @title Calculate Dominance and Variance Metrics - Fixed Version
#' @description Calculates both dominance and variance metrics for any category groupings.
#'   Uses GLOBAL dominant topics for all variance calculations to properly test epistemicide hypothesis.
#'   All categories are processed uniformly, with category-level values as means of subcategories.
#'   
#' @param model Result from fit_model() containing STM model, metadata, and category_map
#' @param topics Result from name_topics() containing topic names and metadata
#' @param n Number of top topics to consider for each group (default: 3)
#' @param bootstrap Whether to calculate bootstrap confidence intervals (default: TRUE)
#' @param n_bootstrap Number of bootstrap iterations (default: 1000)
#' @param min_group_size Minimum group size to include in analysis (default: 8)
#'
#' @return A list containing dominance and variance metrics for all groups
calculate_metrics <- function(model, topics, n = 3, bootstrap = TRUE, 
                              n_bootstrap = 1000, min_group_size = 8) {
  
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    excluded_groups = list(),
    group_stats = list()
  )
  
  ## --- Input validation & component extraction --------------------------------
  log_message("Validating input data and extracting components", "calculate_metrics")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model) || 
      !all(c("model", "aligned_meta", "category_map") %in% names(model$data))) {
    error_msg <- "Model must contain 'data$model', 'data$aligned_meta', and 'data$category_map'"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "calculate_metrics", "ERROR")
    
    return(create_result(
      data = list(dominance = NULL, variance = NULL),
      metadata = list(timestamp = Sys.time(), success = FALSE),
      diagnostics = diagnostics
    ))
  }
  
  # Validate topics structure
  if (!is.list(topics) || !"data" %in% names(topics) || !"topics_table" %in% names(topics$data)) {
    error_msg <- "Topics must contain 'data$topics_table'"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "calculate_metrics", "ERROR")
    
    return(create_result(
      data = list(dominance = NULL, variance = NULL),
      metadata = list(timestamp = Sys.time(), success = FALSE),
      diagnostics = diagnostics
    ))
  }
  
  # Extract components
  theta <- model$data$model$theta  
  meta <- model$data$aligned_meta  
  category_map <- model$data$category_map
  topics_table <- topics$data$topics_table
  k <- ncol(theta)
  
  log_message(paste("Extracted components:", nrow(theta), "documents,", k, "topics,", 
                    length(category_map), "categories"), "calculate_metrics")
  
  ## --- Calculate GLOBAL dominant topics for variance analysis -----------------
  log_message("Calculating global dominant topics for variance analysis", "calculate_metrics")
  
  # Calculate global dominance to get the globally dominant topics
  all_docs <- 1:nrow(theta)
  global_dominance_result <- find_dominance(theta, all_docs, n, bootstrap = FALSE)
  
  if (is.null(global_dominance_result)) {
    error_msg <- "Failed to calculate global dominance for variance analysis"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "calculate_metrics", "ERROR")
    
    return(create_result(
      data = list(dominance = NULL, variance = NULL),
      metadata = list(timestamp = Sys.time(), success = FALSE),
      diagnostics = diagnostics
    ))
  }
  
  # Extract the globally dominant topic indices - USE THESE FOR ALL VARIANCE CALCULATIONS
  global_top_topics <- global_dominance_result$corpus_level$top_indices
  global_top_topic_names <- sapply(global_top_topics, function(idx) {
    topic_row <- which(topics_table$topic_id == idx)
    if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
  })
  
  log_message(paste("Global dominant topics for variance analysis:", 
                    paste(global_top_topic_names, collapse = ", ")), "calculate_metrics")
  
  ## --- Initialize results data frames -----------------------------------------
  log_message("Initializing results structures", "calculate_metrics")
  
  # Create empty results data frames
  dominance_results <- data.frame(
    category = character(), 
    subcategory = character(),
    documents = integer(),
    raw_dominance = numeric(),
    normalized_dominance = numeric(),
    variance = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    top_topics = character(),
    top_topic_ids = character(),
    stringsAsFactors = FALSE
  )
  
  variance_results <- data.frame(
    category = character(),
    subcategory = character(), 
    documents = integer(),
    variance_explained = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  ## --- Process Each Category --------------------------------------------------
  log_message("Processing category groupings from category_map", "calculate_metrics")
  
  # Loop through each category in category_map
  for (category_name in names(category_map)) {
    log_message(paste("Processing category:", category_name), "calculate_metrics")
    
    category_columns <- category_map[[category_name]]
    
    # Storage for subcategory values to calculate means
    subcategory_dominance_values <- numeric()
    subcategory_variance_values <- numeric()
    
    ## --- Handle Multi-column Categories (e.g., Geography) -------------------
    if (length(category_columns) > 1) {
      log_message(paste("Multi-column category detected:", category_name), "calculate_metrics")
      
      # Process each column as separate subcategory
      for (col_name in category_columns) {
        
        if (!col_name %in% names(meta)) {
          warning_msg <- paste("Column", col_name, "not found in metadata")
          diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
          log_message(warning_msg, "calculate_metrics", "WARNING")
          next
        }
        
        # Handle binary columns (assuming multi-column are binary)
        if (is.logical(meta[[col_name]])) {
          doc_indices <- which(meta[[col_name]] == TRUE)
          
          # Format subcategory name
          subcategory_name <- if (grepl("^is_", col_name)) {
            toupper(gsub("^is_", "", col_name))
          } else {
            col_name
          }
          
          # Skip if too few documents
          if (length(doc_indices) < min_group_size) {
            excluded_msg <- paste(subcategory_name, "(n=", length(doc_indices), ")")
            diagnostics$excluded_groups[[category_name]] <- c(diagnostics$excluded_groups[[category_name]], excluded_msg)
            log_message(paste("Excluding", excluded_msg, "- below minimum group size"), "calculate_metrics")
            next
          }
          
          # Calculate metrics for this subcategory
          dominance_result <- find_dominance(theta, doc_indices, n, bootstrap, n_bootstrap)
          
          if (!is.null(dominance_result)) {
            # Get top topic names and IDs (group-specific for dominance display)
            top_indices <- dominance_result$corpus_level$top_indices
            top_topics <- sapply(top_indices, function(idx) {
              topic_row <- which(topics_table$topic_id == idx)
              if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
            })
            
            # Add subcategory dominance results
            dominance_results <- rbind(dominance_results, data.frame(
              category = category_name,
              subcategory = subcategory_name,
              documents = length(doc_indices),
              raw_dominance = dominance_result$corpus_level$raw,
              normalized_dominance = dominance_result$corpus_level$normalized,
              variance = dominance_result$corpus_level$variance,
              ci_lower = dominance_result$corpus_level$ci_lower,
              ci_upper = dominance_result$corpus_level$ci_upper,
              top_topics = paste(top_topics, collapse = ", "),
              top_topic_ids = paste(top_indices, collapse = ","),
              stringsAsFactors = FALSE
            ))
            
            # FIXED: Calculate variance using GLOBAL topics instead of group-specific topics
            binary_indicator <- meta[[col_name]]
            variance_result <- find_variance(theta, 1:nrow(theta), binary_indicator, global_top_topics, bootstrap, n_bootstrap)
            
            if (!is.null(variance_result)) {
              # Add subcategory variance results
              variance_results <- rbind(variance_results, data.frame(
                category = category_name,
                subcategory = subcategory_name,
                documents = length(doc_indices),
                variance_explained = variance_result$raw,
                ci_lower = variance_result$ci_lower,
                ci_upper = variance_result$ci_upper,
                stringsAsFactors = FALSE
              ))
              
              # Store values for category average
              subcategory_dominance_values <- c(subcategory_dominance_values, dominance_result$corpus_level$normalized)
              subcategory_variance_values <- c(subcategory_variance_values, variance_result$raw)
            }
          }
        }
      }
      
      ## --- Handle Single-column Categories ------------------------------------
    } else {
      log_message(paste("Single-column category detected:", category_name), "calculate_metrics")
      
      col_name <- category_columns[1]
      
      if (!col_name %in% names(meta)) {
        warning_msg <- paste("Column", col_name, "not found in metadata")
        diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
        log_message(warning_msg, "calculate_metrics", "WARNING")
        next
      }
      
      # For ALL single-column categories, process subcategories first
      # Extract unique values/groups from the column
      unique_values <- unique(meta[[col_name]])
      unique_values <- unique_values[!is.na(unique_values)]
      
      log_message(paste("Found", length(unique_values), "unique values for", category_name, ":", paste(unique_values, collapse = ", ")), "calculate_metrics")
      
      # Process each subcategory
      for (value in unique_values) {
        doc_indices <- which(meta[[col_name]] == value)
        
        # Skip if too few documents
        if (length(doc_indices) < min_group_size) {
          excluded_msg <- paste(value, "(n=", length(doc_indices), ")")
          diagnostics$excluded_groups[[category_name]] <- c(diagnostics$excluded_groups[[category_name]], excluded_msg)
          log_message(paste("Excluding", excluded_msg, "- below minimum group size"), "calculate_metrics")
          next
        }
        
        # Calculate subcategory dominance
        subcategory_dominance_result <- find_dominance(theta, doc_indices, n, bootstrap, n_bootstrap)
        
        if (!is.null(subcategory_dominance_result)) {
          # Get subcategory top topics (group-specific for dominance display)
          subcategory_top_indices <- subcategory_dominance_result$corpus_level$top_indices
          subcategory_top_topics <- sapply(subcategory_top_indices, function(idx) {
            topic_row <- which(topics_table$topic_id == idx)
            if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
          })
          
          # Add subcategory dominance results
          dominance_results <- rbind(dominance_results, data.frame(
            category = category_name,
            subcategory = as.character(value),
            documents = length(doc_indices),
            raw_dominance = subcategory_dominance_result$corpus_level$raw,
            normalized_dominance = subcategory_dominance_result$corpus_level$normalized,
            variance = subcategory_dominance_result$corpus_level$variance,
            ci_lower = subcategory_dominance_result$corpus_level$ci_lower,
            ci_upper = subcategory_dominance_result$corpus_level$ci_upper,
            top_topics = paste(subcategory_top_topics, collapse = ", "),
            top_topic_ids = paste(subcategory_top_indices, collapse = ","),
            stringsAsFactors = FALSE
          ))
          
          # FIXED: Calculate variance using GLOBAL topics instead of group-specific topics
          binary_indicator <- meta[[col_name]] == value
          subcategory_variance_result <- find_variance(theta, 1:nrow(theta), binary_indicator, global_top_topics, bootstrap, n_bootstrap)
          
          if (!is.null(subcategory_variance_result)) {
            # Add subcategory variance results
            variance_results <- rbind(variance_results, data.frame(
              category = category_name,
              subcategory = as.character(value),
              documents = length(doc_indices),
              variance_explained = subcategory_variance_result$raw,
              ci_lower = subcategory_variance_result$ci_lower,
              ci_upper = subcategory_variance_result$ci_upper,
              stringsAsFactors = FALSE
            ))
            
            # Store values for category average
            subcategory_dominance_values <- c(subcategory_dominance_values, subcategory_dominance_result$corpus_level$normalized)
            subcategory_variance_values <- c(subcategory_variance_values, subcategory_variance_result$raw)
          }
        }
      }
    }
    
    ## --- Calculate Category-Level Averages ----------------------------------
    log_message(paste("Category", category_name, "has", length(subcategory_dominance_values), "valid subcategories for averaging"), "calculate_metrics")
    
    if (length(subcategory_dominance_values) > 0) {
      # Category values = mean of subcategory values
      category_dominance_avg <- mean(subcategory_dominance_values)
      category_variance_avg <- mean(subcategory_variance_values)
      
      # Add category-level results (using averages)
      dominance_results <- rbind(dominance_results, data.frame(
        category = category_name,
        subcategory = "Overall",
        documents = nrow(theta),  # All documents conceptually
        raw_dominance = category_dominance_avg,
        normalized_dominance = category_dominance_avg,
        variance = 0,  # No bootstrap variance for averaged values
        ci_lower = category_dominance_avg,
        ci_upper = category_dominance_avg,
        top_topics = "Average of subcategories",
        top_topic_ids = "",
        stringsAsFactors = FALSE
      ))
      
      variance_results <- rbind(variance_results, data.frame(
        category = category_name,
        subcategory = "Overall",
        documents = nrow(theta),
        variance_explained = category_variance_avg,
        ci_lower = category_variance_avg,
        ci_upper = category_variance_avg,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  ## --- Finalize Results -------------------------------------------------------
  log_message("Finalizing results", "calculate_metrics")
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    n_value = n,
    bootstrap = bootstrap,
    n_bootstrap = n_bootstrap,
    min_group_size = min_group_size,
    global_topics_used = paste(global_top_topic_names, collapse = ", "),
    success = TRUE
  )
  
  # Return standardized result
  return(create_result(
    data = list(
      dominance = dominance_results,
      variance = variance_results
    ),
    metadata = metadata,
    diagnostics = diagnostics
  ))
}