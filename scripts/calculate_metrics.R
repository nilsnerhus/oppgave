#' @title Calculate Dominance and Variance Metrics with Bootstrap Confidence Intervals
#' @description Generalizable function that calculates both dominance and variance metrics
#'   for any category groupings defined in the model. Each group gets analyzed on its own
#'   most prevalent topics. Categories with multiple columns (like Geography) are handled
#'   as averages of their subcategories.
#'   
#' @param model Result from fit_model() containing STM model, metadata, and category_map
#' @param topics Result from name_topics() containing topic names and metadata
#' @param n Number of top topics to consider for each group (default: 3)
#' @param bootstrap Whether to calculate bootstrap confidence intervals (default: TRUE)
#' @param n_bootstrap Number of bootstrap iterations (default: 1000)
#' @param min_group_size Minimum group size to include in analysis (default: 8)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item dominance - Data frame with dominance metrics for all groups
#'       \item variance - Data frame with variance metrics for all groups
#'     }
#'   }
#'   \item{metadata}{Processing information and parameters}
#'   \item{diagnostics}{Issues encountered and group statistics}
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
  
  # Add Overall category to category_map and meta
  category_map[["Overall"]] <- "overall_category"
  meta$overall_category <- "Overall"
  
  log_message(paste("Extracted components:", nrow(theta), "documents,", k, "topics,", 
                    length(category_map), "categories"), "calculate_metrics")
  
  ## --- Initialize results data frames -----------------------------------------
  log_message("Initializing results structures", "calculate_metrics")
  
  # Create empty dominance results data frame
  dominance_results <- data.frame(
    level_type = character(),
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
  
  # Create empty variance results data frame  
  variance_results <- data.frame(
    level_type = character(),
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
  
  # Loop through each category in category_map (including Overall)
  for (category_name in names(category_map)) {
    log_message(paste("Processing category:", category_name), "calculate_metrics")
    
    category_columns <- category_map[[category_name]]
    
    ## --- Determine processing approach based on column count ----------------
    if (length(category_columns) > 1) {
      # Multi-column category (e.g., Geography: c("is_sids", "is_lldc"))
      # Process subcategories individually, then average for category level
      log_message(paste("Multi-column category detected:", category_name), "calculate_metrics")
      
      subcategory_dominance_values <- numeric()
      subcategory_variance_values <- numeric()
      
      # Process each column as separate subcategory
      for (col_name in category_columns) {
        
        ## --- Extract Groups for This Column ---------------------------
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
          
          ## --- Calculate Metrics for This Subcategory -----------------
          # Calculate dominance
          dominance_result <- find_dominance(theta, doc_indices, n, bootstrap, n_bootstrap)
          
          if (!is.null(dominance_result)) {
            # Get top topic names and IDs
            top_indices <- dominance_result$corpus_level$top_indices
            top_topics <- sapply(top_indices, function(idx) {
              topic_row <- which(topics_table$topic_id == idx)
              if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
            })
            
            # Add dominance results (corpus level only)
            dominance_results <- rbind(dominance_results, data.frame(
              level_type = "corpus",
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
            
            # Calculate variance using binary indicator across all documents
            binary_indicator <- meta[[col_name]]
            variance_result <- find_variance(theta, 1:nrow(theta), binary_indicator, top_indices, bootstrap, n_bootstrap)
            
            if (!is.null(variance_result)) {
              # Add variance results (corpus level only)
              variance_results <- rbind(variance_results, data.frame(
                level_type = "corpus",
                category = category_name,
                subcategory = subcategory_name,
                documents = length(doc_indices),
                variance_explained = variance_result$raw,
                ci_lower = variance_result$ci_lower,
                ci_upper = variance_result$ci_upper,
                stringsAsFactors = FALSE
              ))
              
              # Store values for category average (use corpus level)
              subcategory_dominance_values <- c(subcategory_dominance_values, dominance_result$corpus_level$normalized)
              subcategory_variance_values <- c(subcategory_variance_values, variance_result$raw)
            }
          }
        }
      }
      
      ## --- Calculate Category-Level Averages ----------------------------
      if (length(subcategory_dominance_values) > 0) {
        # Category dominance = mean of subcategory dominance values
        category_dominance_avg <- mean(subcategory_dominance_values)
        category_variance_avg <- mean(subcategory_variance_values)
        
        # Add category-level results (corpus level only, using averages)
        dominance_results <- rbind(dominance_results, data.frame(
          level_type = "corpus",
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
          level_type = "corpus",
          category = category_name,
          subcategory = "Overall",
          documents = nrow(theta),
          variance_explained = category_variance_avg,
          ci_lower = category_variance_avg,
          ci_upper = category_variance_avg,
          stringsAsFactors = FALSE
        ))
      }
      
    } else {
      # Single-column category (e.g., Income: "wb_income_level", Overall: "overall_category")
      # Standard approach: category uses all docs, subcategories use subsets
      log_message(paste("Single-column category detected:", category_name), "calculate_metrics")
      
      col_name <- category_columns[1]
      
      if (!col_name %in% names(meta)) {
        warning_msg <- paste("Column", col_name, "not found in metadata")
        diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
        log_message(warning_msg, "calculate_metrics", "WARNING")
        next
      }
      
      ## --- Calculate Category-Level Metrics --------------------
      # All documents in this category
      all_docs <- 1:nrow(theta)
      
      # Calculate category dominance
      category_dominance_result <- find_dominance(theta, all_docs, n, bootstrap, n_bootstrap)
      
      if (!is.null(category_dominance_result)) {
        # Get category top topics
        category_top_indices <- category_dominance_result$corpus_level$top_indices
        category_top_topics <- sapply(category_top_indices, function(idx) {
          topic_row <- which(topics_table$topic_id == idx)
          if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
        })
        
        # Calculate category variance
        category_var <- meta[[col_name]]
        category_variance_result <- find_variance(theta, all_docs, category_var, category_top_indices, bootstrap, n_bootstrap)
        
        if (!is.null(category_variance_result)) {
          # Add category-level results (corpus level only)
          dominance_results <- rbind(dominance_results, data.frame(
            level_type = "corpus",
            category = category_name,
            subcategory = "Overall",
            documents = length(all_docs),
            raw_dominance = category_dominance_result$corpus_level$raw,
            normalized_dominance = category_dominance_result$corpus_level$normalized,
            variance = category_dominance_result$corpus_level$variance,
            ci_lower = category_dominance_result$corpus_level$ci_lower,
            ci_upper = category_dominance_result$corpus_level$ci_upper,
            top_topics = paste(category_top_topics, collapse = ", "),
            top_topic_ids = paste(category_top_indices, collapse = ","),
            stringsAsFactors = FALSE
          ))
          
          variance_results <- rbind(variance_results, data.frame(
            level_type = "corpus",
            category = category_name,
            subcategory = "Overall",
            documents = length(all_docs),
            variance_explained = category_variance_result$raw,
            ci_lower = category_variance_result$ci_lower,
            ci_upper = category_variance_result$ci_upper,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      ## --- Calculate Subcategory-Level Metrics -----------------
      # Skip subcategories for Overall (it's just one group)
      if (category_name == "Overall") {
        next
      }
      
      # Extract unique values/groups from the column
      unique_values <- unique(meta[[col_name]])
      unique_values <- unique_values[!is.na(unique_values)]
      
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
          # Get subcategory top topics
          subcategory_top_indices <- subcategory_dominance_result$corpus_level$top_indices
          subcategory_top_topics <- sapply(subcategory_top_indices, function(idx) {
            topic_row <- which(topics_table$topic_id == idx)
            if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
          })
          
          # Calculate subcategory variance using binary indicator across all documents
          binary_indicator <- meta[[col_name]] == value
          subcategory_variance_result <- find_variance(theta, 1:nrow(theta), binary_indicator, subcategory_top_indices, bootstrap, n_bootstrap)
          
          if (!is.null(subcategory_variance_result)) {
            # Add subcategory results (corpus level only)
            dominance_results <- rbind(dominance_results, data.frame(
              level_type = "corpus",
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
            
            variance_results <- rbind(variance_results, data.frame(
              level_type = "corpus",
              category = category_name,
              subcategory = as.character(value),
              documents = length(doc_indices),
              variance_explained = subcategory_variance_result$raw,
              ci_lower = subcategory_variance_result$ci_lower,
              ci_upper = subcategory_variance_result$ci_upper,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  ## --- Finalize Results -------------------------------------------------------
  log_message("Finalizing results", "calculate_metrics")
  
  # Remove level_type column (not needed)
  dominance_results$level_type <- NULL
  variance_results$level_type <- NULL
  
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