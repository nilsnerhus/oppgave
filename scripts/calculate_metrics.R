#' @title Calculate Dominance Metrics with Statistical Significance Testing
#' @description Calculates dominance patterns for category groupings and tests statistical 
#'   significance using STM's estimateEffect. Focuses on methodologically sound analysis.
#'   
#' @param model Result from fit_model() containing STM model, metadata, and category_map
#' @param topics Result from name_topics() containing topic names and metadata
#' @param dfm Original dfm object used for fitting (contains correct metadata for significance testing)
#' @param n Number of top topics to consider for each group (default: 3)
#' @param min_group_size Minimum group size to include in analysis (default: 8)
#'
#' @return A list containing dominance metrics with statistical significance for all groups
calculate_metrics <- function(model, topics, dfm, n = 3, min_group_size = 8) {
  
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    excluded_groups = list(),
    significance_tests = list()
  )
  
  ## --- Input validation & component extraction --------------------------------
  log_message("Validating input data and extracting components", "calculate_metrics")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model) || 
      !all(c("model", "topic_proportions", "aligned_meta", "category_map") %in% names(model$data))) {
    error_msg <- "Model must contain 'data$model', 'data$topic_proportions', 'data$aligned_meta', and 'data$category_map'"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "calculate_metrics", "ERROR")
    stop(error_msg)
  }
  
  # Validate topics structure
  if (!is.list(topics) || !"data" %in% names(topics) || !"topics_table" %in% names(topics$data)) {
    error_msg <- "Topics must contain 'data$topics_table'"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "calculate_metrics", "ERROR")
    stop(error_msg)
  }
  
  # Validate dfm structure for significance testing
  if (!is.list(dfm) || !"data" %in% names(dfm) || !"meta" %in% names(dfm$data)) {
    error_msg <- "DFM must contain 'data$meta' for significance testing"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "calculate_metrics", "ERROR")
    stop(error_msg)
  }
  
  # Extract components
  theta <- model$data$topic_proportions
  meta <- model$data$aligned_meta  # For dominance calculation
  stm_meta <- dfm$data$meta       # For significance testing (original structure)
  stm_model <- model$data$model   # For estimateEffect
  category_map <- model$data$category_map
  topics_table <- topics$data$topics_table
  k <- ncol(theta)
  
  log_message(paste("Extracted components:", nrow(theta), "documents,", k, "topics,", 
                    length(category_map), "categories"), "calculate_metrics")
  
  ## --- Initialize results data frame ------------------------------------------
  log_message("Initializing results structures", "calculate_metrics")
  
  # Create clean results data frame  
  results <- data.frame(
    category = character(), 
    subcategory = character(),
    documents = integer(),
    dominance = numeric(),
    top_topics = character(),
    top_topic_ids = character(),
    p_value = numeric(),
    significant = logical(),
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
          
          # Calculate dominance for this subcategory (no bootstrap)
          dominance_result <- find_dominance(theta, doc_indices, n, bootstrap = FALSE)
          
          if (!is.null(dominance_result)) {
            # Get top topic names and IDs
            top_indices <- dominance_result$corpus_level$top_indices
            top_topics <- get_topic_names(top_indices, topics_table)
            
            # Test statistical significance using find_variance
            significance_result <- find_variance(
              stm_model = stm_model,
              stm_meta = stm_meta,
              col_name = col_name,
              col_value = TRUE,
              top_topics = top_indices
            )
            
            # Add subcategory results
            results <- rbind(results, data.frame(
              category = category_name,
              subcategory = subcategory_name,
              documents = length(doc_indices),
              dominance = dominance_result$corpus_level$normalized,
              top_topics = paste(top_topics, collapse = ", "),
              top_topic_ids = paste(top_indices, collapse = ","),
              p_value = significance_result$p_value,
              significant = significance_result$significant,
              stringsAsFactors = FALSE
            ))
            
            # Store values for category average
            subcategory_dominance_values <- c(subcategory_dominance_values, dominance_result$corpus_level$normalized)
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
      
      # Process subcategories
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
        
        # Calculate subcategory dominance (no bootstrap)
        subcategory_dominance_result <- find_dominance(theta, doc_indices, n, bootstrap = FALSE)
        
        if (!is.null(subcategory_dominance_result)) {
          # Get subcategory top topics
          subcategory_top_indices <- subcategory_dominance_result$corpus_level$top_indices
          subcategory_top_topics <- get_topic_names(subcategory_top_indices, topics_table)
          
          # Test statistical significance using find_variance
          significance_result <- find_variance(
            stm_model = stm_model,
            stm_meta = stm_meta,
            col_name = col_name,
            col_value = value,
            top_topics = subcategory_top_indices
          )
          
          # Add subcategory results
          results <- rbind(results, data.frame(
            category = category_name,
            subcategory = as.character(value),
            documents = length(doc_indices),
            dominance = subcategory_dominance_result$corpus_level$normalized,
            top_topics = paste(subcategory_top_topics, collapse = ", "),
            top_topic_ids = paste(subcategory_top_indices, collapse = ","),
            p_value = significance_result$p_value,
            significant = significance_result$significant,
            stringsAsFactors = FALSE
          ))
          
          # Store values for category average
          subcategory_dominance_values <- c(subcategory_dominance_values, subcategory_dominance_result$corpus_level$normalized)
        }
      }
    }
    
    ## --- Calculate Category-Level Averages ----------------------------------
    if (length(subcategory_dominance_values) > 0) {
      category_dominance_avg <- mean(subcategory_dominance_values)
      
      # Add category-level results (using averages)
      results <- rbind(results, data.frame(
        category = category_name,
        subcategory = "Overall",
        documents = nrow(theta),
        dominance = category_dominance_avg,
        top_topics = "Average of subcategories",
        top_topic_ids = "",
        p_value = NA_real_,
        significant = NA,
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
    min_group_size = min_group_size,
    significance_tests_run = sum(!is.na(results$p_value)),
    significant_results = sum(results$significant, na.rm = TRUE),
    method = "Dominance calculation with STM significance testing",
    success = TRUE
  )
  
  # Return standardized result
  return(create_result(
    data = results,
    metadata = metadata,
    diagnostics = diagnostics
  ))
}

## --- Helper Functions -------------------------------------------------------

#' Get topic names from topic IDs
get_topic_names <- function(topic_ids, topics_table) {
  sapply(topic_ids, function(idx) {
    topic_row <- which(topics_table$topic_id == idx)
    if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
  })
}