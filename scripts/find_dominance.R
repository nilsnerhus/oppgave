#' @title Calculate dominance across all dimensions
#' @description Calculates discourse dominance (topic concentration) across documents
#'   and analyzes how it varies by different dimensions like income level, region,
#'   or geographic characteristics. Identifies top topics for each category.
#'   
#' @param model Result from fit_model() containing topic proportions and model data
#' @param n Number of top topics to include in dominance calculation (default: 3)
#' @param dominance_config Configuration options for dominance analysis:
#'   \itemize{
#'     \item dimensions - Which dimensions to analyze (default: all from category_map)
#'     \item include_top_topics - Whether to identify top topics (default: TRUE)
#'     \item min_docs - Minimum documents for a subcategory (default: 3)
#'   }
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item document_dominance - Document-level dominance values
#'       \item dimension_dominance - Dominance by category dimensions
#'       \item top_topics - Most dominant topics by category and subcategory
#'     }
#'   }
#'   \item{metadata}{Processing information and configuration}
#'   \item{diagnostics}{Processing statistics and issues}
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' dominance <- find_dominance(model)
#' 
#' # Use more topics in dominance calculation
#' dominance <- find_dominance(model, n = 5)
#' 
#' # Focus on specific dimensions only
#' dominance_config <- list(
#'   dimensions = c("Income", "Region")
#' )
#' dominance <- find_dominance(model, n = 3, dominance_config = dominance_config)
#' }
find_dominance <- function(
    model,
    n = 3,
    dominance_config = list(
      dimensions = NULL,       # Default: all from category_map
      include_top_topics = TRUE,
      min_docs = 3
    )
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Set defaults for any missing config options
  dominance_config$dimensions <- dominance_config$dimensions %||% NULL
  dominance_config$include_top_topics <- dominance_config$include_top_topics %||% TRUE
  dominance_config$min_docs <- dominance_config$min_docs %||% 3
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    calculation_issues = character(),
    skipped_categories = character(),
    processing_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "find_dominance")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model)) {
    error_msg <- "model must be the output from fit_model() with a 'data' component"
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, error_msg)
    log_message(error_msg, "find_dominance", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check required components
  required_components <- c("topic_proportions", "topic_data")
  missing_components <- setdiff(required_components, names(model$data))
  if (length(missing_components) > 0) {
    error_msg <- paste("Model missing required components:", paste(missing_components, collapse = ", "))
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, error_msg)
    log_message(error_msg, "find_dominance", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate n value
  if (!is.numeric(n) || n < 1) {
    error_msg <- "n must be a positive integer"
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, error_msg)
    log_message(error_msg, "find_dominance", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Check that category_map is available
  if (!"category_map" %in% names(model$metadata)) {
    warning_msg <- "No category_map found in model metadata; using fallback dimensions"
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, warning_msg)
    log_message(warning_msg, "find_dominance", "WARNING")
    
    # Create basic category map from available columns
    topic_props <- model$data$topic_proportions
    available_cols <- names(topic_props)
    
    category_map <- list()
    if ("wb_income_level" %in% available_cols) {
      category_map$Income <- "wb_income_level"
    }
    if ("region" %in% available_cols) {
      category_map$Region <- "region"
    }
    geo_cols <- c("is_sids", "is_lldc")[c("is_sids", "is_lldc") %in% available_cols]
    if (length(geo_cols) > 0) {
      category_map$Geography <- geo_cols
    }
  } else {
    category_map <- model$metadata$category_map
  }
  
  ## --- Extract topic data and metadata ----------------------------------------
  log_message("Extracting topic data", "find_dominance")
  
  # Get topic proportions (wide format)
  topic_props <- model$data$topic_proportions
  
  # Get topic data (long format)
  topic_data <- model$data$topic_data
  
  # Determine dimensions to analyze
  if (is.null(dominance_config$dimensions)) {
    dimensions_to_analyze <- names(category_map)
  } else {
    dimensions_to_analyze <- intersect(dominance_config$dimensions, names(category_map))
    
    if (length(dimensions_to_analyze) == 0) {
      warning_msg <- "None of the specified dimensions found in category_map; using all dimensions"
      diagnostics$calculation_issues <- c(diagnostics$calculation_issues, warning_msg)
      log_message(warning_msg, "find_dominance", "WARNING")
      dimensions_to_analyze <- names(category_map)
    }
  }
  
  log_message(paste("Analyzing dimensions:", paste(dimensions_to_analyze, collapse = ", ")), 
              "find_dominance")
  
  ## --- Calculate document-level dominance -------------------------------------
  log_message("Calculating document-level dominance", "find_dominance")
  
  # Get unique document IDs
  doc_ids <- unique(topic_props$doc_id)
  doc_count <- length(doc_ids)
  
  # Initialize document dominance dataframe
  document_dominance <- data.frame(
    doc_id = doc_ids,
    dominance = numeric(doc_count),
    stringsAsFactors = FALSE
  )
  
  # Calculate dominance for each document
  for (i in 1:doc_count) {
    document_dominance$dominance[i] <- tryCatch({
      # Filter for this document
      doc_topics <- topic_data[topic_data$doc_id == doc_ids[i], ]
      
      # Sort by proportion
      doc_topics <- doc_topics[order(doc_topics$Proportion, decreasing = TRUE), ]
      
      # Calculate dominance (sum of top n proportions)
      top_n_sum <- sum(doc_topics$Proportion[1:min(n, nrow(doc_topics))])
      
      # Return as proportion of total
      top_n_sum / sum(doc_topics$Proportion)
    }, error = function(e) {
      warning_msg <- paste("Error calculating dominance for document", doc_ids[i], ":", e$message)
      diagnostics$calculation_issues <<- c(diagnostics$calculation_issues, warning_msg)
      log_message(warning_msg, "find_dominance", "WARNING")
      NA
    })
  }
  
  # Join with metadata from topic_props
  metadata_cols <- setdiff(names(topic_props), c(grep("^Topic_", names(topic_props), value = TRUE)))
  document_dominance <- dplyr::left_join(
    document_dominance,
    topic_props[, metadata_cols],
    by = "doc_id"
  )
  
  ## --- Calculate overall dominance --------------------------------------------
  log_message("Calculating overall dominance", "find_dominance")
  
  # Calculate mean and standard deviation
  overall_mean <- mean(document_dominance$dominance, na.rm = TRUE)
  overall_sd <- stats::sd(document_dominance$dominance, na.rm = TRUE)
  
  # Calculate confidence interval
  ci_width <- 1.96 * overall_sd / sqrt(sum(!is.na(document_dominance$dominance)))
  
  # Initialize dimension dominance table
  dimension_dominance <- data.frame(
    category = "Overall",
    level = "Category",
    docs = sum(!is.na(document_dominance$dominance)),
    dominance = round(overall_mean, 3),
    std_dev = round(overall_sd, 3),
    ci_lower = round(overall_mean - ci_width, 3),
    ci_upper = round(overall_mean + ci_width, 3),
    top_topics = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Add top topics if requested
  if (dominance_config$include_top_topics) {
    # Inline the identify_top_topics functionality
    # Calculate average proportion for each topic
    topic_averages <- stats::aggregate(
      Proportion ~ Topic, 
      data = topic_data, 
      FUN = mean
    )
    
    # Sort by proportion and get top n
    topic_averages <- topic_averages[order(topic_averages$Proportion, decreasing = TRUE), ]
    top_n_topics <- topic_averages[1:min(n, nrow(topic_averages)), ]
    
    # Inline the format_top_topics functionality
    if (nrow(top_n_topics) > 0) {
      # Extract topic numbers
      topic_numbers <- gsub("Topic_", "", top_n_topics$Topic)
      
      # Format as string: "Topic X (XX.X%), Topic Y (YY.Y%), ..."
      formatted_topics <- paste(
        paste0("Topic ", topic_numbers, " (", 
               round(top_n_topics$Proportion * 100, 1), "%)"),
        collapse = ", "
      )
      
      dimension_dominance$top_topics[1] <- formatted_topics
    }
  }
  
  ## --- Calculate dimension-level dominance ------------------------------------
  log_message("Calculating dimension-level dominance", "find_dominance")
  
  # Process each dimension
  for (category_name in dimensions_to_analyze) {
    log_message(paste("Processing", category_name, "dimension"), "find_dominance")
    
    # Get dimensions for this category
    category_dimensions <- category_map[[category_name]]
    
    # Skip empty categories
    if (length(category_dimensions) == 0) {
      skip_msg <- paste("Skipping empty category:", category_name)
      diagnostics$skipped_categories <- c(diagnostics$skipped_categories, skip_msg)
      log_message(skip_msg, "find_dominance", "WARNING")
      next
    }
    
    # Add category row (same as overall)
    dimension_dominance <- rbind(dimension_dominance, data.frame(
      category = category_name,
      level = "Category",
      docs = sum(!is.na(document_dominance$dominance)),
      dominance = round(overall_mean, 3),
      std_dev = round(overall_sd, 3),
      ci_lower = round(overall_mean - ci_width, 3),
      ci_upper = round(overall_mean + ci_width, 3),
      top_topics = if(dominance_config$include_top_topics) 
        dimension_dominance$top_topics[1] else NA_character_,
      stringsAsFactors = FALSE
    ))
    
    # Process dimensions differently based on category type
    if (category_name %in% c("Income", "Region", "Time")) {
      # For categorical dimensions (single column)
      dim <- category_dimensions[1]  # Should be only one dimension
      
      # Skip if dimension not in data
      if (!dim %in% names(document_dominance)) {
        skip_msg <- paste("Dimension not found in data:", dim)
        diagnostics$skipped_categories <- c(diagnostics$skipped_categories, skip_msg)
        log_message(skip_msg, "find_dominance", "WARNING")
        next
      }
      
      # Get unique values for this dimension
      values <- unique(document_dominance[[dim]])
      values <- values[!is.na(values)]
      
      # Process each value
      for (val in values) {
        # Filter data
        group_data <- document_dominance[document_dominance[[dim]] == val, ]
        
        # Skip if too few documents
        if (nrow(group_data) < dominance_config$min_docs) {
          skip_msg <- paste("Skipping", val, "in", category_name, 
                            "(only", nrow(group_data), "documents)")
          diagnostics$skipped_categories <- c(diagnostics$skipped_categories, skip_msg)
          log_message(skip_msg, "find_dominance", "WARNING")
          next
        }
        
        # Calculate stats
        group_mean <- mean(group_data$dominance, na.rm = TRUE)
        group_sd <- stats::sd(group_data$dominance, na.rm = TRUE)
        group_ci_width <- 1.96 * group_sd / sqrt(sum(!is.na(group_data$dominance)))
        
        # Format for top topics
        top_topics_str <- NA_character_
        
        # Calculate top topics if requested (inline)
        if (dominance_config$include_top_topics) {
          # Filter topic data for this group
          group_topic_data <- topic_data[topic_data$doc_id %in% group_data$doc_id, ]
          
          # Calculate average proportion for each topic
          group_topic_avgs <- stats::aggregate(
            Proportion ~ Topic, 
            data = group_topic_data, 
            FUN = mean
          )
          
          # Sort by proportion and get top n
          group_topic_avgs <- group_topic_avgs[order(group_topic_avgs$Proportion, decreasing = TRUE), ]
          group_top_n <- group_topic_avgs[1:min(n, nrow(group_topic_avgs)), ]
          
          # Format as string (inline)
          if (nrow(group_top_n) > 0) {
            # Extract topic numbers
            topic_numbers <- gsub("Topic_", "", group_top_n$Topic)
            
            # Format as string
            top_topics_str <- paste(
              paste0("Topic ", topic_numbers, " (", 
                     round(group_top_n$Proportion * 100, 1), "%)"),
              collapse = ", "
            )
          }
        }
        
        # Add to results
        dimension_dominance <- rbind(dimension_dominance, data.frame(
          category = paste0("- ", val),
          level = "Sub-category",
          docs = sum(!is.na(group_data$dominance)),
          dominance = round(group_mean, 3),
          std_dev = round(group_sd, 3),
          ci_lower = round(group_mean - group_ci_width, 3),
          ci_upper = round(group_mean + group_ci_width, 3),
          top_topics = top_topics_str,
          stringsAsFactors = FALSE
        ))
      }
    } else if (category_name == "Geography") {
      # For binary dimensions (multiple columns)
      for (dim in category_dimensions) {
        # Skip if dimension not in data
        if (!dim %in% names(document_dominance)) {
          skip_msg <- paste("Dimension not found in data:", dim)
          diagnostics$skipped_categories <- c(diagnostics$skipped_categories, skip_msg)
          log_message(skip_msg, "find_dominance", "WARNING")
          next
        }
        
        # Ensure binary format
        if (!is.logical(document_dominance[[dim]])) {
          if (is.character(document_dominance[[dim]]) || is.factor(document_dominance[[dim]])) {
            document_dominance[[dim]] <- tolower(as.character(document_dominance[[dim]])) %in% 
              c("true", "yes", "1", "t")
          } else if (is.numeric(document_dominance[[dim]])) {
            document_dominance[[dim]] <- document_dominance[[dim]] > 0
          }
        }
        
        # Filter data for TRUE values
        group_data <- document_dominance[document_dominance[[dim]] == TRUE, ]
        
        # Skip if too few documents
        if (nrow(group_data) < dominance_config$min_docs) {
          skip_msg <- paste("Skipping", dim, "in", category_name, 
                            "(only", nrow(group_data), "documents)")
          diagnostics$skipped_categories <- c(diagnostics$skipped_categories, skip_msg)
          log_message(skip_msg, "find_dominance", "WARNING")
          next
        }
        
        # Calculate stats
        group_mean <- mean(group_data$dominance, na.rm = TRUE)
        group_sd <- stats::sd(group_data$dominance, na.rm = TRUE)
        group_ci_width <- 1.96 * group_sd / sqrt(sum(!is.na(group_data$dominance)))
        
        # Format for top topics
        top_topics_str <- NA_character_
        
        # Calculate top topics if requested (inline)
        if (dominance_config$include_top_topics) {
          # Filter topic data for this group
          group_topic_data <- topic_data[topic_data$doc_id %in% group_data$doc_id, ]
          
          # Calculate average proportion for each topic
          group_topic_avgs <- stats::aggregate(
            Proportion ~ Topic, 
            data = group_topic_data, 
            FUN = mean
          )
          
          # Sort by proportion and get top n
          group_topic_avgs <- group_topic_avgs[order(group_topic_avgs$Proportion, decreasing = TRUE), ]
          group_top_n <- group_topic_avgs[1:min(n, nrow(group_topic_avgs)), ]
          
          # Format as string (inline)
          if (nrow(group_top_n) > 0) {
            # Extract topic numbers
            topic_numbers <- gsub("Topic_", "", group_top_n$Topic)
            
            # Format as string
            top_topics_str <- paste(
              paste0("Topic ", topic_numbers, " (", 
                     round(group_top_n$Proportion * 100, 1), "%)"),
              collapse = ", "
            )
          }
        }
        
        # Format subcategory name based on dimension
        subcategory_display <- switch(dim,
                                      "is_sids" = "SIDS",
                                      "is_lldc" = "LLDC",
                                      gsub("is_", "", dim)  # Default formatting
        )
        
        # Add to results
        dimension_dominance <- rbind(dimension_dominance, data.frame(
          category = paste0("- ", subcategory_display),
          level = "Sub-category",
          docs = sum(!is.na(group_data$dominance)),
          dominance = round(group_mean, 3),
          std_dev = round(group_sd, 3),
          ci_lower = round(group_mean - group_ci_width, 3),
          ci_upper = round(group_mean + group_ci_width, 3),
          top_topics = top_topics_str,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    document_dominance = document_dominance,
    dimension_dominance = dimension_dominance
  )
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    n_value = n,
    total_documents = doc_count,
    dimensions_analyzed = dimensions_to_analyze,
    dominance_config = dominance_config
  )
  
  # Update diagnostics
  diagnostics$processing_stats <- list(
    doc_count = doc_count,
    dimensions_count = length(dimensions_to_analyze),
    subcategories_count = nrow(dimension_dominance) - 1 - length(dimensions_to_analyze),
    skipped_categories_count = length(diagnostics$skipped_categories)
  )
  
  log_message(paste("Dominance analysis complete for", doc_count, "documents,", 
                    length(dimensions_to_analyze), "dimensions"), 
              "find_dominance")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}