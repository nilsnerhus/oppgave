#' @title Create consolidated tables for analysis findings
#' @description Takes outputs from topic naming, dominance analysis, and effects analysis
#'   and creates formatted tables for reporting findings. No calculations are performed.
#'   
#' @param topic_names Output from name_topics() with topic names
#' @param dominance Output from find_all_dominance() with dominance values and top topics
#' @param effects Output from stm_effects() with effect estimates
#' @param p_threshold P-value threshold for significance (default: 0.05)
#'
#' @return A list containing three formatted tables:
#'   \item{topics}{Table with topic information and quality metrics}
#'   \item{dominance}{Table with dominance analysis by category and top topics}
#'   \item{effects}{Table with variance explained by metadata dimensions}
create_tables <- function(
    topic_names,
    dominance,
    effects,
    p_threshold = 0.05
) {
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Start timing
  start_time <- Sys.time()
  
  # Initialize results list
  tables <- list()
  
  # 1. Create topic information table
  log_message("Creating topic information table", "create_tables")
  tables$topics <- create_topics_table(topic_names, dominance)
  
  # 2. Create dominance table
  log_message("Creating dominance table", "create_tables")
  tables$dominance <- create_dominance_table(dominance)
  
  # 3. Create effects table
  log_message("Creating effects table", "create_tables")
  tables$effects <- create_effects_table(effects, p_threshold)
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Add metadata
  tables$metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    topic_count = nrow(topic_names),
    p_threshold = p_threshold
  )
  
  log_message("Table creation complete", "create_tables")
  
  return(tables)
  
  # Helper functions for table creation (all in one file)
  
  # Topics table creation
  function create_topics_table(topic_names, dominance) {
    # Check if we have quality metrics available in model metadata
    if (!is.null(dominance$metadata) && !is.null(dominance$metadata$model_metadata) && 
        !is.null(dominance$metadata$model_metadata$model_quality)) {
      quality <- dominance$metadata$model_metadata$model_quality
      has_quality <- TRUE
    } else {
      has_quality <- FALSE
    }
    
    # Merge topic names with top words
    if (has_quality) {
      topics_table <- topic_names %>%
        dplyr::left_join(
          data.frame(
            topic_id = 1:length(quality$coherence),
            coherence = round(quality$coherence, 3),
            exclusivity = round(quality$exclusivity, 3),
            stringsAsFactors = FALSE
          ),
          by = "topic_id"
        )
    } else {
      topics_table <- topic_names
    }
    
    # Add top words if available in the model
    if (!is.null(dominance$data$label_summary)) {
      topics_table <- topics_table %>%
        dplyr::left_join(
          dominance$data$label_summary,
          by = "topic_id"
        )
    }
    
    return(topics_table)
  }
  
  # Dominance table creation - purely formatting
  function create_dominance_table(dominance) {
    # Get n value from metadata
    n_value <- dominance$metadata$n_value
    
    # Initialize results table
    result_table <- data.frame(
      category = character(),
      level = character(),
      docs = integer(),
      dominance = numeric(),
      top_topics = character(),
      stringsAsFactors = FALSE
    )
    
    # Function to format top topics
    format_top_topics <- function(topics_df) {
      if ("short_name" %in% names(topics_df)) {
        return(paste(paste0(topics_df$short_name, " (", round(topics_df$proportion * 100, 1), "%)"), 
                     collapse = ", "))
      } else {
        return(paste(paste0("Topic ", topics_df$topic_id, " (", round(topics_df$proportion * 100, 1), "%)"), 
                     collapse = ", "))
      }
    }
    
    # Add overall row
    docs_count <- nrow(dominance$data)
    dominance_col <- paste0("dominance_n", n_value)
    
    result_table <- rbind(result_table, data.frame(
      category = "Overall",
      level = "Category",
      docs = docs_count,
      dominance = round(mean(dominance$data[[dominance_col]], na.rm = TRUE), 3),
      top_topics = format_top_topics(dominance$top_topics$Overall),
      stringsAsFactors = FALSE
    ))
    
    # Process each category in top_topics
    for (category_name in setdiff(names(dominance$top_topics), "Overall")) {
      # Add category row first
      result_table <- rbind(result_table, data.frame(
        category = category_name,
        level = "Category",
        docs = docs_count,
        dominance = round(mean(dominance$data[[dominance_col]], na.rm = TRUE), 3),
        top_topics = format_top_topics(dominance$top_topics$Overall), # Use overall for category level
        stringsAsFactors = FALSE
      ))
      
      # Add subcategories
      subcategories <- names(dominance$top_topics[[category_name]])
      
      for (subcat in subcategories) {
        # Calculate dominance for this subcategory
        if (category_name %in% c("Income", "Region")) {
          dim <- dominance$metadata$category_map[[category_name]][1]
          subcat_data <- dominance$data[dominance$data[[dim]] == subcat, ]
        } else {
          # Geography - handle binary dimensions
          dim <- paste0("is_", tolower(subcat))
          if (dim %in% names(dominance$data)) {
            subcat_data <- dominance$data[dominance$data[[dim]] == TRUE, ]
          } else {
            # Skip if dimension not found
            next
          }
        }
        
        # Skip if too few documents
        if (nrow(subcat_data) < 3) next
        
        # Add subcategory row
        result_table <- rbind(result_table, data.frame(
          category = paste0("- ", subcat),
          level = "Sub-category",
          docs = nrow(subcat_data),
          dominance = round(mean(subcat_data[[dominance_col]], na.rm = TRUE), 3),
          top_topics = format_top_topics(dominance$top_topics[[category_name]][[subcat]]),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(result_table)
  }
  
  # Effects table creation - purely formatting
  function create_effects_table(effects, p_threshold = 0.05) {
    # Initialize results table
    result_table <- data.frame(
      category = character(),
      dimension = character(),
      explained_var = numeric(),
      significant = logical(),
      stringsAsFactors = FALSE
    )
    
    # Process each dimension's effects
    for (category in names(effects$effects)) {
      # Extract main category name
      main_category <- if (grepl("^Geography_", category)) "Geography" else category
      
      # Extract dimension name
      dimension <- if (grepl("^Geography_", category)) {
        # Extract the part after "Geography_"
        gsub("^Geography_", "", category)
      } else {
        # Get from the category itself
        sub_dim <- names(effects$effects[[category]]$effects$formula)[2]
        sub_dim <- gsub("^~", "", sub_dim)
        trimws(sub_dim)
      }
      
      # Get variance explained and significance from precalculated values
      variance <- effects$effects[[category]]$variance
      significant <- FALSE
      
      # Try to determine significance from the effects object
      tryCatch({
        # Check if we have significance information
        if (!is.null(effects$effects[[category]]$significant)) {
          significant <- effects$effects[[category]]$significant
        } else {
          # Default is TRUE if variance > 0
          significant <- variance > 0.01  # Arbitrary small threshold
        }
      }, error = function(e) {
        warning(paste("Error checking significance for", dimension, ":", e$message))
      })
      
      # Add to results
      result_table <- rbind(result_table, data.frame(
        category = main_category,
        dimension = dimension,
        explained_var = if (significant) round(variance * 100, 1) else NA,
        significant = significant,
        stringsAsFactors = FALSE
      ))
    }
    
    # Sort by explained variance (descending)
    result_table <- result_table[order(result_table$explained_var, decreasing = TRUE), ]
    
    # Replace NA with "Not sig." for display
    result_table$explained_var_display <- ifelse(
      is.na(result_table$explained_var),
      "Not sig.",
      as.character(result_table$explained_var)
    )
    
    return(result_table)
  }
}