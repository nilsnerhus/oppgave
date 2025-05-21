#' @title Calculate Topic Dominance Across Categories
#' @description Calculates how much of the discourse is dominated by the top N topics,
#'   both overall and by categories defined in the category map.
#'
#' @param model STM model result from fit_model()
#' @param topics Named topics result from name_topics()
#' @param metadata Metadata containing document info and category map
#' @param n Number of top topics to consider (default: 3)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item dominance - Overall dominance score
#'       \item all_topics_table - Complete table of all topics with proportions
#'       \item hierarchical_dominance - Hierarchical table showing dominance by category/subcategory
#'     }
#'   }
#'   \item{metadata}{Processing information}
#'   \item{diagnostics}{Processing issues}
calculate_dominance <- function(model, topics, metadata, n = 3) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    calculation_issues = character(),
    skipped_categories = character(),
    processing_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "calculate_dominance")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model) || !"model" %in% names(model$data)) {
    error_msg <- "model must be the output from fit_model() with a 'model' component"
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, error_msg)
    log_message(error_msg, "calculate_dominance", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate topics structure
  if (!is.list(topics) || !"data" %in% names(topics) || 
      !"topics_table" %in% names(topics$data)) {
    error_msg <- "topics must be the output from name_topics()"
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, error_msg)
    log_message(error_msg, "calculate_dominance", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate metadata
  if (!is.list(metadata) || !"data" %in% names(metadata) || 
      !"config" %in% names(metadata$data) || !"metadata" %in% names(metadata$data)) {
    error_msg <- "metadata must contain both config and metadata components"
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, error_msg)
    log_message(error_msg, "calculate_dominance", "ERROR")
    
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
    log_message(error_msg, "calculate_dominance", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Extract model, metadata, and category map ------------------------------
  log_message("Extracting model, metadata, and category map", "calculate_dominance")
  
  # Extract STM model and theta matrix
  stm_model <- model$data$model
  theta <- stm_model$theta
  
  # Get dimensions
  n_docs <- nrow(theta)
  k <- ncol(theta)
  
  log_message(paste("Analyzing", k, "topics across", n_docs, "documents"), "calculate_dominance")
  
  # Get topic names
  topics_table <- topics$data$topics_table
  
  # Ensure topic_id is numeric for correct matching
  if ("topic_id" %in% names(topics_table)) {
    topics_table$topic_id <- as.numeric(topics_table$topic_id)
  }
  
  # Extract document metadata
  doc_metadata <- metadata$data$metadata
  
  # Extract category map
  category_map <- metadata$data$config$category_map
  
  # Check if we have a valid category map
  if (is.null(category_map) || length(category_map) == 0) {
    warning_msg <- "No valid category map found in metadata, will only calculate overall dominance"
    diagnostics$calculation_issues <- c(diagnostics$calculation_issues, warning_msg)
    log_message(warning_msg, "calculate_dominance", "WARNING")
    
    # Create a minimal default category map
    if ("region" %in% names(doc_metadata)) {
      category_map <- list(Region = "region")
      log_message("Using default 'Region' category from metadata", "calculate_dominance")
    }
  }
  
  ## --- Calculate overall dominance --------------------------------------------
  log_message("Calculating overall dominance", "calculate_dominance")
  
  # Calculate average topic proportions across all documents
  topic_proportions <- colMeans(theta)
  
  # Sort topic proportions in descending order
  sorted_indices <- order(topic_proportions, decreasing = TRUE)
  sorted_proportions <- topic_proportions[sorted_indices]
  
  # Get the top n topics
  top_n <- min(n, k)
  top_indices <- sorted_indices[1:top_n]
  top_proportions <- sorted_proportions[1:top_n]
  
  # Calculate dominance (sum of top n proportions)
  dominance_value <- sum(top_proportions)
  
  log_message(paste("Top", top_n, "topics account for", 
                    round(dominance_value * 100, 1), "% of the discourse"), 
              "calculate_dominance")
  
  ## --- Create complete topics table -------------------------------------------
  log_message("Creating complete topics table", "calculate_dominance")
  
  # Create data frame of all topics with proportions
  all_topics <- data.frame(
    topic_id = 1:k,
    proportion = topic_proportions,
    percentage = round(topic_proportions * 100, 2),
    rank = rank(-topic_proportions),
    stringsAsFactors = FALSE
  )
  
  # Add topic names
  all_topics$topic_name <- paste("Topic", all_topics$topic_id)
  if (nrow(topics_table) > 0 && "topic_name" %in% names(topics_table)) {
    for (i in 1:nrow(all_topics)) {
      topic_id <- all_topics$topic_id[i]
      name_row <- which(topics_table$topic_id == topic_id)
      
      if (length(name_row) > 0) {
        all_topics$topic_name[i] <- topics_table$topic_name[name_row[1]]
      }
    }
  }
  
  # Sort by proportion (descending)
  all_topics <- all_topics[order(all_topics$proportion, decreasing = TRUE), ]
  
  ## --- Initialize hierarchical dominance table --------------------------------
  log_message("Initializing hierarchical dominance table", "calculate_dominance")
  
  # Get top topic names
  top_topic_names <- character(top_n)
  for (i in 1:top_n) {
    topic_id <- top_indices[i]
    if (topic_id <= nrow(all_topics)) {
      top_topic_names[i] <- all_topics$topic_name[all_topics$topic_id == topic_id]
    } else {
      top_topic_names[i] <- paste("Topic", topic_id)
    }
  }
  
  # Create hierarchical dominance table starting with overall
  hierarchical_dominance <- data.frame(
    Category = "Overall",
    Subcategory = NA_character_,
    Documents = n_docs,
    Dominance = round(dominance_value, 3),
    `Dominance (%)` = paste0(round(dominance_value * 100, 1), "%"),
    `Top Topics` = paste(top_topic_names[1:min(3, length(top_topic_names))], collapse = ", "),
    stringsAsFactors = FALSE
  )
  
  ## --- Calculate dominance by categories --------------------------------------
  if (!is.null(category_map) && length(category_map) > 0) {
    log_message("Calculating dominance by categories", "calculate_dominance")
    
    # Process each category in the map
    for (category_name in names(category_map)) {
      log_message(paste("Processing category:", category_name), "calculate_dominance")
      
      # Get the column names for this category
      category_cols <- category_map[[category_name]]
      
      # Add category row (same as overall)
      hierarchical_dominance <- rbind(hierarchical_dominance, data.frame(
        Category = category_name,
        Subcategory = NA_character_,
        Documents = n_docs,
        Dominance = round(dominance_value, 3),
        `Dominance (%)` = paste0(round(dominance_value * 100, 1), "%"),
        `Top Topics` = paste(top_topic_names[1:min(3, length(top_topic_names))], collapse = ", "),
        stringsAsFactors = FALSE
      ))
      
      # Process based on category type
      if (category_name %in% c("Income", "Region")) {
        # These categories are single columns with categorical values
        col_name <- category_cols[1]
        
        # Check if column exists in metadata
        if (!col_name %in% names(doc_metadata)) {
          warning_msg <- paste("Column", col_name, "not found in metadata, skipping category", category_name)
          diagnostics$skipped_categories <- c(diagnostics$skipped_categories, warning_msg)
          log_message(warning_msg, "calculate_dominance", "WARNING")
          next
        }
        
        # Get unique values for this category
        subcategories <- unique(doc_metadata[[col_name]])
        subcategories <- subcategories[!is.na(subcategories)]
        
        # Process each subcategory
        for (subcat in subcategories) {
          # Find documents in this subcategory
          doc_indices <- which(doc_metadata[[col_name]] == subcat)
          
          if (length(doc_indices) < 3) {
            warning_msg <- paste("Too few documents for", subcat, "in", category_name, "- skipping")
            diagnostics$skipped_categories <- c(diagnostics$skipped_categories, warning_msg)
            log_message(warning_msg, "calculate_dominance", "WARNING")
            next
          }
          
          # Calculate dominance for this subcategory
          subcat_dominance <- calculate_subset_dominance(theta, doc_indices, top_n)
          
          # Get top topic IDs for this subcategory
          subcat_top_indices <- get_top_topic_indices(theta, doc_indices, top_n)
          
          # Get top topic names for this subcategory
          subcat_top_names <- character(top_n)
          for (i in 1:top_n) {
            if (i <= length(subcat_top_indices)) {
              topic_id <- subcat_top_indices[i]
              subcat_top_names[i] <- all_topics$topic_name[all_topics$topic_id == topic_id]
            }
          }
          
          # Add to hierarchical table
          hierarchical_dominance <- rbind(hierarchical_dominance, data.frame(
            Category = paste0("- ", subcat),
            Subcategory = subcat,
            Documents = length(doc_indices),
            Dominance = round(subcat_dominance, 3),
            `Dominance (%)` = paste0(round(subcat_dominance * 100, 1), "%"),
            `Top Topics` = paste(subcat_top_names[1:min(3, length(subcat_top_names))], collapse = ", "),
            stringsAsFactors = FALSE
          ))
        }
      } else if (category_name == "Geography") {
        # Geography has multiple binary columns like is_sids, is_lldc
        for (col_name in category_cols) {
          # Check if column exists in metadata
          if (!col_name %in% names(doc_metadata)) {
            warning_msg <- paste("Column", col_name, "not found in metadata, skipping")
            diagnostics$skipped_categories <- c(diagnostics$skipped_categories, warning_msg)
            log_message(warning_msg, "calculate_dominance", "WARNING")
            next
          }
          
          # Format subcategory name (remove "is_" prefix if present)
          subcategory_name <- gsub("is_", "", col_name)
          subcategory_name <- toupper(subcategory_name)
          
          # Find documents in this subcategory (where the binary column is TRUE)
          doc_indices <- which(doc_metadata[[col_name]] == TRUE)
          
          if (length(doc_indices) < 3) {
            warning_msg <- paste("Too few documents for", subcategory_name, "- skipping")
            diagnostics$skipped_categories <- c(diagnostics$skipped_categories, warning_msg)
            log_message(warning_msg, "calculate_dominance", "WARNING")
            next
          }
          
          # Calculate dominance for this subcategory
          subcat_dominance <- calculate_subset_dominance(theta, doc_indices, top_n)
          
          # Get top topic IDs for this subcategory
          subcat_top_indices <- get_top_topic_indices(theta, doc_indices, top_n)
          
          # Get top topic names for this subcategory
          subcat_top_names <- character(top_n)
          for (i in 1:top_n) {
            if (i <= length(subcat_top_indices)) {
              topic_id <- subcat_top_indices[i]
              subcat_top_names[i] <- all_topics$topic_name[all_topics$topic_id == topic_id]
            }
          }
          
          # Add to hierarchical table
          hierarchical_dominance <- rbind(hierarchical_dominance, data.frame(
            Category = paste0("- ", subcategory_name),
            Subcategory = subcategory_name,
            Documents = length(doc_indices),
            Dominance = round(subcat_dominance, 3),
            `Dominance (%)` = paste0(round(subcat_dominance * 100, 1), "%"),
            `Top Topics` = paste(subcat_top_names[1:min(3, length(subcat_top_names))], collapse = ", "),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  ## --- Calculate processing time and create result ----------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    dominance = dominance_value,
    all_topics_table = all_topics,
    hierarchical_dominance = hierarchical_dominance
  )
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    n_value = n,
    k = k,
    n_docs = n_docs,
    categories = if (!is.null(category_map)) names(category_map) else NULL,
    success = TRUE
  )
  
  # Update diagnostics
  diagnostics$processing_stats <- list(
    topic_count = k,
    document_count = n_docs,
    category_count = if (!is.null(category_map)) length(category_map) else 0,
    skipped_categories = length(diagnostics$skipped_categories),
    calculation_issues_count = length(diagnostics$calculation_issues)
  )
  
  log_message("Dominance calculation complete", "calculate_dominance")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}

# Helper function to calculate dominance for a subset of documents
calculate_subset_dominance <- function(theta, doc_indices, n) {
  # Calculate average topic proportions for the subset
  subset_proportions <- colMeans(theta[doc_indices, , drop = FALSE])
  
  # Sort and get top n topics
  sorted_indices <- order(subset_proportions, decreasing = TRUE)
  top_n <- min(n, length(sorted_indices))
  top_indices <- sorted_indices[1:top_n]
  
  # Calculate dominance (sum of top n proportions)
  dominance <- sum(subset_proportions[top_indices])
  
  return(dominance)
}

# Helper function to get top topic indices for a subset of documents
get_top_topic_indices <- function(theta, doc_indices, n) {
  # Calculate average topic proportions for the subset
  subset_proportions <- colMeans(theta[doc_indices, , drop = FALSE])
  
  # Sort and get top n topics
  sorted_indices <- order(subset_proportions, decreasing = TRUE)
  top_n <- min(n, length(sorted_indices))
  top_indices <- sorted_indices[1:top_n]
  
  return(top_indices)
}