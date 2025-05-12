#' @title Create summary tables from document dominance results
#' @description Orchestrates the creation of summary tables analyzing dominance 
#'   patterns across different dimensions, including dominance values, variance 
#'   measures, and explanatory power of different categories.
#'
#' @param dominance_data Output from find_all_dominance() containing document-level dominance values
#' @param n_value The n-value to use for analysis (default: 5)
#' @param category_map List mapping higher-level categories to dimensions 
#'   (default: list(Income = "wb_income_level", Region = "region", 
#'   Geography = c("is_sids", "is_lldc")))
#' @param output_path Path to save results (default: "data/result_tables.rds")
#'
#' @return A list containing:
#'   \item{dominance}{Table of dominance values by category and subcategory}
#'   \item{variance}{Table of variance measures by category and subcategory}
#'   \item{explained}{Table of explanatory power for different dimensions}
#'   \item{metadata}{Processing information and parameters}

create_result_tables <- function(
    dominance_data,
    n_value = 5,
    category_map = list(
      Income = "wb_income_level", 
      Region = "region", 
      Geography = c("is_sids", "is_lldc")
    ),
    output_path = "data/result_tables.rds"
) {
  ## --- Initialization ------------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    skipped_dimensions = character(),
    processing_issues = character()
  )
  
  # Extract all dimensions from category map
  all_dimensions <- unlist(category_map)
  
  ## --- Input validation ----------------------------------------------------
  log_message("Validating input data", "create_result_tables")
  
  tryCatch({
    # Check if dominance_data is the expected structure
    if (!is.list(dominance_data) || !"data" %in% names(dominance_data)) {
      stop("dominance_data must be the output from find_all_dominance()")
    }
    
    # Check if n_value is available in the data
    n_key <- paste0("n", n_value)
    if (!n_key %in% names(dominance_data$data)) {
      stop("n_value ", n_value, " not found in dominance_data")
    }
    
    # Validate category_map
    if (!is.list(category_map) || length(category_map) == 0) {
      stop("category_map must be a non-empty list")
    }
    
  }, error = function(e) {
    log_message(paste("Validation error:", e$message), "create_result_tables", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, e$message)
    stop(e$message)
  })
  
  ## --- Extract relevant data -----------------------------------------------
  log_message(paste("Extracting document data for n =", n_value), "create_result_tables")
  
  # Get the specific n-value data
  n_key <- paste0("n", n_value)
  doc_data <- dominance_data$data[[n_key]]
  dominance_col <- paste0("dominance_n", n_value)
  
  # Check dimensions exist in data
  available_dimensions <- all_dimensions[all_dimensions %in% names(doc_data)]
  missing_dimensions <- setdiff(all_dimensions, available_dimensions)
  
  if (length(missing_dimensions) > 0) {
    log_message(paste("Dimensions not found in data:", paste(missing_dimensions, collapse = ", ")),
                "create_result_tables", "WARNING")
    diagnostics$skipped_dimensions <- missing_dimensions
    
    # Update category_map to remove missing dimensions
    for (cat_name in names(category_map)) {
      category_map[[cat_name]] <- intersect(category_map[[cat_name]], available_dimensions)
    }
    
    # Remove empty categories
    empty_cats <- names(which(sapply(category_map, length) == 0))
    if (length(empty_cats) > 0) {
      category_map[empty_cats] <- NULL
      log_message(paste("Removed empty categories:", paste(empty_cats, collapse = ", ")),
                  "create_result_tables", "WARNING")
    }
  }
  
  if (length(unlist(category_map)) == 0) {
    log_message("No valid dimensions found in data", "create_result_tables", "ERROR")
    stop("No valid dimensions found in data")
  }
  
  ## --- Create dominance table ----------------------------------------------
  log_message("Creating dominance table", "create_result_tables")
  
  dominance_table <- create_dominance_table(doc_data, dominance_col, category_map)
  
  ## --- Create variance table -----------------------------------------------
  log_message("Creating variance table", "create_result_tables")
  
  variance_table <- create_variance_table(doc_data, dominance_col, category_map)
  
  ## --- Create explained table ----------------------------------------------
  log_message("Creating explained variance table", "create_result_tables")
  
  explained_table <- create_explained_table(doc_data, dominance_col, category_map)
  
  ## --- Combine and return results ------------------------------------------
  log_message("Finalizing results", "create_result_tables")
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    n_value = n_value,
    category_map = category_map,
    total_documents = nrow(doc_data)
  )
  
  # Create final result
  final_result <- create_result(
    data = list(
      dominance = dominance_table,
      variance = variance_table,
      explained = explained_table
    ),
    metadata = metadata,
    diagnostics = diagnostics
  )
  
  # Save results if output_path is provided
  if (!is.null(output_path)) {
    log_message(paste("Saving results to", output_path), "create_result_tables")
    saveRDS(final_result, output_path)
  }
  
  log_message(paste("Result tables creation complete in", 
                    round(processing_time, 2), "seconds"), 
              "create_result_tables")
  
  return(final_result)
}