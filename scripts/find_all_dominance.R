#' @title Calculate dominance across all documents and dimensions
#' @description Calculates discourse dominance at the document level for all documents
#'   and specified n-values, preparing data for hierarchical analysis.
#'   
#' @param model The output from fit_model() containing topic data
#' @param value_col Name of column containing proportion values (default: "Proportion")
#' @param doc_id_col Name of column containing document identifiers (default: "doc_id")
#' @param n_values Vector of top-n values to calculate (default: c(3, 5, 8))
#' @param output_path Path to save results (default: "data/dominance_analysis.rds")
#'
#' @return A list containing:
#'   \item{data}{List of data frames, one for each n-value with document dominance values}
#'   \item{metadata}{Processing information including timing and document counts}
#'   \item{diagnostics}{Processing details and potential issues}

find_all_dominance <- function(
    model,
    value_col = "Proportion", 
    doc_id_col = "doc_id",
    n_values = c(3, 5, 8),
    output_path = "data/dominance_analysis.rds"
) {
  ## --- Initialization ------------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character()
  )
  
  ## --- Input validation and data extraction --------------------------------
  log_message("Validating inputs", "find_all_dominance")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model)) {
    stop("model must be the output from fit_model() with a 'data' component")
  }
  
  # Extract topic_data from model
  if (!"topic_data" %in% names(model$data)) {
    stop("model$data must contain a 'topic_data' component")
  }
  
  # Get the topic data in long format
  topic_data <- model$data$topic_data
  
  # Validate topic data structure
  required_cols <- c(value_col, doc_id_col, "Topic")
  missing_cols <- setdiff(required_cols, names(topic_data))
  if (length(missing_cols) > 0) {
    stop("Topic data missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate n_values
  if (!is.numeric(n_values) || any(n_values < 1)) {
    stop("n_values must be positive integers")
  }
  
  ## --- Get unique document IDs ---------------------------------------------
  log_message("Identifying unique documents", "find_all_dominance")
  
  # Get list of unique document IDs
  unique_docs <- unique(topic_data[[doc_id_col]])
  doc_count <- length(unique_docs)
  
  log_message(paste("Found", doc_count, "unique documents to process"), "find_all_dominance")
  
  ## --- Extract document metadata -------------------------------------------
  log_message("Extracting document metadata", "find_all_dominance")
  
  # Get non-topic columns from topic_data
  meta_cols <- setdiff(names(topic_data), c("Topic", value_col))
  
  # Get one row per document with all metadata
  doc_metadata <- topic_data %>%
    dplyr::select(dplyr::all_of(meta_cols)) %>%
    dplyr::distinct(!!dplyr::sym(doc_id_col), .keep_all = TRUE)
  
  ## --- Process each n-value ------------------------------------------------
  log_message("Calculating dominance for each n-value", "find_all_dominance")
  
  # Initialize results list
  results <- list()
  
  # Process each n-value
  for (n_val in n_values) {
    log_message(paste("Processing n =", n_val), "find_all_dominance")
    
    # Initialize data frame for this n-value
    n_results <- data.frame(
      doc_id = character(doc_count),
      dominance = numeric(doc_count),
      stringsAsFactors = FALSE
    )
    names(n_results)[1] <- doc_id_col
    names(n_results)[2] <- paste0("dominance_n", n_val)
    
    # Process each document
    for (i in 1:doc_count) {
      doc_id <- unique_docs[i]
      
      # Log progress periodically
      if (i %% 10 == 0 || i == 1 || i == doc_count) {
        log_message(paste("Document", i, "of", doc_count), "find_all_dominance")
      }
      
      # Calculate dominance for this document
      doc_result <- find_dominance(
        data = topic_data,
        value_col = value_col,
        doc_id_col = doc_id_col,
        n = n_val,
        filter_col = doc_id_col,
        filter_value = doc_id
      )
      
      # Store the result
      n_results[i, doc_id_col] <- doc_id
      n_results[i, paste0("dominance_n", n_val)] <- doc_result$raw_dominance
    }
    
    # Join with metadata
    full_results <- dplyr::left_join(n_results, doc_metadata, by = doc_id_col)
    
    # Store in results list
    results[[paste0("n", n_val)]] <- full_results
    
    log_message(paste("Completed dominance calculations for n =", n_val), "find_all_dominance")
  }
  
  ## --- Finalize and return results -----------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata
  metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    total_documents = doc_count,
    n_values = n_values
  )
  
  # Create final result
  final_result <- create_result(
    data = results,
    metadata = metadata,
    diagnostics = diagnostics
  )
  
  log_message(paste("Document dominance analysis complete for", doc_count, 
                    "documents and", length(n_values), "n-values in", 
                    round(processing_time, 2), "seconds"), 
              "find_all_dominance")
  
  # Save to output path if provided
  if (!is.null(output_path)) {
    saveRDS(final_result, output_path)
    log_message(paste("Results saved to", output_path), "find_all_dominance")
  }
  
  return(final_result)
}