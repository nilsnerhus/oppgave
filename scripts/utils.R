#' @title Utility functions for climate adaptation analysis
#' @description Common helper functions used across the NAP analysis pipeline

#' @title Create directory if it doesn't exist
#' @description Ensures that a directory exists for saving output files
#' @param path Path to file or directory to ensure exists
ensure_directory <- function(path) {
  if (is.null(path)) return(invisible())
  
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message("Created directory: ", dir)
  }
}

#' @title Format timestamp for logging
#' @description Returns current time in consistent format for logging
get_timestamp <- function() {
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
}

#' @title Log message with timestamp and function name
#' @description Outputs a timestamped message for consistent logging
#' @param msg Message to output
#' @param func Function name to include in log
#' @param type Message type (e.g., "INFO", "WARNING", "ERROR")
log_message <- function(msg, func = NULL, type = "INFO") {
  prefix <- if (!is.null(func)) paste0("[", func, "] ") else ""
  message(get_timestamp(), " ", prefix, type, ": ", msg)
}

#' @title Validate common input parameters
#' @description Checks that input data meets basic requirements
#' @param input_data Data frame or list to validate
#' @param required_cols Vector of column names that must exist
#' @param func_name Name of calling function for error messages
validate_input <- function(input_data, required_cols = NULL, func_name = "function") {
  # Check data frame
  if (!is.data.frame(input_data) && !is.list(input_data)) {
    stop(func_name, ": Input must be a data frame or list")
  }
  
  # For data frames, check rows and columns
  if (is.data.frame(input_data)) {
    if (nrow(input_data) == 0) {
      stop(func_name, ": Input has no rows")
    }
    
    if (!is.null(required_cols)) {
      missing_cols <- setdiff(required_cols, names(input_data))
      if (length(missing_cols) > 0) {
        stop(func_name, ": Input is missing required columns: ", 
             paste(missing_cols, collapse = ", "))
      }
    }
  }
  
  # For lists with special structure (like stm results)
  if (is.list(input_data) && !is.data.frame(input_data) && !is.null(required_cols)) {
    missing_elements <- setdiff(required_cols, names(input_data))
    if (length(missing_elements) > 0) {
      stop(func_name, ": Input is missing required elements: ", 
           paste(missing_elements, collapse = ", "))
    }
  }
  
  return(TRUE)
}

#' @title Create standardized result structure
#' @description Generates a consistent result template for functions
#' @param data Main result data
#' @param metadata Additional information about the processing
#' @param diagnostics Quality metrics or validation results
create_result <- function(data = NULL, metadata = list(), diagnostics = list()) {
  # Basic metadata that should be included in all results
  base_metadata <- list(
    timestamp = Sys.time(),
    r_version = R.version.string,
    session_info = sessionInfo()$platform
  )
  
  # Merge with provided metadata
  metadata <- c(base_metadata, metadata)
  
  # Create and return result structure
  list(
    data = data,
    metadata = metadata,
    diagnostics = diagnostics
  )
}

#' @title Track execution time of a code block
#' @description Measures how long a section of code takes to run
#' @param expr Expression to evaluate and time
#' @param func_name Function name for logging
#' @return The result of the expression
time_operation <- function(expr, func_name = NULL) {
  start_time <- Sys.time()
  prefix <- if (!is.null(func_name)) paste0("[", func_name, "] ") else ""
  
  # Evaluate the expression
  result <- eval(expr, parent.frame())
  
  # Calculate runtime
  end_time <- Sys.time()
  runtime <- difftime(end_time, start_time, units = "secs")
  
  # Log the runtime
  message(get_timestamp(), " ", prefix, 
          "Operation completed in ", round(as.numeric(runtime), 2), " seconds")
  
  # Return the result
  return(result)
}

#' @title Check cache and conditionally execute function
#' @description Checks if a cached result exists and is newer than dependencies
#' @param cache_path Path to look for cached result
#' @param dependencies Files that the result depends on
#' @param recreate_fn Function to call if cache needs to be refreshed
#' @return The cached or freshly created result
use_cache <- function(cache_path, dependencies = NULL, recreate_fn) {
  should_recreate <- FALSE
  
  # Check if cache exists
  if (!file.exists(cache_path)) {
    should_recreate <- TRUE
  } else if (!is.null(dependencies)) {
    # Check if any dependency is newer than cache
    cache_mtime <- file.info(cache_path)$mtime
    for (dep in dependencies) {
      if (file.exists(dep) && file.info(dep)$mtime > cache_mtime) {
        should_recreate <- TRUE
        break
      }
    }
  }
  
  # Either load cache or recreate
  if (should_recreate) {
    log_message(paste("Creating", basename(cache_path)))
    result <- recreate_fn()
    saveRDS(result, cache_path)
    return(result)
  } else {
    log_message(paste("Using cached", basename(cache_path)))
    return(readRDS(cache_path))
  }
}