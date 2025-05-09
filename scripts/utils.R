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

#' @title Automatic function caching
#' @description Caches function results based on automatic hashing of all inputs
#' @param func The function to execute if cache is invalid
#' @param ... All arguments to pass to the function
#' @param cache_path Optional custom path to store/retrieve cached results
#' @param overwrite Whether to force recalculation and overwrite any existing cache (default: FALSE)
#' @return The function result (either from cache or newly generated)
auto_cache <- function(func, ..., cache_path = NULL, overwrite = FALSE) {
  # Get function name for default cache path
  func_name <- deparse(substitute(func))
  
  # Set default cache path based on function name if not provided
  if (is.null(cache_path)) {
    cache_path <- file.path("data", paste0(func_name, ".rds"))
  }
  
  # Get all arguments
  args <- list(...)
  
  # Calculate hash of current args
  current_hash <- digest::digest(args, algo = "md5")
  
  # Create directory if needed
  ensure_directory(cache_path)
  
  # Check if cache and hash exist
  hash_path <- paste0(cache_path, ".hash")
  cache_valid <- FALSE
  
  # Only check cache validity if we're not forcing overwrite
  if (!overwrite) {
    if (file.exists(cache_path) && file.exists(hash_path)) {
      # Compare with stored hash
      stored_hash <- readLines(hash_path, warn = FALSE)[1]
      
      if (current_hash == stored_hash) {
        cache_valid <- TRUE
      }
    }
  }
  
  if (cache_valid) {
    log_message(paste("Using cached result from", basename(cache_path)))
    return(readRDS(cache_path))
  } else {
    # Log appropriate message based on whether we're forcing overwrite
    if (overwrite && file.exists(cache_path)) {
      log_message(paste("Overwriting cache for", basename(cache_path), "auto_cache"))
    } else {
      log_message(paste("Computing new result for", basename(cache_path)))
    }
    
    result <- do.call(func, args)
    saveRDS(result, cache_path)
    
    # Update the hash
    writeLines(current_hash, hash_path)
    
    return(result)
  }
}

#' @title Check for web content changes
#' @description Monitors a web page for changes by checking table content
#' @param func The function to execute if content has changed
#' @param ... Arguments to pass to the function
#' @param url URL of the webpage to check
#' @param cache_path Path to the cached data (defaults to data/func_name.rds)
#' @param table_index Index of the table to monitor (default: 1)
#' @return The function result (either from cache or newly computed)
web_cache <- function(func, ..., url = "https://napcentral.org/submitted-naps", 
                              cache_path = NULL, table_index = 1) {
  # Get function name for default cache path
  func_name <- deparse(substitute(func))
  
  # Set default cache path based on function name if not provided
  if (is.null(cache_path)) {
    cache_path <- file.path("data", paste0(func_name, ".rds"))
  }
  
  # Check if cache exists
  if (!file.exists(cache_path)) {
    log_message("No cache found, need to fetch data")
    # Need to fetch data
    result <- do.call(func, list(...))
    saveRDS(result, cache_path)
    return(result)
  }
  
  # Content-based check
  size_path <- paste0(cache_path, ".size")
  content_changed <- FALSE
  
  tryCatch({
    # Do a quick scrape to check the content
    session <- polite::bow(url = url)
    quick_html <- polite::scrape(session)
    tables <- rvest::html_nodes(quick_html, "table")
    
    if (length(tables) >= table_index) {
      # Get the table and count rows as a proxy for content
      table_html <- tables[[table_index]]
      rows <- rvest::html_nodes(table_html, "tr")
      current_row_count <- length(rows)
      
      # Calculate a simple content signature
      row_texts <- sapply(rows, rvest::html_text)
      content_signature <- digest::digest(row_texts)
      
      # Check against previous signature if available
      if (file.exists(size_path)) {
        previous_data <- readLines(size_path, warn = FALSE)
        if (length(previous_data) >= 2) {
          previous_row_count <- as.numeric(previous_data[1])
          previous_signature <- previous_data[2]
          
          # Check if content changed
          if (current_row_count != previous_row_count || 
              content_signature != previous_signature) {
            content_changed <- TRUE
            log_message("Web content has changed")
          }
        }
      } else {
        # No previous data, assume content changed
        content_changed <- TRUE
      }
      
      # Save current data for future comparison
      writeLines(c(as.character(current_row_count), content_signature), size_path)
    } else {
      # Table not found, assume content changed
      content_changed <- TRUE
    }
  }, error = function(e) {
    log_message(paste("Error checking web content:", e$message), "web_cache", "WARNING")
    # On error, default to needing fresh data to be safe
    content_changed <- TRUE
  })
  
  if (content_changed) {
    # Content changed, need to fetch new data
    result <- do.call(func, list(...))
    saveRDS(result, cache_path)
    return(result)
  } else {
    log_message("Web content unchanged, using cached data")
    # Use cached data
    return(readRDS(cache_path))
  }
}