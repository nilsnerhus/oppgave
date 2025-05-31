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
    log_message(paste("Using cached result from", basename(cache_path)), "auto_cache")  # FIXED: Added closing parenthesis and func parameter
    return(readRDS(cache_path))
  } else {
    # Log appropriate message based on whether we're forcing overwrite
    if (overwrite && file.exists(cache_path)) {
      log_message(paste("Overwriting cache for", basename(cache_path)), "auto_cache")  # FIXED: Added closing parenthesis and func parameter
    } else {
      log_message(paste("Computing new result for", basename(cache_path)), "auto_cache")  # FIXED: Added closing parenthesis and func parameter
    }
    
    result <- do.call(func, args)
    saveRDS(result, cache_path)
    
    # Update the hash
    writeLines(current_hash, hash_path)
    
    return(result)
  }
}

#' @title Cache web scraping results with content change detection
#' @description Caches results from web scraping functions, with the option to check for content
#'   changes before reusing cached results. Supports forced refresh via the overwrite parameter.
#'   
#' @param func The web scraping function to execute if cache is invalid
#' @param ... Arguments to pass to the function
#' @param url URL of the webpage to check for changes (default: "https://napcentral.org/submitted-naps")
#' @param cache_path Path to the cached data (defaults to data/func_name.rds)
#' @param table_index Index of the table to monitor (default: 1)
#' @param overwrite Whether to force recalculation and ignore the cache (default: FALSE)
#' @return The function result (either from cache or newly computed)
web_cache <- function(func, ..., url = "https://napcentral.org/submitted-naps", 
                      cache_path = NULL, table_index = 1, overwrite = FALSE) {
  # Get function name for default cache path
  func_name <- deparse(substitute(func))
  
  # Set default cache path based on function name if not provided
  if (is.null(cache_path)) {
    cache_path <- file.path("data", paste0(func_name, ".rds"))
  }
  
  # Check if cache exists
  if (!file.exists(cache_path) || overwrite) {
    log_message(if(overwrite) "Overwrite requested, fetching fresh data" else "No cache found, need to fetch data")
    # Need to fetch data
    result <- do.call(func, list(...))
    saveRDS(result, cache_path)
    return(result)
  }
  
  # Only check content if we're not overwriting
  if (!overwrite) {
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
}

# Formatting helpers for inline text
pct <- function(x, digits = 1) paste0(round(x * 100, digits), "%")
num <- function(x) {
  if (x < 10000) {
    return(as.character(round(x)))  # No separator for numbers under 10,000
  } else {
    return(format(round(x), big.mark = " "))  # Space separator for 10,000+
  }
}

#' @title Count thesis in standard pages (2400 characters)
#' @description Counts thesis length in standard pages as defined by university regulations
#' @param include_index Whether to include index.qmd (default: FALSE)
#' @param verbose Whether to print detailed breakdown (default: TRUE)
#' @return List with page count and statistics
count_thesis_pages <- function(include_index = TRUE, verbose = TRUE) {
  
  # Constants
  CHARS_PER_PAGE <- 2400
  MIN_PAGES <- 60
  MAX_PAGES <- 80
  
  # Get all QMD files
  qmd_files <- list.files("text", pattern = "\\.qmd$", full.names = TRUE)
  
  # Add index.qmd if requested
  if (include_index && file.exists("index.qmd")) {
    qmd_files <- c("index.qmd", qmd_files)
  }
  
  # Process each file
  file_stats <- list()
  
  for (file in qmd_files) {
    lines <- readLines(file, warn = FALSE)
    
    # Remove YAML header
    if (length(lines) > 0 && lines[1] == "---") {
      yaml_end <- which(lines == "---")[2]
      if (!is.na(yaml_end)) {
        lines <- lines[(yaml_end + 1):length(lines)]
      }
    }
    
    # Remove code chunks
    in_chunk <- FALSE
    clean_lines <- character()
    
    for (line in lines) {
      if (grepl("^```\\{", line)) {
        in_chunk <- TRUE
      } else if (grepl("^```$", line) && in_chunk) {
        in_chunk <- FALSE
      } else if (!in_chunk) {
        # Remove inline code
        line <- gsub("`r[^`]+`", "", line)
        clean_lines <- c(clean_lines, line)
      }
    }
    
    # Combine text
    text <- paste(clean_lines, collapse = " ")
    
    # Count characters WITH spaces (this is what matters)
    chars <- nchar(text)
    pages <- chars / CHARS_PER_PAGE
    
    file_stats[[basename(file)]] <- list(
      chars = chars,
      pages = pages
    )
  }
  
  # Calculate totals
  total_chars <- sum(sapply(file_stats, function(x) x$chars))
  total_pages <- total_chars / CHARS_PER_PAGE
  
  # Check requirements
  within_limits <- total_pages >= MIN_PAGES && total_pages <= MAX_PAGES
  pages_to_min <- max(0, MIN_PAGES - total_pages)
  pages_to_max <- max(0, total_pages - MAX_PAGES)
  
  # Print if verbose
  if (verbose) {
    cat("\n=== THESIS LENGTH (Standard Pages) ===\n")
    cat("1 standard page = 2,400 characters\n")
    cat("Required: 60-80 pages\n")
    cat("=====================================\n\n")
    
    # Chapter breakdown
    cat("By chapter:\n")
    cat("-------------------------------------\n")
    
    # Sort by chapter order
    chapter_order <- c("intro.qmd", "context.qmd", "lit.qmd", "theory.qmd", 
                       "methods.qmd", "findings.qmd", "discussion.qmd", "conclusion.qmd")
    
    for (chapter in chapter_order) {
      if (chapter %in% names(file_stats)) {
        cat(sprintf("%-15s %5.1f pages (%7s chars)\n", 
                    gsub("\\.qmd", "", chapter), 
                    file_stats[[chapter]]$pages,
                    format(file_stats[[chapter]]$chars, big.mark = ",")))
      }
    }
    
    cat("=====================================\n")
    cat(sprintf("TOTAL:          %5.1f pages (%7s chars)\n", 
                total_pages, format(total_chars, big.mark = ",")))
    cat("=====================================\n\n")
    
    # Status
    if (within_limits) {
      cat("✓ Within required limits (60-80 pages)\n")
      cat(sprintf("  Room to minimum: %.1f pages\n", total_pages - MIN_PAGES))
      cat(sprintf("  Room to maximum: %.1f pages\n", MAX_PAGES - total_pages))
    } else if (total_pages < MIN_PAGES) {
      cat("⚠ BELOW minimum requirement\n")
      cat(sprintf("  Need %.1f more pages (%.0f more characters)\n", 
                  pages_to_min, pages_to_min * CHARS_PER_PAGE))
    } else {
      cat("⚠ ABOVE maximum requirement\n")
      cat(sprintf("  Need to cut %.1f pages (%.0f characters)\n", 
                  pages_to_max, pages_to_max * CHARS_PER_PAGE))
    }
    
    cat("\n")
  }
  
  # Return stats
  return(invisible(list(
    total_pages = total_pages,
    total_chars = total_chars,
    within_limits = within_limits,
    pages_to_min = pages_to_min,
    pages_to_max = pages_to_max,
    by_file = file_stats,
    requirements = list(
      min = MIN_PAGES,
      max = MAX_PAGES,
      chars_per_page = CHARS_PER_PAGE
    )
  )))
}

#' @title Quick page count
thesis_pages <- function() {
  stats <- count_thesis_pages(verbose = FALSE)
  cat(sprintf("%.1f standard pages", stats$total_pages))
  if (!stats$within_limits) {
    if (stats$total_pages < stats$requirements$min) {
      cat(sprintf(" (%.1f below minimum)", stats$pages_to_min))
    } else {
      cat(sprintf(" (%.1f above maximum)", stats$pages_to_max))
    }
  }
  cat("\n")
}