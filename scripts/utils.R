# ===============================
# Utility functions for NAP analysis (leaned-down)
# ===============================

# --- Helpers -----------------------------------------------------

ensure_directory <- function(path) {
  if (is.null(path)) return(invisible())
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

get_timestamp <- function() format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")

log_message <- function(msg, func = NULL, type = "INFO") {
  prefix <- if (!is.null(func)) paste0("[", func, "] ") else ""
  message(get_timestamp(), " ", prefix, type, ": ", msg)
}

# --- Validation --------------------------------------------------

validate_input <- function(input_data, required_cols = NULL, func_name = "function") {
  if (!is.data.frame(input_data) && !is.list(input_data)) {
    stop(func_name, ": Input must be a data frame or list")
  }
  
  if (is.data.frame(input_data)) {
    if (nrow(input_data) == 0) stop(func_name, ": Input has no rows")
    if (!is.null(required_cols)) {
      missing_cols <- setdiff(required_cols, names(input_data))
      if (length(missing_cols) > 0) stop(func_name, ": Missing columns: ", paste(missing_cols, collapse = ", "))
    }
  }
  
  if (is.list(input_data) && !is.data.frame(input_data) && !is.null(required_cols)) {
    missing_elements <- setdiff(required_cols, names(input_data))
    if (length(missing_elements) > 0) stop(func_name, ": Missing elements: ", paste(missing_elements, collapse = ", "))
  }
  
  TRUE
}

# --- Result structure -------------------------------------------

create_result <- function(data = NULL, metadata = list(), diagnostics = list()) {
  list(
    data = data,
    metadata = c(
      list(timestamp = Sys.time(),
           r_version = R.version.string,
           session_info = sessionInfo()$platform),
      metadata
    ),
    diagnostics = diagnostics
  )
}

# --- Timing ------------------------------------------------------

time_operation <- function(expr, func_name = NULL) {
  start_time <- Sys.time()
  result <- eval(expr, parent.frame())
  runtime <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
  prefix <- if (!is.null(func_name)) paste0("[", func_name, "] ") else ""
  message(get_timestamp(), " ", prefix, "Operation completed in ", runtime, " seconds")
  result
}

# --- Dual save (RDS + JSON) --------------------------------------

save_dual <- function(object, path, hash = NULL) {
  saveRDS(object, path)
  json_path <- sub("\\.rds$", ".json", path)
  tryCatch(
    jsonlite::write_json(object, json_path, pretty = TRUE, auto_unbox = TRUE),
    error = function(e) message("JSON export skipped for ", path, ": ", e$message)
  )
  if (!is.null(hash)) writeLines(hash, paste0(path, ".hash"))
}

# --- Automatic caching ------------------------------------------

auto_cache <- function(func, ..., cache_path = NULL, overwrite = FALSE) {
  func_name <- deparse(substitute(func))
  cache_path <- cache_path %||% file.path("data", paste0(func_name, ".rds"))
  
  args <- list(...)
  current_hash <- digest::digest(args, algo = "md5")
  ensure_directory(cache_path)
  
  hash_path <- paste0(cache_path, ".hash")
  cache_valid <- FALSE
  if (!overwrite && file.exists(cache_path) && file.exists(hash_path)) {
    stored_hash <- readLines(hash_path, warn = FALSE)[1]
    if (identical(current_hash, stored_hash)) cache_valid <- TRUE
  }
  
  if (cache_valid) {
    log_message(paste("Using cached result from", basename(cache_path)), "auto_cache")
    return(readRDS(cache_path))
  }
  
  log_message(if (overwrite && file.exists(cache_path)) paste("Overwriting cache for", basename(cache_path))
              else paste("Computing new result for", basename(cache_path)), "auto_cache")
  
  result <- do.call(func, args)
  save_dual(result, cache_path, current_hash)
  result
}

rds_files <- list.files("data", pattern = "\\.rds$", full.names = TRUE)
for (f in rds_files) {
  obj <- readRDS(f)
  save_dual(obj, f)  # writes .json alongside .rds
}

# --- Web caching -----------------------------------------------

web_cache <- function(func, ..., url = "https://napcentral.org/submitted-naps", 
                      cache_path = NULL, table_index = 1, overwrite = FALSE) {
  func_name <- deparse(substitute(func))
  cache_path <- cache_path %||% file.path("data", paste0(func_name, ".rds"))
  
  if (!file.exists(cache_path) || overwrite) {
    log_message(if (overwrite) "Overwrite requested, fetching fresh data" else "No cache found, fetching data", "web_cache")
    result <- do.call(func, list(...))
    save_dual(result, cache_path)
    return(result)
  }
  
  size_path <- paste0(cache_path, ".size")
  content_changed <- FALSE
  
  tryCatch({
    session <- polite::bow(url)
    tables <- rvest::html_nodes(polite::scrape(session), "table")
    
    if (length(tables) >= table_index) {
      rows <- rvest::html_nodes(tables[[table_index]], "tr")
      signature <- digest::digest(sapply(rows, rvest::html_text))
      current <- c(length(rows), signature)
      
      if (file.exists(size_path)) {
        prev <- readLines(size_path, warn = FALSE)
        if (!identical(current, prev)) {
          content_changed <- TRUE
          log_message("Web content has changed", "web_cache")
        }
      } else content_changed <- TRUE
      
      writeLines(current, size_path)
    } else content_changed <- TRUE
  }, error = function(e) {
    log_message(paste("Error checking web content:", e$message), "web_cache", "WARNING")
    content_changed <- TRUE
  })
  
  if (content_changed) {
    result <- do.call(func, list(...))
    save_dual(result, cache_path)
    return(result)
  }
  
  log_message("Web content unchanged, using cached data", "web_cache")
  readRDS(cache_path)
}

# --- Thesis page counter ----------------------------------------

count_thesis_pages <- function(include_index = TRUE, verbose = TRUE) {
  CHARS_PER_PAGE <- 2400
  MIN_PAGES <- 60
  MAX_PAGES <- 80
  
  qmd_files <- list.files("text", pattern = "\\.qmd$", full.names = TRUE)
  if (include_index && file.exists("index.qmd")) qmd_files <- c("index.qmd", qmd_files)
  
  file_stats <- lapply(qmd_files, function(file) {
    lines <- readLines(file, warn = FALSE)
    
    # Remove YAML
    if (length(lines) > 0 && lines[1] == "---") {
      yaml_end <- which(lines == "---")[2]
      if (!is.na(yaml_end)) lines <- lines[(yaml_end + 1):length(lines)]
    }
    
    # Remove code chunks and inline code
    in_chunk <- FALSE
    clean_lines <- character()
    for (line in lines) {
      if (grepl("^```\\{", line)) in_chunk <- TRUE
      else if (grepl("^```$", line) && in_chunk) in_chunk <- FALSE
      else if (!in_chunk) clean_lines <- c(clean_lines, gsub("`r[^`]+`", "", line))
    }
    
    text <- paste(clean_lines, collapse = " ")
    chars <- nchar(text)
    pages <- chars / CHARS_PER_PAGE
    list(chars = chars, pages = pages)
  })
  
  names(file_stats) <- basename(qmd_files)
  
  total_chars <- sum(sapply(file_stats, `[[`, "chars"))
  total_pages <- total_chars / CHARS_PER_PAGE
  pages_to_min <- max(0, MIN_PAGES - total_pages)
  pages_to_max <- max(0, total_pages - MAX_PAGES)
  within_limits <- total_pages >= MIN_PAGES && total_pages <= MAX_PAGES
  
  if (verbose) {
    cat("\n=== THESIS LENGTH ===\n")
    for (f in names(file_stats)) cat(f, sprintf("%5.1f pages (%d chars)\n", file_stats[[f]]$pages, file_stats[[f]]$chars))
    cat(sprintf("TOTAL: %5.1f pages (%d chars)\n", total_pages, total_chars))
  }
  
  invisible(list(
    total_pages = total_pages,
    total_chars = total_chars,
    within_limits = within_limits,
    pages_to_min = pages_to_min,
    pages_to_max = pages_to_max,
    by_file = file_stats,
    requirements = list(min = MIN_PAGES, max = MAX_PAGES, chars_per_page = CHARS_PER_PAGE)
  ))
}

thesis_pages <- function() {
  stats <- count_thesis_pages(verbose = FALSE)
  cat(sprintf("%.1f standard pages", stats$total_pages))
  if (!stats$within_limits) {
    if (stats$total_pages < stats$requirements$min) cat(sprintf(" (%.1f below minimum)", stats$pages_to_min))
    else cat(sprintf(" (%.1f above maximum)", stats$pages_to_max))
  }
  cat("\n")
}

# --- Dependency updater (leaner) --------------------------------

update_dependencies <- function(
    exclude_packages = "renv",
    quarto_file = "_quarto.yml",
    verbose = FALSE,
    overwrite = TRUE,
    fallback_title = "Research Project",
    fallback_author = "Author"
) {
  qlog <- function(msg, type = "INFO") if (verbose) log_message(msg, "update_dependencies", type)
  
  # Read Quarto metadata
  title <- fallback_title
  subtitle <- ""
  author <- fallback_author
  email <- paste0(tolower(gsub("[^A-Za-z]", "", fallback_author)), "@example.com")
  
  if (file.exists(quarto_file)) {
    try({
      yaml_content <- gsub("\\s*#.*$", "", readLines(quarto_file, warn = FALSE))
      conf <- yaml::yaml.load(paste(yaml_content, collapse = "\n"))
      book <- conf$book
      if (!is.null(book)) {
        title <- book$title %||% fallback_title
        subtitle <- book$subtitle %||% ""
        auth <- book$author %||% fallback_author
        if (is.list(auth)) { author <- auth$name %||% fallback_author; email <- auth$email %||% email }
        else { author <- auth; if (grepl("@", auth)) email <- gsub(".*<(.+@.+)>.*", "\\1", auth) }
        qlog("✓ Quarto metadata loaded")
      }
    }, silent = TRUE)
  }
  
  description <- if (subtitle != "") paste(title, subtitle, sep = ": ") else title
  suppressMessages(renv::snapshot())
  if (!file.exists("renv.lock")) stop("renv.lock not found")
  
  lockfile <- renv::lockfile_read("renv.lock")
  packages <- setdiff(names(lockfile$Packages), exclude_packages)
  
  # Create DESCRIPTION
  package_name <- gsub("[^A-Za-z0-9]", "", gsub("\\s+", "", title))
  if (nchar(package_name) == 0) package_name <- "ResearchProject"
  
  if (overwrite && file.exists("DESCRIPTION")) file.remove("DESCRIPTION")
  
  usethis::use_description(fields = list(
    Package = package_name,
    Title = title,
    Description = description,
    `Authors@R` = paste0('person("', author, '", email = "', email, '", role = c("aut", "cre"))'),
    License = "MIT",
    Version = "0.1.0"
  ), check_name = FALSE)
  
  desc::desc(file = "DESCRIPTION")$set_deps(data.frame(type = "Imports", package = packages, version = "*"))$write()
  
  cat(sprintf("✓ Updated dependencies: %d packages → DESCRIPTION\n", length(packages)))
  cat(sprintf("✓ Project: %s\n", title))
  TRUE
}

# --- Null coalescing ------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
