#' @title Update Dependencies for CI/CD
#' @description Snapshots renv and generates DESCRIPTION from Quarto metadata + lockfile
#'   
#' @param exclude_packages Packages to exclude from DESCRIPTION (default: "renv")
#' @param quarto_file Path to Quarto project file (default: "_quarto.yml") 
#' @param verbose Show detailed progress messages (default: FALSE)
#' @param overwrite Automatically overwrite existing DESCRIPTION (default: TRUE)
#' @param fallback_title Fallback title if YAML parsing fails
#' @param fallback_author Fallback author if YAML parsing fails
#' 
#' @return Invisible TRUE if successful
update_dependencies <- function(
    exclude_packages = "renv",
    quarto_file = "_quarto.yml",
    verbose = FALSE,
    overwrite = TRUE,
    fallback_title = "Research Project",
    fallback_author = "Author"
) {
  
  # Quiet helper for conditional logging
  qlog <- function(msg, type = "INFO") {
    if (verbose) log_message(msg, "update_dependencies", type)
  }
  
  ## --- Read Quarto metadata with error handling -------------------------------
  title <- fallback_title
  subtitle <- ""
  author <- fallback_author
  
  if (file.exists(quarto_file)) {
    tryCatch({
      # Try to read and parse YAML
      yaml_content <- readLines(quarto_file, warn = FALSE)
      
      # Remove any problematic comments that might break YAML parsing
      yaml_content <- gsub("\\s*#.*$", "", yaml_content)
      
      # Parse the cleaned YAML
      quarto_config <- yaml::yaml.load(paste(yaml_content, collapse = "\n"))
      
      book_info <- quarto_config$book
      if (!is.null(book_info)) {
        title <- book_info$title %||% fallback_title
        subtitle <- book_info$subtitle %||% ""
        author <- book_info$author %||% fallback_author
        qlog("✓ Successfully read Quarto metadata")
      } else {
        qlog("No 'book' section found, using fallbacks")
      }
      
    }, error = function(e) {
      if (verbose) {
        cat("⚠ YAML parsing failed:", e$message, "\n")
        cat("Using fallback values. To debug, check your _quarto.yml syntax.\n")
      } else {
        cat("⚠ YAML parsing failed, using fallback values\n")
      }
    })
  } else {
    qlog(paste("Quarto file not found:", quarto_file))
  }
  
  # Create description
  description <- if (!is.null(subtitle) && subtitle != "") {
    paste0(title, ": ", subtitle)
  } else {
    title
  }
  
  # Generate email
  email <- if (grepl("@", author)) {
    gsub(".*<(.+@.+)>.*", "\\1", author)
  } else {
    paste0(tolower(gsub("[^A-Za-z]", "", author)), "@example.com")
  }
  
  ## --- Snapshot and read lockfile ---------------------------------------------
  qlog("Taking renv snapshot")
  suppressMessages(renv::snapshot())
  
  if (!file.exists("renv.lock")) {
    stop("renv.lock not found. Make sure you're in an renv project.")
  }
  
  lockfile <- renv::lockfile_read("renv.lock")
  packages <- setdiff(names(lockfile$Packages), exclude_packages)
  
  ## --- Install required packages temporarily ----------------------------------
  required_packages <- c("usethis", "desc", "yaml")
  temp_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(temp_packages) > 0) {
    qlog(paste("Installing:", paste(temp_packages, collapse = ", ")))
    suppressMessages(install.packages(temp_packages, quiet = TRUE))
  }
  
  ## --- Create DESCRIPTION -----------------------------------------------------
  package_name <- gsub("[^A-Za-z0-9]", "", gsub("\\s+", "", title))
  if (nchar(package_name) == 0) package_name <- "ResearchProject"
  
  # Handle overwrite quietly
  if (overwrite && file.exists("DESCRIPTION")) {
    file.remove("DESCRIPTION")
  }
  
  # Create DESCRIPTION
  suppressMessages({
    usethis::use_description(fields = list(
      Package = package_name,
      Title = title,
      Description = description,
      `Authors@R` = paste0('person("', author, '", email = "', email, '", role = c("aut", "cre"))'),
      License = "MIT",
      Version = "0.1.0"
    ), check_name = FALSE)
  })
  
  # Add packages
  desc_file <- desc::desc(file = "DESCRIPTION")
  desc_file$set_deps(data.frame(
    type = "Imports",
    package = packages,
    version = "*"
  ))
  desc_file$write()
  
  ## --- Clean up ---------------------------------------------------------------
  if (length(temp_packages) > 0) {
    suppressWarnings(try(remove.packages(temp_packages), silent = TRUE))
  }
  
  ## --- Summary (always show) --------------------------------------------------
  cat(sprintf("✓ Updated dependencies: %d packages from renv.lock → DESCRIPTION\n", length(packages)))
  cat(sprintf("✓ Project: %s\n", title))
  if (title == fallback_title) {
    cat("ℹ Used fallback title (check _quarto.yml syntax if this is wrong)\n")
  }
  cat("✓ Ready to commit and deploy\n")
  
  return(invisible(TRUE))
}

# Helper for null-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a