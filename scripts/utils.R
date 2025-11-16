# ===============================
# Utility functions for NAP analysis (leaned-down)
# ===============================

# --- Helpers -----------------------------------------------------

ensure_directory <- function(path) {
	if (is.null(path)) {
		return(invisible())
	}
	dir <- dirname(path)
	if (!dir.exists(dir)) {
		dir.create(dir, recursive = TRUE, showWarnings = FALSE)
	}
}

get_timestamp <- function() format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")

log_message <- function(msg, func = NULL, type = "INFO") {
	prefix <- if (!is.null(func)) paste0("[", func, "] ") else ""
	message(get_timestamp(), " ", prefix, type, ": ", msg)
}

# --- Validation --------------------------------------------------

validate_input <- function(
	input_data,
	required_cols = NULL,
	func_name = "function"
) {
	if (!is.data.frame(input_data) && !is.list(input_data)) {
		stop(func_name, ": Input must be a data frame or list")
	}

	if (is.data.frame(input_data)) {
		if (nrow(input_data) == 0) {
			stop(func_name, ": Input has no rows")
		}
		if (!is.null(required_cols)) {
			missing_cols <- setdiff(
				required_cols,
				names(input_data)
			)
			if (length(missing_cols) > 0) {
				stop(
					func_name,
					": Missing columns: ",
					paste(missing_cols, collapse = ", ")
				)
			}
		}
	}

	if (
		is.list(input_data) &&
			!is.data.frame(input_data) &&
			!is.null(required_cols)
	) {
		missing_elements <- setdiff(required_cols, names(input_data))
		if (length(missing_elements) > 0) {
			stop(
				func_name,
				": Missing elements: ",
				paste(missing_elements, collapse = ", ")
			)
		}
	}

	TRUE
}

# --- Result structure -------------------------------------------

create_result <- function(
	data = NULL,
	metadata = list(),
	diagnostics = list()
) {
	list(
		data = data,
		metadata = c(
			list(
				timestamp = Sys.time(),
				r_version = R.version.string,
				session_info = sessionInfo()$platform
			),
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
	message(
		get_timestamp(),
		" ",
		prefix,
		"Operation completed in ",
		runtime,
		" seconds"
	)
	result
}

# --- Dual save (RDS + JSON) --------------------------------------

save_dual <- function(object, path, hash = NULL) {
	saveRDS(object, path)
	json_path <- sub("\\.rds$", ".json", path)
	tryCatch(
		jsonlite::write_json(
			object,
			json_path,
			pretty = TRUE,
			auto_unbox = TRUE
		),
		error = function(e) {
			message(
				"JSON export skipped for ",
				path,
				": ",
				e$message
			)
		}
	)
	if (!is.null(hash)) writeLines(hash, paste0(path, ".hash"))
}

# --- Automatic caching ------------------------------------------

auto_cache <- function(func, ..., cache_path = NULL, overwrite = FALSE) {
	func_name <- deparse(substitute(func))
	cache_path <- cache_path %||%
		file.path("data", paste0(func_name, ".rds"))

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
		log_message(
			paste("Using cached result from", basename(cache_path)),
			"auto_cache"
		)
		return(readRDS(cache_path))
	}

	log_message(
		if (overwrite && file.exists(cache_path)) {
			paste("Overwriting cache for", basename(cache_path))
		} else {
			paste("Computing new result for", basename(cache_path))
		},
		"auto_cache"
	)

	result <- do.call(func, args)
	save_dual(result, cache_path, current_hash)
	result
}

rds_files <- list.files("data", pattern = "\\.rds$", full.names = TRUE)
for (f in rds_files) {
	obj <- readRDS(f)
	save_dual(obj, f) # writes .json alongside .rds
}

# --- Web caching -----------------------------------------------

web_cache <- function(
	func,
	...,
	url = "https://napcentral.org/submitted-naps",
	cache_path = NULL,
	table_index = 1,
	overwrite = FALSE
) {
	func_name <- deparse(substitute(func))
	cache_path <- cache_path %||%
		file.path("data", paste0(func_name, ".rds"))

	if (!file.exists(cache_path) || overwrite) {
		log_message(
			if (overwrite) {
				"Overwrite requested, fetching fresh data"
			} else {
				"No cache found, fetching data"
			},
			"web_cache"
		)
		result <- do.call(func, list(...))
		save_dual(result, cache_path)
		return(result)
	}

	size_path <- paste0(cache_path, ".size")
	content_changed <- FALSE

	tryCatch(
		{
			session <- polite::bow(url)
			tables <- rvest::html_nodes(
				polite::scrape(session),
				"table"
			)

			if (length(tables) >= table_index) {
				rows <- rvest::html_nodes(
					tables[[table_index]],
					"tr"
				)
				signature <- digest::digest(sapply(
					rows,
					rvest::html_text
				))
				current <- c(length(rows), signature)

				if (file.exists(size_path)) {
					prev <- readLines(
						size_path,
						warn = FALSE
					)
					if (!identical(current, prev)) {
						content_changed <- TRUE
						log_message(
							"Web content has changed",
							"web_cache"
						)
					}
				} else {
					content_changed <- TRUE
				}

				writeLines(current, size_path)
			} else {
				content_changed <- TRUE
			}
		},
		error = function(e) {
			log_message(
				paste("Error checking web content:", e$message),
				"web_cache",
				"WARNING"
			)
			content_changed <- TRUE
		}
	)

	if (content_changed) {
		result <- do.call(func, list(...))
		save_dual(result, cache_path)
		return(result)
	}

	log_message("Web content unchanged, using cached data", "web_cache")
	readRDS(cache_path)
}

# --- Thesis page counter ----------------------------------------

check_thesis <- function(
	path = "_book/text/",
	per_chapter = TRUE,
	exclude_refs = TRUE
) {
	# Constants
	CHARS_PER_PAGE <- 2400
	MIN_PAGES <- 60
	MAX_PAGES <- 80

	# Get HTML files
	if (grepl("\\.html$", path)) {
		files <- path
		per_chapter <- FALSE # Single file, no chapter view
	} else {
		files <- list.files(
			path,
			"\\.html$",
			recursive = TRUE,
			full.names = TRUE
		)
	}

	if (exclude_refs) {
		files <- files[
			!grepl("(references|bibliography)\\.html$", files)
		]
	}

	# Check if any files were found
	if (length(files) == 0) {
		cat("\n⚠️  No HTML files found in path:", path, "\n")
		cat("   Make sure you've built your Quarto book first.\n")
		cat("   Try running: quarto render\n")
		return(invisible(NULL))
	}

	# Process each file
	chapter_data <- list()

	for (file in files) {
		# Check if file exists
		if (!file.exists(file)) {
			cat("Warning: File not found -", file, "\n")
			next
		}

		tryCatch(
			{
				html <- read_html(file)

				# Get all content sections
				sections <- html_nodes(
					html,
					"section.level2, section.level1"
				)
				main_content <- html_nodes(html, "main")

				if (length(main_content) > 0) {
					text <- html_text2(main_content)
				} else if (length(sections) > 0) {
					text <- html_text2(sections) %>%
						paste(collapse = " ")
				} else {
					body <- html_node(html, "body")
					text <- html_text2(body)
				}

				# Clean text
				text <- trimws(gsub("\\s+", " ", text))

				# Only store if we got some text
				if (nchar(text) > 0) {
					chapter_name <- gsub(
						"\\.html$",
						"",
						basename(file)
					)
					chapter_data[[chapter_name]] <- list(
						chars = nchar(text),
						pages = nchar(text) /
							CHARS_PER_PAGE,
						words = length(strsplit(
							text,
							"\\s+"
						)[[1]])
					)
				}
			},
			error = function(e) {
				cat(
					"Warning: Error processing",
					basename(file),
					"-",
					e$message,
					"\n"
				)
			}
		)
	}

	# Check if we have any data
	if (length(chapter_data) == 0) {
		cat("\n⚠️  No content could be extracted from HTML files.\n")
		cat("   This might happen if:\n")
		cat("   - The HTML files are empty\n")
		cat("   - The HTML structure is unexpected\n")
		cat("   - There was an error reading the files\n")
		return(invisible(NULL))
	}

	# Calculate totals (now safe because we know chapter_data has content)
	total_chars <- sum(sapply(chapter_data, function(x) x$chars))
	total_pages <- total_chars / CHARS_PER_PAGE
	total_words <- sum(sapply(chapter_data, function(x) x$words))

	# Print header
	cat("\n══════════════════════════════════════════════\n")
	cat("         THESIS CHARACTER COUNT\n")
	cat("══════════════════════════════════════════════\n")

	# Print chapter breakdown if requested
	if (per_chapter && length(chapter_data) > 1) {
		cat("\n📚 CHAPTER BREAKDOWN:\n")
		cat("──────────────────────────────────────────────\n")

		# Sort by page count
		sorted_chapters <- chapter_data[order(
			sapply(chapter_data, function(x) x$pages),
			decreasing = TRUE
		)]

		# Print each chapter
		for (name in names(sorted_chapters)) {
			ch <- sorted_chapters[[name]]
			# Create a mini progress bar
			bar_size <- round(
				(ch$pages /
					max(sapply(chapter_data, function(x) {
						x$pages
					}))) *
					20
			)
			bar <- paste0(rep("▓", bar_size), collapse = "")
			bar <- paste0(
				bar,
				paste0(rep("░", 20 - bar_size), collapse = "")
			)

			cat(sprintf(
				"%-20s %s %.1f pages\n",
				substr(name, 1, 20),
				bar,
				ch$pages
			))
		}
		cat("──────────────────────────────────────────────\n")
	}

	# Print totals
	cat("\n📊 TOTALS:\n")
	cat("──────────────────────────────────────────────\n")
	cat(sprintf(
		"📄 Pages:      %.1f / %d-%d\n",
		total_pages,
		MIN_PAGES,
		MAX_PAGES
	))
	cat(sprintf("📝 Characters: %s\n", format(total_chars, big.mark = " ")))
	cat(sprintf("💬 Words:      %s\n", format(total_words, big.mark = " ")))
	cat(sprintf("📁 Files:      %d chapters\n", length(chapter_data)))

	# Progress bar
	cat("\nProgress: [")
	filled <- round((total_pages / MAX_PAGES) * 30)
	min_mark <- round((MIN_PAGES / MAX_PAGES) * 30)
	for (i in 1:30) {
		if (i == min_mark) {
			cat("|")
		} else if (i <= filled) {
			cat("█")
		} else {
			cat("░")
		}
	}
	cat("] ", sprintf("%.0f%%\n", (total_pages / MIN_PAGES) * 100))

	# Status
	if (total_pages < MIN_PAGES) {
		cat(sprintf(
			"\n⚠️  Need %.1f more pages (%.0f chars)\n",
			MIN_PAGES - total_pages,
			(MIN_PAGES - total_pages) * CHARS_PER_PAGE
		))
	} else if (total_pages > MAX_PAGES) {
		cat(sprintf(
			"\n⚠️  %.1f pages over maximum!\n",
			total_pages - MAX_PAGES
		))
	} else {
		cat(sprintf(
			"\n✅ Within range! %.1f pages to max\n",
			MAX_PAGES - total_pages
		))
	}

	cat("══════════════════════════════════════════════\n")

	# Return data invisibly
	invisible(list(
		total = list(
			pages = round(total_pages, 1),
			chars = total_chars,
			words = total_words
		),
		chapters = chapter_data
	))
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
	qlog <- function(msg, type = "INFO") {
		if (verbose) log_message(msg, "update_dependencies", type)
	}

	# Read Quarto metadata
	title <- fallback_title
	subtitle <- ""
	author <- fallback_author
	email <- paste0(
		tolower(gsub("[^A-Za-z]", "", fallback_author)),
		"@example.com"
	)

	if (file.exists(quarto_file)) {
		try(
			{
				yaml_content <- gsub(
					"\\s*#.*$",
					"",
					readLines(quarto_file, warn = FALSE)
				)
				conf <- yaml::yaml.load(paste(
					yaml_content,
					collapse = "\n"
				))
				book <- conf$book
				if (!is.null(book)) {
					title <- book$title %||% fallback_title
					subtitle <- book$subtitle %||% ""
					auth <- book$author %||% fallback_author
					if (is.list(auth)) {
						author <- auth$name %||%
							fallback_author
						email <- auth$email %||% email
					} else {
						author <- auth
						if (grepl("@", auth)) {
							email <- gsub(
								".*<(.+@.+)>.*",
								"\\1",
								auth
							)
						}
					}
					qlog("✓ Quarto metadata loaded")
				}
			},
			silent = TRUE
		)
	}

	description <- if (subtitle != "") {
		paste(title, subtitle, sep = ": ")
	} else {
		title
	}
	suppressMessages(renv::snapshot())
	if (!file.exists("renv.lock")) {
		stop("renv.lock not found")
	}

	lockfile <- renv::lockfile_read("renv.lock")
	packages <- setdiff(names(lockfile$Packages), exclude_packages)

	# Create DESCRIPTION
	package_name <- gsub("[^A-Za-z0-9]", "", gsub("\\s+", "", title))
	if (nchar(package_name) == 0) {
		package_name <- "ResearchProject"
	}

	if (overwrite && file.exists("DESCRIPTION")) {
		file.remove("DESCRIPTION")
	}

	usethis::use_description(
		fields = list(
			Package = package_name,
			Title = title,
			Description = description,
			`Authors@R` = paste0(
				'person("',
				author,
				'", email = "',
				email,
				'", role = c("aut", "cre"))'
			),
			License = "MIT",
			Version = "0.1.0"
		),
		check_name = FALSE
	)

	desc::desc(file = "DESCRIPTION")$set_deps(data.frame(
		type = "Imports",
		package = packages,
		version = "*"
	))$write()

	cat(sprintf(
		"✓ Updated dependencies: %d packages → DESCRIPTION\n",
		length(packages)
	))
	cat(sprintf("✓ Project: %s\n", title))
	TRUE
}

# --- Null coalescing ------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
