#' Load Variables for Inline Text
#' @description Processes topics and metrics data to create inline variables,
#'   INCLUDING pipeline metadata values, for use across thesis chapters
#' @param topics Result object from name_topics pipeline function
#' @param metrics Result object from calculate_metrics pipeline function  
#' @param web Result object from scrape_web pipeline function (optional)
#' @param dfm Result object from process_dfm pipeline function (optional)
#' @param model Result object from fit_model pipeline function (optional)
#' @param digits Number of decimal places for percentages (default: 1)
#' @return Standard pipeline result object with success metadata
load_variables <- function(topics, metrics, web = NULL, dfm = NULL, model = NULL, digits = 2) {
  start_time <- Sys.time()
  
  # Extract data
  topics_table <- topics$data
  metrics_table <- metrics$data
  
  # Initialize variables list
  variables_list <- list()
  
  # =========================================================================
  # FORMATTING FUNCTIONS
  # =========================================================================
  
  # Format percentages (always from 0-1 input)
  pct <- function(x) {
    if (is.na(x) || is.null(x)) return("NA%")
    paste0(round(x * 100, digits), "%")
  }
  
  # Format numbers with consistent spacing and decimal support
  num <- function(x, big.mark = " ", digits = digits) {
    if (is.na(x) || is.null(x)) return("NA")
    if (x < 10000) {
      return(as.character(round(x, digits)))
    } else {
      return(format(round(x, digits), big.mark = big.mark))
    }
  }
  
  # Format strings with consistent style
  string <- function(x, case = "sentence") {
    if (is.na(x) || is.null(x) || x == "") return("")
    
    # Handle lists (comma-separated strings)
    if (grepl(",", x)) {
      items <- trimws(strsplit(x, ",")[[1]])
      
      # Apply case formatting
      if (case == "sentence") {
        items <- suppressWarnings(tools::toTitleCase(tolower(items)))
      }
      
      # Add markdown italics
      items <- paste0("*", items, "*")
      
      # Join with "and" for last item
      if (length(items) > 1) {
        last_item <- items[length(items)]
        other_items <- items[-length(items)]
        return(paste0(paste(other_items, collapse = ", "), " and ", last_item))
      } else {
        return(items)
      }
    } else {
      # Single string - just add italics
      return(paste0("*", x, "*"))
    }
  }
  
  # Add this function near the top of load_variables.R
  format_country_scores <- function(json_string, n_show = 2, as_percentage = TRUE) {
    # Parse JSON
    country_data <- jsonlite::fromJSON(json_string)
    
    if (nrow(country_data) == 0) {
      return("No country data")
    }
    
    # Take only top N countries
    country_data <- head(country_data, n_show)
    
    # Format as percentage or decimal
    if (as_percentage) {
      formatted <- paste0(country_data$country, " (", 
                          round(country_data$score * 100, digits), "%)")
    } else {
      formatted <- paste0(country_data$country, " (", 
                          round(country_data$score, 3), ")")
    }
    
    return(paste(formatted, collapse = ", "))
  }
  
  # In the topics processing loop:
  for (i in 1:nrow(topics_table)) {
    # ... other variables ...
    
    variables_list[[paste0("topic_", i, "_countries")]] <- 
      format_country_scores(topics_table$top_countries[i], 
                            n_show = 2, 
                            as_percentage = TRUE)
  }
  
  # =========================================================================
  # PIPELINE METADATA VARIABLES
  # =========================================================================
  
  log_message("Extracting pipeline metadata values", "load_variables")
  
  # --- Web scraping metadata ---
  if (!is.null(web) && !is.null(web$metadata)) {
    variables_list$n_countries_scraped <- web$metadata$document_count
    variables_list$n_rows_processed <- web$metadata$row_count
    
    # Safe date formatting
    if (!is.null(web$metadata$timestamp)) {
      tryCatch({
        variables_list$scraping_date <- format(as.Date(web$metadata$timestamp), "%B %Y")
      }, error = function(e) {
        variables_list$scraping_date <- as.character(web$metadata$timestamp)
      })
    }
  }
  
  # --- Document processing metadata ---
  if (!is.null(dfm) && !is.null(dfm$metadata)) {
    # Input statistics
    variables_list$n_documents <- num(dfm$metadata$input_documents)
    variables_list$n_tokens_raw <- num(dfm$metadata$input_tokens)
    variables_list$n_vocabulary_raw <- num(dfm$metadata$input_vocabulary)
    
    # Segmentation
    if (!is.null(dfm$metadata$segmentation)) {
      variables_list$n_segments <- num(dfm$metadata$segmentation$segment_count)
      variables_list$avg_segment_length <- num(dfm$metadata$segmentation$avg_segment_length)
      variables_list$min_segment_length <- num(dfm$metadata$segmentation$min_segment_length)
      variables_list$max_segment_length <- num(dfm$metadata$segmentation$max_segment_length)
    }
    
    # Final statistics
    variables_list$n_final_documents <- num(dfm$metadata$final_documents)
    variables_list$n_final_vocabulary <- num(dfm$metadata$final_vocabulary)
    variables_list$n_final_tokens <- num(dfm$metadata$final_tokens)
    
    # Thresholds used
    variables_list$min_docs_threshold <- num(dfm$metadata$min_docs_absolute)
    variables_list$max_docs_threshold <- num(dfm$metadata$max_docs_absolute)
    variables_list$n_custom_stopwords <- num(dfm$metadata$custom_stopwords_count)
  }
  
  # --- Model fitting metadata ---
  if (!is.null(model) && !is.null(model$metadata)) {
    # Basic model parameters
    if (!is.null(model$metadata$k)) {
      variables_list$k_topics <- model$metadata$k
    }
    
    if (!is.null(model$metadata$iterations_run)) {
      variables_list$model_iterations <- model$metadata$iterations_run
    }
    
    # Convergence status
    if (!is.null(model$metadata$converged)) {
      variables_list$model_converged <- ifelse(model$metadata$converged, "converged", "not converged")
    }
    
    # Segmentation used
    if (!is.null(model$metadata$segmentation_used)) {
      variables_list$segmentation_used <- ifelse(model$metadata$segmentation_used, "yes", "no")
    }
    
    # Processing time
    if (!is.null(model$metadata$processing_time_sec) && is.numeric(model$metadata$processing_time_sec)) {
      variables_list$model_runtime_min <- round(model$metadata$processing_time_sec / 60, 1)
    }
  }
  
  if (!is.null(model$data$category_map)) {
    category_map <- model$data$category_map
    
    # Reconstruct the formula (same as build_prevalence_formula does)
    formula_vars <- unlist(category_map)
    variables_list$prevalence_formula <- paste("~", paste(formula_vars, collapse = " + "))
  }
  
  # Document frequency thresholds (as percentages)
  if (!is.null(dfm$metadata$min_doc_freq)) {
    variables_list$min_doc_threshold_pct <- pct(dfm$metadata$min_doc_freq)
  }
  
  if (!is.null(dfm$metadata$max_doc_freq)) {
    variables_list$max_doc_threshold_pct <- pct(dfm$metadata$max_doc_freq)
  }
  
  # Documents after aggregation (from model)
  if (!is.null(model) && !is.null(model$data$aligned_meta)) {
    # Count unique documents after aggregation
    variables_list$n_documents_after_aggregation <- length(unique(model$data$aligned_meta$doc_id))
  }
  
  # =========================================================================
  # TOPIC VARIABLES
  # =========================================================================
  
  if (!is.null(topics_table) && nrow(topics_table) > 0) {
    log_message(paste("Creating variables for", nrow(topics_table), "topics"), "load_variables")
    
    for(i in 1:nrow(topics_table)) {
      topic_name <- paste0("*", suppressWarnings(tools::toTitleCase(tolower(topics_table$topic_name[i]))), "* (Topic ", i, ")")
      variables_list[[paste0("topic_", i, "_name")]] <- topic_name
      variables_list[[paste0("topic_", i, "_prop")]] <- pct(topics_table$topic_proportion[i])
      variables_list[[paste0("topic_", i, "_frex")]] <- string(topics_table$frex_terms[i])
      variables_list[[paste0("topic_", i, "_documents")]] <- num(topics_table$effective_documents[i])
      variables_list[[paste0("topic_", i, "_countries")]] <- 
        format_country_scores(topics_table$top_countries[i], 
                              n_show = 2,  # Only show top 2 countries
                              as_percentage = TRUE)
    }
  }
  
  if (!is.null(topics$metadata$meaningful_threshold)) {
    variables_list$meaningful_threshold <- pct(topics$metadata$meaningful_threshold)
  }
  
  # =========================================================================
  # METRICS VARIABLES
  # =========================================================================
  
  metrics_count <- 0
  
  if (!is.null(metrics_table) && nrow(metrics_table) > 0) {
    log_message("Creating variables for metrics data", "load_variables")
    
    for(i in 1:nrow(metrics_table)) {
      row <- metrics_table[i, ]
      if(is.na(row$effect_size)) next
      
      var_name <- ifelse(row$subcategory == "Overall", row$category, row$subcategory)
      clean_name <- janitor::make_clean_names(var_name)
      
      variables_list[[paste0(clean_name, "_dom")]] <- pct(row$dominance)
      variables_list[[paste0(clean_name, "_topics")]] <- string(row$top_topics)
      
      # Safe rounding for effect size
      if (is.numeric(row$effect_size)) {
        variables_list[[paste0(clean_name, "_effect")]] <- pct(row$effect_size)
      }
      # ADD THIS: Store the abbreviation
      if (!is.null(row$subcategory_abbrev)) {
        variables_list[[paste0(clean_name, "_abbrev")]] <- row$subcategory_abbrev
      }
      
      metrics_count <- metrics_count + 1
    }
  }
  
  if (!is.null(metrics$metadata)) {
    if (!is.null(metrics$metadata$n_value)) {
      variables_list$top_n_topics <- num(metrics$metadata$n_value)  # Number of top topics (3)
    }
  }
  
  # --- Additional metrics metadata ---
  if (!is.null(metrics$metadata)) {
    variables_list$n_categories_tested <- metrics$metadata$categories_tested
    variables_list$n_subcategories_tested <- metrics$metadata$subcategories_tested
    variables_list$n_significant_effects <- metrics$metadata$significant_effects
    
    # Extract min/max effect sizes with safety checks
    effect_sizes <- metrics_table$effect_size[!is.na(metrics_table$effect_size)]
    if (length(effect_sizes) > 0 && all(is.numeric(effect_sizes))) {
      variables_list$min_effect_size <- num(min(effect_sizes), digits = 3)
      variables_list$max_effect_size <- num(max(effect_sizes), digits = 3)
      variables_list$median_effect_size <- num(median(effect_sizes), digits = 3)
    }
  }
  
  # =========================================================================
  # RETURN RESULT
  # =========================================================================
  
  log_message(paste("Created", length(variables_list), "variables"), "load_variables")
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Return standard pipeline result (auto_cache will handle saving)
  return(create_result(
    data = variables_list,
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      topic_variables = ifelse(!is.null(topics_table), nrow(topics_table) * 5, 0),
      metrics_variables = metrics_count * 3,
      total_variables = length(variables_list),
      success = TRUE
    ),
    diagnostics = list()
  ))
}