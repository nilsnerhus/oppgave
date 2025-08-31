#' Load Variables for Inline Text
#' @description Processes topics and metrics data to create inline variables,
#'   then saves them to an RDS file for use across thesis chapters
#' @param topics Result object from name_topics pipeline function
#' @param metrics Result object from calculate_metrics pipeline function  
#' @param digits Number of decimal places for percentages (default: 1)
#' @param output_path Path to save variables RDS file (default: "data/inline_variables.rds")
#' @return Standard pipeline result object with success metadata
load_variables <- function(topics, metrics, digits = 1, output_path = "data/inline_variables.rds") {
  start_time <- Sys.time()
  
  # Ensure output directory exists
  ensure_directory(output_path)
  
  # Extract data
  topics_table <- topics$data
  metrics_table <- metrics$data
  
  # Initialize variables list
  variables_list <- list()
  
  # Formatting helpers
  pct <- function(x) paste0(round(x * 100, digits), "%")
  
  num <- function(x) {
    if (x < 10000) {
      return(as.character(round(x)))
    } else {
      return(format(round(x), big.mark = " "))
    }
  }
  
  format_topic_string <- function(topic_string) {
    topics_list <- trimws(strsplit(topic_string, ",")[[1]])
    formatted_topics <- paste0("*", suppressWarnings(tools::toTitleCase(tolower(topics_list))), "*")
    
    if (length(formatted_topics) > 1) {
      last_topic <- formatted_topics[length(formatted_topics)]
      other_topics <- formatted_topics[-length(formatted_topics)]
      return(paste0(paste(other_topics, collapse = ", "), " and ", last_topic))
    } else {
      return(formatted_topics)
    }
  }
  
  # Create topic variables
  log_message(paste("Creating variables for", nrow(topics_table), "topics"), "load_variables")
  
  for(i in 1:nrow(topics_table)) {
    topic_name <- paste0("*", suppressWarnings(tools::toTitleCase(tolower(topics_table$topic_name[i]))), "* (Topic ", i, ")")
    variables_list[[paste0("topic_", i, "_name")]] <- topic_name
    variables_list[[paste0("topic_", i, "_prop")]] <- pct(topics_table$topic_proportion[i])
    variables_list[[paste0("topic_", i, "_frex")]] <- topics_table$frex_terms[i]
    variables_list[[paste0("topic_", i, "_countries")]] <- topics_table$top_countries[i]
  }
  
  # Create metrics variables
  metrics_count <- 0
  log_message("Creating variables for metrics data", "load_variables")
  
  for(i in 1:nrow(metrics_table)) {
    row <- metrics_table[i, ]
    if(is.na(row$effect_size)) next
    
    var_name <- ifelse(row$subcategory == "Overall", row$category, row$subcategory)
    clean_name <- janitor::make_clean_names(var_name)
    
    variables_list[[paste0(clean_name, "_dom")]] <- pct(row$dominance)
    variables_list[[paste0(clean_name, "_topics")]] <- format_topic_string(row$top_topics)
    variables_list[[paste0(clean_name, "_effect")]] <- round(row$effect_size, 4)
    
    metrics_count <- metrics_count + 1
  }
  
  # Save variables to RDS file
  log_message(paste("Saving", length(variables_list), "variables to", output_path), "load_variables")
  saveRDS(variables_list, output_path)
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Return standard pipeline result
  return(create_result(
    data = variables_list,
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      topic_variables = nrow(topics_table) * 4,
      metrics_variables = metrics_count * 3,
      total_variables = length(variables_list),
      output_file = output_path,
      success = TRUE
    ),
    diagnostics = list()
  ))
}