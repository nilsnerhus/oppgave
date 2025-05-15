#' @title Extract top topics for document groups
#' @description Identifies the most prevalent topics within a group of documents
#'   and formats them as a string with proportions. Used to enhance dominance tables
#'   with information about which topics dominate in each category.
#'   
#' @param data Data frame containing document-topic proportions
#' @param group_filter Optional logical vector to filter rows in data
#' @param group_col Optional column name to use for filtering by group
#' @param group_value Optional value to match in group_col for filtering
#' @param n_topics Number of top topics to include (default: 3)
#' @param topic_names Optional data frame with topic_id and short_name columns for labeling
#'   
#' @return A formatted string listing the top topics with their proportions
get_top_topics <- function(data, group_filter = NULL, group_col = NULL, 
                           group_value = NULL, n_topics = 3, topic_names = NULL) {
  # Filter data if needed
  if (!is.null(group_filter)) {
    data <- data[group_filter, ]
  } else if (!is.null(group_col) && !is.null(group_value)) {
    data <- data[data[[group_col]] == group_value, ]
  }
  
  # Get topic columns
  topic_cols <- grep("^Topic_", names(data), value = TRUE)
  
  # Skip if no topics found
  if (length(topic_cols) == 0) {
    return("No topics found")
  }
  
  # Average topic proportions
  topic_means <- colMeans(data[, topic_cols, drop = FALSE], na.rm = TRUE)
  
  # Sort and get top n
  top_indices <- order(topic_means, decreasing = TRUE)[1:min(n_topics, length(topic_means))]
  top_topic_ids <- as.numeric(gsub("Topic_", "", topic_cols[top_indices]))
  top_proportions <- topic_means[top_indices]
  
  # Format result
  result <- data.frame(
    topic_id = top_topic_ids,
    proportion = top_proportions,
    stringsAsFactors = FALSE
  )
  
  # Add names if provided
  if (!is.null(topic_names)) {
    result <- merge(result, topic_names[, c("topic_id", "short_name")], by = "topic_id")
    result <- result[order(result$proportion, decreasing = TRUE), ]
  }
  
  # Format as string
  if (!is.null(topic_names) && nrow(result) > 0 && "short_name" %in% names(result)) {
    result_str <- paste(paste0(result$short_name, " (", round(result$proportion * 100, 1), "%)"), 
                        collapse = ", ")
  } else {
    result_str <- paste(paste0("Topic ", result$topic_id, " (", round(result$proportion * 100, 1), "%)"), 
                        collapse = ", ")
  }
  
  return(result_str)
}