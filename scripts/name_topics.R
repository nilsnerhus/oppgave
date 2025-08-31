# Convert simple character vector to proper topics structure
name_topics <- function(model, topic_names_vector) {
  
  log_message("Converting topic names to proper structure", "name_topics")
  
  # Extract model components
  stm_model <- model$data$model
  theta <- model$data$topic_proportions
  meta <- model$data$aligned_meta
  k <- stm_model$settings$dim$K
  
  # Validate input
  if (length(topic_names_vector) != k) {
    stop(paste("topic_names_vector must contain exactly", k, "names for", k, "topics"))
  }
  
  # Calculate topic proportions and document spread
  topic_proportions <- colMeans(theta)
  
  # Calculate effective document count (meaningful presence threshold)
  meaningful_threshold <- 0.05  # 5% threshold
  effective_doc_counts <- numeric(k)
  
  for (i in 1:k) {
    effective_doc_counts[i] <- sum(theta[, i] > meaningful_threshold)
  }
  
  # Get FREX terms from model (stored by fit_model)
  if ("frex_terms" %in% names(model$data)) {
    frex_terms_strings <- sapply(model$data$frex_terms, function(x) paste(x[1:5], collapse = ", "))
  } else {
    # Fallback: generate FREX terms
    topic_labels <- stm::labelTopics(stm_model, n = 5)
    frex_terms_strings <- apply(topic_labels$frex, 1, function(x) paste(x[1:5], collapse = ", "))
  }
  
  # Calculate top countries for each topic
  top_countries_per_topic <- character(k)
  if ("country_name" %in% names(meta)) {
    for (i in 1:k) {
      country_scores <- aggregate(theta[, i], 
                                  by = list(country = meta$country_name),
                                  FUN = mean)
      top_2 <- country_scores[order(country_scores$x, decreasing = TRUE)[1:min(2, nrow(country_scores))], ]
      country_info <- paste0(top_2$country, " (", round(top_2$x, 3), ")")
      top_countries_per_topic[i] <- paste(country_info, collapse = ", ")
    }
  } else {
    top_countries_per_topic <- rep("No country data available", k)
  }
  
  # Create the data frame that calculate_metrics expects
  topics_table <- data.frame(
    topic_id = 1:k,
    topic_name = topic_names_vector,
    topic_proportion = topic_proportions,
    frex_terms = frex_terms_strings,
    top_countries = top_countries_per_topic,
    effective_documents = effective_doc_counts,  # NEW: Document spread
    stringsAsFactors = FALSE
  )
  
  log_message(paste("Created topics structure with", k, "topics"), "name_topics")
  
  # Return in the format that calculate_metrics expects
  return(create_result(
    data = topics_table,
    metadata = list(
      timestamp = Sys.time(),
      k = k,
      manual_names_used = TRUE,
      success = TRUE
    ),
    diagnostics = list()
  ))
}