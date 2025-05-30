#' @title Topic Naming with HuggingFace Integration
#' @description Uses the topiclabels package to generate 2-word topic names from FREX terms
#'   via HuggingFace language models. Simple and effective approach for topic labeling.
#'   
#' @param model_result Result from fit_model() containing STM model and aligned metadata
#' @param n_terms Number of FREX terms to send to LLM (default: 10)
#' @param hf_model HuggingFace model identifier (default: "mistralai/Mixtral-8x7B-Instruct-v0.1")
#' @param max_retries Maximum number of retry attempts if model fails (default: 3)
#' @param context Optional context string to guide topic naming (default: "climate adaptation policy")
#' @param top_countries Number of top countries to include per topic (default: 2)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item topics_table - Data frame with topic_id, topic_name, frex_terms, 
#'                           top_countries, topic_proportion
#'     }
#'   }
#'   \item{metadata}{Processing information including model used and timing}
#'   \item{diagnostics}{Model call details and any issues encountered}
#'
#' @examples
#' \dontrun{
#' # Standard usage (2-word labels from FREX terms)
#' topics <- auto_cache(name_topics, model)
#' 
#' # With custom context
#' topics <- name_topics(model, context = "development finance")
#' 
#' # Access results
#' print(topics$data$topics_table$topic_name)
#' }
#'
#' @note Requires HUGGINGFACE_TOKEN environment variable to be set.
#'   Install topiclabels package: install.packages("topiclabels")
name_topics <- function(
    model_result,
    n_terms = 10,
    hf_model = "mistralai/Mixtral-8x7B-Instruct-v0.1", 
    max_retries = 3,
    context = "climate adaptation policy",
    top_countries = 2,
    max_length_label = 2
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    model_calls = list(),
    processing_issues = character(),
    retry_attempts = 0
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data and dependencies", "name_topics")
  
  # Check for HuggingFace token
  hf_token <- Sys.getenv("HUGGINGFACE_TOKEN")
  if (hf_token == "") {
    error_msg <- "HUGGINGFACE_TOKEN environment variable not set"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "name_topics", "ERROR")
    stop(error_msg)
  }
  
  # Validate model structure
  if (!is.list(model_result) || !"data" %in% names(model_result) || 
      !all(c("model", "aligned_meta") %in% names(model_result$data))) {
    error_msg <- "model_result must be the output from fit_model() with model and aligned_meta components"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "name_topics", "ERROR")
    
    return(create_result(
      data = list(topics_table = NULL),
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Extract model components -----------------------------------------------
  log_message("Extracting STM model components", "name_topics")
  
  stm_model <- model_result$data$model
  theta <- model_result$data$topic_proportions
  meta <- model_result$data$aligned_meta
  
  # Get topic count
  k <- stm_model$settings$dim$K
  log_message(paste("Processing", k, "topics"), "name_topics")
  
  ## --- Extract FREX terms -----------------------------------------------------
  log_message("Extracting FREX terms for naming", "name_topics")
  
  # Generate FREX terms using STM
  topic_labels <- stm::labelTopics(stm_model, n = n_terms)
  
  # Extract FREX terms as list format for topiclabels
  frex_terms_list <- list()
  frex_terms_strings <- character(k)
  
  for (i in 1:k) {
    frex_terms <- topic_labels$frex[i, ]
    frex_terms_list[[i]] <- frex_terms
    frex_terms_strings[i] <- paste(frex_terms, collapse = ", ")
  }
  
  log_message(paste("Extracted", n_terms, "FREX terms per topic"), "name_topics")
  
  ## --- Calculate topic metadata -----------------------------------------------
  log_message("Calculating topic proportions and top countries", "name_topics")
  
  # Calculate topic proportions
  topic_proportions <- colMeans(theta)
  
  # Calculate top countries for each topic
  top_countries_per_topic <- character(k)
  
  if ("country_name" %in% names(meta)) {
    for (i in 1:k) {
      # Get top countries for this topic
      country_scores <- aggregate(theta[, i], 
                                  by = list(country = meta$country_name), 
                                  FUN = mean)
      top_2 <- country_scores[order(country_scores$x, decreasing = TRUE)[1:min(top_countries, nrow(country_scores))], ]
      
      # Format with scores
      country_info <- paste0(top_2$country, " (", round(top_2$x, 3), ")")
      top_countries_per_topic[i] <- paste(country_info, collapse = ", ")
    }
  } else {
    top_countries_per_topic <- rep("No country data available", k)
    log_message("No country_name column found in metadata", "name_topics", "WARNING")
  }
  
  ## --- Generate topic names ---------------------------------------------------
  log_message("Naming topics", "name_topics")
  
  naming_result <- NULL
  retry_count <- 0
  
  while (is.null(naming_result) && retry_count < max_retries) {
    retry_count <- retry_count + 1
    if (retry_count > 1) {
      log_message(paste("Retry attempt", retry_count, "of", max_retries), "name_topics")
    }
    
    tryCatch({
      naming_result <- topiclabels::label_topics(
        terms = frex_terms_list,
        token = hf_token,
        model = hf_model,
        context = context,
        progress = FALSE,
        max_length_label = max_length_label
      )
      
      # Store successful call information
      diagnostics$model_calls[[retry_count]] <- list(
        attempt = retry_count,
        success = TRUE,
        model = hf_model,
        context = context,
        topics_processed = k
      )
      
      log_message("Model call successful", "name_topics")
      
    }, error = function(e) {
      error_msg <- paste("Model call failed on attempt", retry_count, ":", e$message)
      diagnostics$model_calls[[retry_count]] <- list(
        attempt = retry_count,
        success = FALSE,
        error = e$message
      )
      log_message(error_msg, "name_topics", "WARNING")
      
      if (retry_count >= max_retries) {
        stop(paste("All", max_retries, "naming attempts failed. Last error:", e$message))
      }
      
      # Wait before retry (exponential backoff)
      Sys.sleep(2^retry_count)
    })
  }
  
  diagnostics$retry_attempts <- retry_count
  
  ## --- Process results --------------------------------------------------------
  log_message("Processing naming results", "name_topics")
  
  # Extract and clean labels
  topic_names <- trimws(naming_result$labels)
  
  # Simple validation
  invalid_labels <- which(is.na(topic_names) | topic_names == "" | nchar(topic_names) < 2)
  if (length(invalid_labels) > 0) {
    error_msg <- paste("Generated invalid labels for topics:", paste(invalid_labels, collapse = ", "))
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "name_topics", "ERROR")
    stop(error_msg)
  }
  
  log_message(paste("Successfully generated", length(topic_names), "topic labels"), "name_topics")
  
  ## --- Create results data frame ----------------------------------------------
  log_message("Creating final results data frame", "name_topics")
  
  topics_table <- data.frame(
    topic_id = 1:k,
    topic_name = topic_names,
    frex_terms = frex_terms_strings,
    top_countries = top_countries_per_topic,
    topic_proportion = topic_proportions,
    stringsAsFactors = FALSE
  )
  
  ## --- Calculate processing time and create result ----------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k = k,
    hf_model = hf_model,
    n_terms = n_terms,
    context_used = context,
    retry_attempts = retry_count,
    success = TRUE
  )
  
  log_message(paste("Topic naming complete for", k, "topics"), "name_topics")
  
  # Return standardized result
  return(create_result(
    data = list(
      topics_table = topics_table
    ),
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}