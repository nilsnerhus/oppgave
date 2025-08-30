#' @title Topic Naming with OpenAI Integration
#' @description Uses OpenAI GPT models to generate topic names from FREX terms
#'   with automatic fallback to FREX-based labels.
#'   
#' @param model_result Result from fit_model() containing STM model and aligned metadata
#' @param n_terms Number of FREX terms to send to LLM (default: 10)
#' @param openai_model OpenAI model identifier (default: "gpt-3.5-turbo")
#' @param max_retries Maximum number of retry attempts if model fails (default: 3)
#' @param context Optional context string to guide topic naming
#' @param top_countries Number of top countries to include per topic (default: 2)
#'
#' @return A list containing:
#'   \item{data}{Data frame with topic_id, topic_name, frex_terms, top_countries, topic_proportion}
#'   \item{metadata}{Processing information including model used and timing}
#'   \item{diagnostics}{Model call details and any issues encountered}
#'
#' @note Requires OPENAI_API_KEY environment variable to be set.
name_topics <- function(
    model_result,
    n_terms = 10,
    openai_model = "gpt-3.5-turbo", 
    max_retries = 3,
    context = NULL,
    top_countries = 2
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    api_calls = list(),
    processing_issues = character(),
    retry_attempts = 0,
    fallback_used = FALSE
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data and dependencies", "name_topics")
  
  # Check for OpenAI API key
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  if (openai_key == "") {
    log_message("No OPENAI_API_KEY found - will use FREX fallback", "name_topics", "WARNING")
    use_openai <- FALSE
  } else {
    use_openai <- TRUE
  }
  
  # Validate model structure
  if (!is.list(model_result) || !"data" %in% names(model_result) || 
      !all(c("model", "aligned_meta") %in% names(model_result$data))) {
    error_msg <- "model_result must be the output from fit_model() with model and aligned_meta components"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "name_topics", "ERROR")
    stop(error_msg)
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
  
  # Extract FREX terms as list format
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
  log_message("Generating topic names", "name_topics")
  
  topic_names <- character(k)
  
  if (use_openai) {
    log_message("Using OpenAI for topic naming", "name_topics")
    
    for(i in 1:k) {
      retry_count <- 0
      success <- FALSE
      
      while(!success && retry_count < max_retries) {
        retry_count <- retry_count + 1
        
        # Prepare prompt
        terms_text <- paste(head(frex_terms_list[[i]], 3), collapse = ", ")
        prompt <- if(!is.null(context)) {
          paste("Context:", context, "Pick exactly word as a topic label for:", terms_text, "Response format: Word")
        } else {
          paste("Pick exactly 1 word as a topic label for:", terms_text, "Response format: Word1")
        }
        
        # Make API call
        response <- tryCatch({
          httr::POST(
            url = "https://api.openai.com/v1/chat/completions",
            httr::add_headers(
              "Authorization" = paste("Bearer", openai_key),
              "Content-Type" = "application/json"
            ),
            body = jsonlite::toJSON(list(
              model = openai_model,
              messages = list(list(role = "user", content = prompt)),
              max_tokens = 10,
              temperature = 0.1
            ), auto_unbox = TRUE),
            encode = "raw"
          )
        }, error = function(e) NULL)
        
        # Process response
        if(!is.null(response) && httr::status_code(response) == 200) {
          result <- jsonlite::fromJSON(httr::content(response, "text"))
          
          if("choices" %in% names(result) && nrow(result$choices) > 0) {
            message_content <- result$choices$message$content[1]
            # Clean the response (remove numbers, newlines, take first phrase)
            clean_content <- gsub("^\\d+\\.\\s*", "", strsplit(message_content, "\n")[[1]][1])
            label <- trimws(clean_content)
            words <- strsplit(gsub("[^a-zA-Z\\s]", "", label), "\\s+")[[1]]
            topic_names[i] <- paste(head(words[words != ""], 2), collapse = "_")
            success <- TRUE
            
            # Log successful call
            diagnostics$api_calls[[length(diagnostics$api_calls) + 1]] <- list(
              topic = i,
              attempt = retry_count,
              success = TRUE,
              label = topic_names[i]
            )
          }
        }
        
        if(!success) {
          Sys.sleep(2^retry_count)  # Exponential backoff
        }
      }
      
      # Fallback to FREX if all retries failed
      if(!success) {
        clean_terms <- gsub("[^a-zA-Z]", "", frex_terms_list[[i]][1:2])
        topic_names[i] <- paste(stringr::str_to_title(clean_terms), collapse = "_")
        diagnostics$fallback_used <- TRUE
        
        diagnostics$api_calls[[length(diagnostics$api_calls) + 1]] <- list(
          topic = i,
          success = FALSE,
          fallback_label = topic_names[i]
        )
      }
      
      Sys.sleep(0.1)  # Rate limiting
    }
    
    diagnostics$retry_attempts <- max(sapply(diagnostics$api_calls, function(x) x$attempt %||% 0))
    
  } else {
    # FREX fallback for all topics
    log_message("Using FREX terms for all topics (no OpenAI key)", "name_topics")
    for(i in 1:k) {
      clean_terms <- gsub("[^a-zA-Z]", "", frex_terms_list[[i]][1:2])
      topic_names[i] <- paste(stringr::str_to_title(clean_terms), collapse = "_")
    }
    diagnostics$fallback_used <- TRUE
  }
  
  ## --- Validate results -------------------------------------------------------
  invalid_labels <- which(is.na(topic_names) | topic_names == "" | nchar(topic_names) < 2)
  if (length(invalid_labels) > 0) {
    log_message(paste("Fixing", length(invalid_labels), "invalid labels"), "name_topics", "WARNING")
    
    for(idx in invalid_labels) {
      clean_terms <- gsub("[^a-zA-Z]", "", frex_terms_list[[idx]][1:2])
      topic_names[idx] <- paste(stringr::str_to_title(clean_terms), collapse = "_")
    }
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
    openai_model = openai_model,
    n_terms = n_terms,
    context_used = context,
    openai_used = use_openai,
    success = TRUE
  )
  
  log_message(paste("Topic naming complete for", k, "topics"), "name_topics")
  
  # Return standardized result
  return(create_result(
    data = topics_table,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}