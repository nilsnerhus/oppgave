#' @title Interactive Qualitative Topic Naming
#' @description Guides researchers through an interactive process of naming topics
#'   based on term lists and representative document excerpts. Supports automatic
#'   naming as a fallback if the interactive process is interrupted.
#'
#' @param model_result Result from fit_model() containing an STM model
#' @param mode Naming mode: "interactive" or "auto" (default: "interactive")
#' @param n_terms Number of terms to display for each ranking method (default: 10)
#' @param n_docs Number of representative documents to display (default: 3)
#' @param doc_length Maximum length of document excerpts to display (default: 300)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item topics_table - Data frame with topic IDs, names, and explanations
#'       \item topic_terms - Top terms for each method
#'       \item topic_examples - Example text snippets for each topic
#'     }
#'   }
#'   \item{metadata}{Process information and configuration}
#'   \item{diagnostics}{Naming issues and statistics}
name_topics <- function(
    model_result,
    mode = "interactive",
    n_terms = 10,
    n_docs = 3,
    doc_length = 300
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    naming_issues = character(),
    example_issues = character(),
    processing_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "name_topics")
  
  # Validate model structure
  if (!is.list(model_result) || !"data" %in% names(model_result) || !"model" %in% names(model_result$data)) {
    error_msg <- "model_result must be the output from fit_model() with a 'model' component"
    diagnostics$naming_issues <- c(diagnostics$naming_issues, error_msg)
    log_message(error_msg, "name_topics", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Validate mode parameter
  valid_modes <- c("auto", "interactive")
  if (!mode %in% valid_modes) {
    warning_msg <- paste("Invalid mode:", mode, "- using 'interactive' instead")
    diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
    log_message(warning_msg, "name_topics", "WARNING")
    mode <- "interactive"
  }
  
  ## --- Extract model and prepare data -----------------------------------------
  log_message("Extracting model and preparing data", "name_topics")
  
  # Extract STM model
  stm_model <- model_result$data$model
  
  # Get topic count
  k <- stm_model$settings$dim$K
  log_message(paste("Processing", k, "topics"), "name_topics")
  
  # Generate different types of topic labels
  log_message("Generating topic labels", "name_topics")
  topic_labels <- stm::labelTopics(stm_model, n = n_terms)
  
  # Try to get representative documents for each topic
  log_message("Finding representative documents", "name_topics")
  topic_docs <- list()
  topic_texts <- list()
  
  # Attempt to retrieve document information
  doc_success <- tryCatch({
    for (i in 1:k) {
      # Get representative documents
      thoughts <- stm::findThoughts(stm_model, texts = stm_model$documents$meta$text,
                                    n = n_docs, topics = i)
      
      # Extract document metadata if available
      meta_info <- NULL
      if (!is.null(stm_model$documents$meta)) {
        doc_indices <- thoughts$index[[1]]
        if (length(doc_indices) > 0) {
          meta_subset <- stm_model$documents$meta[doc_indices, ]
          
          # Try to get country information
          country_col <- intersect(c("country", "country_name", "country_iso3c"), 
                                   names(meta_subset))
          
          if (length(country_col) > 0) {
            meta_info <- meta_subset[, country_col[1], drop = FALSE]
          }
        }
      }
      
      # Store documents and metadata
      topic_docs[[i]] <- meta_info
      topic_texts[[i]] <- thoughts$docs[[1]]
    }
    TRUE
  }, error = function(e) {
    warning_msg <- paste("Could not extract document examples:", e$message)
    diagnostics$example_issues <- c(diagnostics$example_issues, warning_msg)
    log_message(warning_msg, "name_topics", "WARNING")
    FALSE
  })
  
  ## --- Initialize topic names dataframe ---------------------------------------
  log_message("Initializing topic names data frame", "name_topics")
  
  # Initialize dataframe for topic names
  topic_names <- data.frame(
    topic_id = 1:k,
    topic_name = character(k),
    topic_explanation = character(k),
    auto_generated = logical(k),
    stringsAsFactors = FALSE
  )
  
  ## --- Topic review and naming (interactive or automatic) ---------------------
  if (mode == "interactive") {
    log_message("Starting interactive topic naming", "name_topics")
    
    cat("\n=================================================================\n")
    cat("INTERACTIVE TOPIC NAMING\n")
    cat("=================================================================\n")
    cat("For each topic, you'll see term lists and representative documents.\n")
    cat("You'll be asked to provide:\n")
    cat("1. A descriptive name for the topic\n")
    cat("2. A brief explanation of the topic and its conceptual meaning\n")
    cat("\nType 'exit' at any prompt to switch to automatic naming for remaining topics.\n\n")
    
    # Interactive loop for naming topics
    for (i in 1:k) {
      cat("\n=================================================================\n")
      cat(paste0("TOPIC ", i, " OF ", k, "\n"))
      cat("=================================================================\n\n")
      
      # Display term rankings
      cat("FREX terms (frequent and exclusive):\n")
      cat(paste(topic_labels$frex[i,], collapse = ", "), "\n\n")
      
      cat("Highest probability terms:\n")
      cat(paste(topic_labels$prob[i,], collapse = ", "), "\n\n")
      
      cat("Lift terms (distinctive):\n")
      cat(paste(topic_labels$lift[i,], collapse = ", "), "\n\n")
      
      # Display representative documents if available
      if (doc_success && i <= length(topic_texts) && length(topic_texts[[i]]) > 0) {
        cat("REPRESENTATIVE DOCUMENTS:\n")
        
        for (j in 1:length(topic_texts[[i]])) {
          # Get document text and truncate if needed
          doc_text <- topic_texts[[i]][j]
          if (nchar(doc_text) > doc_length) {
            doc_text <- paste0(substr(doc_text, 1, doc_length), "...")
          }
          
          # Display document with metadata if available
          if (!is.null(topic_docs[[i]]) && nrow(topic_docs[[i]]) >= j) {
            meta_str <- paste(names(topic_docs[[i]]), topic_docs[[i]][j,], sep = ": ", collapse = ", ")
            cat(paste0("\nDocument ", j, " (", meta_str, "):\n"))
          } else {
            cat(paste0("\nDocument ", j, ":\n"))
          }
          cat(doc_text, "\n")
        }
        cat("\n")
      }
      
      # Prompt for topic name
      cat("Enter topic name: ")
      topic_name <- readline()
      
      # Check for exit command
      if (tolower(topic_name) %in% c("exit", "quit", "q")) {
        log_message("User exited interactive naming", "name_topics")
        # Switch to auto mode for remaining topics
        log_message(paste("Switching to automatic naming from topic", i), "name_topics")
        
        # Store that this topic was auto-generated
        topic_names$auto_generated[i:k] <- TRUE
        
        # Generate automatic names for remaining topics
        for (j in i:k) {
          # Use first FREX term capitalized followed by next 2-3 terms
          frex_terms <- topic_labels$frex[j,]
          topic_names$topic_name[j] <- paste0(
            toupper(substr(frex_terms[1], 1, 1)),
            substr(frex_terms[1], 2, nchar(frex_terms[1])),
            ": ",
            paste(frex_terms[2:min(4, length(frex_terms))], collapse = ", ")
          )
          
          # Generate basic explanation
          topic_names$topic_explanation[j] <- paste(
            "This topic is characterized by terms related to",
            paste(frex_terms[1:min(5, length(frex_terms))], collapse = ", "),
            "."
          )
        }
        
        # Break out of the loop
        break
      }
      
      # If user entered nothing, generate a default name
      if (topic_name == "") {
        frex_terms <- topic_labels$frex[i,]
        topic_name <- paste0(
          toupper(substr(frex_terms[1], 1, 1)),
          substr(frex_terms[1], 2, nchar(frex_terms[1])),
          ": ",
          paste(frex_terms[2:min(4, length(frex_terms))], collapse = ", ")
        )
        cat(paste0("Using default: '", topic_name, "'\n"))
      }
      
      # Store the topic name
      topic_names$topic_name[i] <- topic_name
      
      # Prompt for explanation
      cat("Enter topic explanation (describing its conceptual meaning): \n")
      topic_explanation <- readline()
      
      # Store explanation (or default if empty)
      if (topic_explanation == "") {
        frex_terms <- topic_labels$frex[i,]
        topic_explanation <- paste(
          "This topic is characterized by terms related to",
          paste(frex_terms[1:min(5, length(frex_terms))], collapse = ", "),
          "."
        )
        cat(paste0("Using default explanation.\n"))
      }
      topic_names$topic_explanation[i] <- topic_explanation
      
      # Mark as manually named
      topic_names$auto_generated[i] <- FALSE
      
      cat("\nTopic", i, "named successfully.\n")
    }
    
  } else {
    # Automatic naming mode
    log_message("Using automatic naming mode", "name_topics")
    
    for (i in 1:k) {
      # Use first FREX term capitalized followed by next 2-3 terms
      frex_terms <- topic_labels$frex[i,]
      topic_names$topic_name[i] <- paste0(
        toupper(substr(frex_terms[1], 1, 1)),
        substr(frex_terms[1], 2, nchar(frex_terms[1])),
        ": ",
        paste(frex_terms[2:min(4, length(frex_terms))], collapse = ", ")
      )
      
      # Generate basic explanation
      topic_names$topic_explanation[i] <- paste(
        "This topic is characterized by terms related to",
        paste(frex_terms[1:min(5, length(frex_terms))], collapse = ", "),
        "."
      )
      
      # Mark as auto-generated
      topic_names$auto_generated[i] <- TRUE
    }
  }
  
  ## --- Prepare result ---------------------------------------------------------
  log_message("Preparing result", "name_topics")
  
  # Create topics table for easy reference
  topics_table <- data.frame(
    topic_id = topic_names$topic_id,
    topic_name = topic_names$topic_name,
    topic_explanation = topic_names$topic_explanation,
    top_frex_terms = sapply(1:k, function(i) paste(topic_labels$frex[i,], collapse = ", ")),
    top_prob_terms = sapply(1:k, function(i) paste(topic_labels$prob[i,], collapse = ", ")),
    auto_generated = topic_names$auto_generated,
    stringsAsFactors = FALSE
  )
  
  # Create examples table if document extraction was successful
  topic_examples <- NULL
  if (doc_success) {
    example_rows <- list()
    
    for (i in 1:k) {
      if (i <= length(topic_texts) && length(topic_texts[[i]]) > 0) {
        for (j in 1:length(topic_texts[[i]])) {
          # Truncate text if needed
          doc_text <- topic_texts[[i]][j]
          if (nchar(doc_text) > doc_length) {
            doc_text <- paste0(substr(doc_text, 1, doc_length), "...")
          }
          
          # Get metadata if available
          meta_info <- if (!is.null(topic_docs[[i]]) && nrow(topic_docs[[i]]) >= j) {
            topic_docs[[i]][j,]
          } else {
            NA
          }
          
          # Create row
          example_rows[[length(example_rows) + 1]] <- list(
            topic_id = i,
            doc_id = j,
            doc_text = doc_text,
            country = if (is.data.frame(meta_info)) as.character(meta_info[1,1]) else NA
          )
        }
      }
    }
    
    # Convert to data frame if we have examples
    if (length(example_rows) > 0) {
      topic_examples <- do.call(rbind, lapply(example_rows, function(x) {
        data.frame(
          topic_id = x$topic_id,
          doc_id = x$doc_id,
          doc_text = x$doc_text,
          country = x$country,
          stringsAsFactors = FALSE
        )
      }))
    }
  }
  
  ## --- Calculate processing time and create result ----------------------------
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    topics_table = topics_table,
    topic_terms = topic_labels,
    topic_examples = topic_examples
  )
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k = k,
    naming_mode = mode,
    manually_named = sum(!topic_names$auto_generated, na.rm = TRUE),
    auto_named = sum(topic_names$auto_generated, na.rm = TRUE),
    success = TRUE
  )
  
  # Update diagnostics
  diagnostics$processing_stats <- list(
    topic_count = k,
    naming_issues_count = length(diagnostics$naming_issues),
    example_issues_count = length(diagnostics$example_issues)
  )
  
  log_message(paste("Topic naming complete for", k, "topics"), "name_topics")
  
  # Return standardized result
  return(create_result(
    data = result_data,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}