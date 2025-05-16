#' @title Name topics with automatic or interactive labeling
#' @description Assigns meaningful names to topics based on word distributions.
#'   Works in either automated mode (generating names from top terms) or interactive
#'   mode (prompting user for names). Provides example text to aid interpretation.
#'   
#' @param model Result from fit_model() containing topic information
#' @param mode Naming mode: "auto" or "interactive" (default: "auto")
#' @param method Method for auto-naming: "frex", "prob", "lift", "score" (default: "frex")
#' @param include_examples Whether to extract example text for topics (default: TRUE)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item topic_names - Dataframe with topic_id, topic_name, and short_name
#'       \item topic_terms - Top terms for each naming method
#'       \item topic_examples - Example text snippets for each topic (if requested)
#'     }
#'   }
#'   \item{metadata}{Process information and configuration}
#'   \item{diagnostics}{Naming issues and statistics}
#'
#' @examples
#' \dontrun{
#' # Automatic naming based on FREX terms
#' topic_names <- name_topics(model)
#' 
#' # Interactive naming with user input
#' topic_names <- name_topics(model, mode = "interactive")
#' 
#' # Automatic naming using probability method without examples
#' topic_names <- name_topics(model, method = "prob", include_examples = FALSE)
#' }
name_topics <- function(
    model,
    mode = "auto",           # "auto" or "interactive"
    method = "frex",         # "frex", "prob", "lift", "score"
    include_examples = TRUE  # Whether to extract example text
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    naming_issues = character(),
    example_issues = character(),
    processing_stats = list()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "name_topics")
  
  # Validate model structure
  if (!is.list(model) || !"data" %in% names(model)) {
    error_msg <- "model must be the output from fit_model() with a 'data' component"
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
  
  # Check for required model components
  if (!("topic_terms" %in% names(model$data))) {
    error_msg <- "Model missing topic_terms component"
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
    warning_msg <- paste("Invalid mode:", mode, "- using 'auto' instead")
    diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
    log_message(warning_msg, "name_topics", "WARNING")
    mode <- "auto"
  }
  
  # Validate method parameter
  valid_methods <- c("frex", "prob", "lift", "score")
  if (!method %in% valid_methods) {
    warning_msg <- paste("Invalid method:", method, "- using 'frex' instead")
    diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
    log_message(warning_msg, "name_topics", "WARNING")
    method <- "frex"
  }
  
  # Validate include_examples parameter
  if (!is.logical(include_examples)) {
    warning_msg <- "include_examples must be logical - using TRUE instead"
    diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
    log_message(warning_msg, "name_topics", "WARNING")
    include_examples <- TRUE
  }
  
  ## --- Extract topic information ----------------------------------------------
  log_message("Extracting topic information", "name_topics")
  
  # Get topic terms
  topic_terms <- model$data$topic_terms
  
  # Try both possible structures for topic terms
  topic_label_summary <- NULL
  
  if ("label_summary" %in% names(topic_terms)) {
    topic_label_summary <- topic_terms$label_summary
  } else if ("all_labels" %in% names(topic_terms)) {
    # Reshape from long to wide format
    label_types <- unique(topic_terms$all_labels$label_type)
    topic_ids <- unique(topic_terms$all_labels$topic_id)
    
    topic_label_summary <- data.frame(topic_id = topic_ids)
    
    for (lt in label_types) {
      lt_data <- topic_terms$all_labels[topic_terms$all_labels$label_type == lt, ]
      lt_data <- lt_data[match(topic_ids, lt_data$topic_id), ]
      topic_label_summary[[paste0(lt, "_words")]] <- lt_data$words
    }
  } else {
    # Try direct access
    topic_label_summary <- topic_terms
  }
  
  # Check we have a valid topic_label_summary
  if (is.null(topic_label_summary) || nrow(topic_label_summary) == 0) {
    error_msg <- "Could not extract topic terms from model"
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
  
  # Get topic count
  k <- nrow(topic_label_summary)
  log_message(paste("Processing", k, "topics"), "name_topics")
  
  ## --- Extract example documents (if requested) -------------------------------
  topic_examples <- NULL
  
  if (include_examples) {
    log_message("Extracting example text for topics", "name_topics")
    
    # Check for required components
    if (!all(c("topic_proportions", "topic_data") %in% names(model$data))) {
      warning_msg <- "Cannot extract examples: missing topic_proportions or topic_data"
      diagnostics$example_issues <- c(diagnostics$example_issues, warning_msg)
      log_message(warning_msg, "name_topics", "WARNING")
    } else {
      # Get topic proportions and data
      topic_props <- model$data$topic_proportions
      topic_data <- model$data$topic_data
      
      # Initialize examples dataframe
      topic_examples <- data.frame(
        topic_id = 1:k,
        doc_id = character(k),
        example_text = character(k),
        topic_proportion = numeric(k),
        stringsAsFactors = FALSE
      )
      
      # Get original document text if available
      original_docs <- NULL
      if ("segments" %in% names(model$data) && !is.null(model$data$segments)) {
        # Try to get original text via segments info
        segments <- model$data$segments$mapping
        if (is.data.frame(segments) && "original_doc_id" %in% names(segments)) {
          # Look for original document text
          if (exists("nap_data") && is.list(nap_data)) {
            if (is.list(nap_data$data) && "documents" %in% names(nap_data$data)) {
              original_docs <- nap_data$data$documents
            }
          }
        }
      }
      
      # Fallback - use topic_props if it has a text column
      if (is.null(original_docs) && "text" %in% names(topic_props)) {
        original_docs <- topic_props[, c("doc_id", "text")]
      }
      
      # Process each topic
      for (i in 1:k) {
        topic_col <- paste0("Topic_", i)
        
        # Skip if topic column missing
        if (!topic_col %in% names(topic_props)) {
          warning_msg <- paste("Topic column", topic_col, "not found in topic_proportions")
          diagnostics$example_issues <- c(diagnostics$example_issues, warning_msg)
          next
        }
        
        # Find document with highest proportion of this topic
        best_doc_idx <- which.max(topic_props[[topic_col]])
        
        if (length(best_doc_idx) == 0 || is.na(best_doc_idx)) {
          warning_msg <- paste("Could not find representative document for topic", i)
          diagnostics$example_issues <- c(diagnostics$example_issues, warning_msg)
          next
        }
        
        best_doc_id <- topic_props$doc_id[best_doc_idx]
        best_prop <- topic_props[[topic_col]][best_doc_idx]
        
        # Try to get example text
        example_text <- ""
        
        if (!is.null(original_docs)) {
          # Get text from original docs
          doc_text <- original_docs$text[original_docs$doc_id == best_doc_id]
          
          if (length(doc_text) > 0 && !is.na(doc_text[1])) {
            # Extract a snippet (first 200 characters)
            example_text <- substr(doc_text[1], 1, 200)
            
            # Add ellipsis if shortened
            if (nchar(doc_text[1]) > 200) {
              example_text <- paste0(example_text, "...")
            }
          }
        }
        
        # Store in examples dataframe
        topic_examples$doc_id[i] <- best_doc_id
        topic_examples$example_text[i] <- example_text
        topic_examples$topic_proportion[i] <- best_prop
      }
    }
  }
  
  ## --- Generate topic names ---------------------------------------------------
  log_message(paste("Generating topic names using", mode, "mode"), "name_topics")
  
  # Initialize topic names dataframe
  topic_names <- data.frame(
    topic_id = 1:k,
    topic_name = character(k),
    short_name = character(k),
    stringsAsFactors = FALSE
  )
  
  if (mode == "auto") {
    # Generate names automatically based on top terms
    log_message(paste("Using", method, "method for automatic naming"), "name_topics")
    
    # Choose the words column based on method
    words_col <- paste0(method, "_words")
    
    if (!words_col %in% names(topic_label_summary)) {
      warning_msg <- paste("Method", method, "not available, falling back to first available")
      diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
      log_message(warning_msg, "name_topics", "WARNING")
      
      # Use first words column available
      words_col <- grep("_words$", names(topic_label_summary), value = TRUE)[1]
    }
    
    # Generate names for each topic
    for (i in 1:k) {
      # Get words for this topic
      words <- topic_label_summary[[words_col]][i]
      
      # Split into array
      word_array <- unlist(strsplit(words, ", "))
      
      # Generate descriptive name (first 3-4 words)
      topic_words <- word_array[1:min(4, length(word_array))]
      descriptive_name <- paste0(
        toupper(substr(topic_words[1], 1, 1)),
        substr(topic_words[1], 2, nchar(topic_words[1])),
        ": ",
        paste(topic_words[2:length(topic_words)], collapse = ", ")
      )
      
      # Generate short name (first word or acronym)
      short_name <- word_array[1]
      
      # Store names
      topic_names$topic_name[i] <- descriptive_name
      topic_names$short_name[i] <- short_name
    }
  } else {
    # Interactive naming
    cat("\nInteractive Topic Naming\n")
    cat("=======================\n")
    cat("For each topic, you'll see the top words and be prompted to enter:\n")
    cat("1. A descriptive name (e.g., 'Climate Finance')\n")
    cat("2. A short name/abbreviation (e.g., 'Finance')\n\n")
    cat("To exit naming early, type 'exit' or 'quit' at any prompt.\n\n")
    
    for (i in 1:k) {
      cat(paste0("\n------ Topic ", i, " of ", k, " ------\n"))
      
      # Display top words from each method
      if ("prob_words" %in% names(topic_label_summary)) {
        cat(paste0("Probability: ", topic_label_summary$prob_words[i], "\n"))
      }
      if ("frex_words" %in% names(topic_label_summary)) {
        cat(paste0("FREX: ", topic_label_summary$frex_words[i], "\n"))
      }
      if ("lift_words" %in% names(topic_label_summary)) {
        cat(paste0("Lift: ", topic_label_summary$lift_words[i], "\n"))
      }
      if ("score_words" %in% names(topic_label_summary)) {
        cat(paste0("Score: ", topic_label_summary$score_words[i], "\n"))
      }
      
      # Display example text if available
      if (!is.null(topic_examples) && !is.na(topic_examples$example_text[i]) && 
          nchar(topic_examples$example_text[i]) > 0) {
        cat("\nExample text:\n")
        cat(paste0("\"", topic_examples$example_text[i], "\"\n"))
        cat(paste0("(Document: ", topic_examples$doc_id[i], ", Topic proportion: ", 
                   round(topic_examples$topic_proportion[i] * 100, 1), "%)\n\n"))
      } else {
        cat("\n")
      }
      
      # Prompt for topic name
      cat("Enter descriptive name (or 'exit' to quit): ")
      topic_name <- readline()
      
      # Check for exit command
      if (tolower(topic_name) %in% c("exit", "quit", "q")) {
        log_message("User exited interactive naming", "name_topics")
        # Fill remaining topics with automatic names
        if (i < k) {
          warning_msg <- paste("Interactive naming exited early at topic", i, "- using auto names for remaining topics")
          diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
          log_message(warning_msg, "name_topics", "WARNING")
          
          # Switch to auto mode for remaining topics
          # Get words column based on method parameter
          words_col <- paste0(method, "_words")
          if (!words_col %in% names(topic_label_summary)) {
            words_col <- grep("_words$", names(topic_label_summary), value = TRUE)[1]
          }
          
          for (j in i:k) {
            # Get words for this topic
            words <- topic_label_summary[[words_col]][j]
            
            # Split into array
            word_array <- unlist(strsplit(words, ", "))
            
            # Generate names
            topic_words <- word_array[1:min(4, length(word_array))]
            descriptive_name <- paste0(
              toupper(substr(topic_words[1], 1, 1)),
              substr(topic_words[1], 2, nchar(topic_words[1])),
              ": ",
              paste(topic_words[2:length(topic_words)], collapse = ", ")
            )
            
            topic_names$topic_name[j] <- descriptive_name
            topic_names$short_name[j] <- word_array[1]
          }
          
          break
        }
      }
      
      # If user entered nothing, use default
      if (topic_name == "") {
        # Generate default from top words
        words_col <- paste0(method, "_words")
        if (!words_col %in% names(topic_label_summary)) {
          words_col <- grep("_words$", names(topic_label_summary), value = TRUE)[1]
        }
        
        words <- topic_label_summary[[words_col]][i]
        word_array <- unlist(strsplit(words, ", "))
        topic_words <- word_array[1:min(4, length(word_array))]
        
        topic_name <- paste0(
          toupper(substr(topic_words[1], 1, 1)),
          substr(topic_words[1], 2, nchar(topic_words[1])),
          ": ",
          paste(topic_words[2:length(topic_words)], collapse = ", ")
        )
        
        cat(paste0("Using default: '", topic_name, "'\n"))
      }
      
      # Prompt for short name
      cat("Enter short name/abbreviation: ")
      short_name <- readline()
      
      # Check for exit command again
      if (tolower(short_name) %in% c("exit", "quit", "q")) {
        log_message("User exited interactive naming", "name_topics")
        # Use topic name first word as short name and continue exiting
        short_name <- unlist(strsplit(topic_name, " "))[1]
        cat(paste0("Using default: '", short_name, "'\n"))
        
        # Fill remaining topics with automatic names
        if (i < k) {
          warning_msg <- paste("Interactive naming exited early at topic", i, "- using auto names for remaining topics")
          diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
          log_message(warning_msg, "name_topics", "WARNING")
          
          # Save this topic
          topic_names$topic_name[i] <- topic_name
          topic_names$short_name[i] <- short_name
          
          # Switch to auto mode for remaining topics
          words_col <- paste0(method, "_words")
          if (!words_col %in% names(topic_label_summary)) {
            words_col <- grep("_words$", names(topic_label_summary), value = TRUE)[1]
          }
          
          for (j in (i+1):k) {
            # Get words for this topic
            words <- topic_label_summary[[words_col]][j]
            
            # Split into array
            word_array <- unlist(strsplit(words, ", "))
            
            # Generate names
            topic_words <- word_array[1:min(4, length(word_array))]
            descriptive_name <- paste0(
              toupper(substr(topic_words[1], 1, 1)),
              substr(topic_words[1], 2, nchar(topic_words[1])),
              ": ",
              paste(topic_words[2:length(topic_words)], collapse = ", ")
            )
            
            topic_names$topic_name[j] <- descriptive_name
            topic_names$short_name[j] <- word_array[1]
          }
          
          break
        }
      }
      
      # If user entered nothing, use default
      if (short_name == "") {
        # Extract first word of topic name as default short name
        first_word <- unlist(strsplit(topic_name, " "))[1]
        first_word <- gsub(":", "", first_word)  # Remove colon if present
        short_name <- first_word
        cat(paste0("Using default: '", short_name, "'\n"))
      }
      
      # Store in data frame
      topic_names$topic_name[i] <- topic_name
      topic_names$short_name[i] <- short_name
    }
    
    # Summarize results
    cat("\nTopic naming complete!\n")
  }
  
  ## --- Validate final topic names ---------------------------------------------
  log_message("Validating final topic names", "name_topics")
  
  # Check for empty names
  empty_names <- which(topic_names$topic_name == "")
  if (length(empty_names) > 0) {
    warning_msg <- paste("Found", length(empty_names), "empty topic names - using defaults")
    diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
    log_message(warning_msg, "name_topics", "WARNING")
    
    # Generate default names for empty entries
    for (i in empty_names) {
      words_col <- paste0(method, "_words")
      if (!words_col %in% names(topic_label_summary)) {
        words_col <- grep("_words$", names(topic_label_summary), value = TRUE)[1]
      }
      
      words <- topic_label_summary[[words_col]][i]
      word_array <- unlist(strsplit(words, ", "))
      topic_words <- word_array[1:min(4, length(word_array))]
      
      topic_names$topic_name[i] <- paste0(
        toupper(substr(topic_words[1], 1, 1)),
        substr(topic_words[1], 2, nchar(topic_words[1])),
        ": ",
        paste(topic_words[2:length(topic_words)], collapse = ", ")
      )
      
      topic_names$short_name[i] <- word_array[1]
    }
  }
  
  # Check for empty short names
  empty_short <- which(topic_names$short_name == "")
  if (length(empty_short) > 0) {
    warning_msg <- paste("Found", length(empty_short), "empty short names - using defaults")
    diagnostics$naming_issues <- c(diagnostics$naming_issues, warning_msg)
    log_message(warning_msg, "name_topics", "WARNING")
    
    # Generate default short names for empty entries
    for (i in empty_short) {
      # Get first word of topic name
      first_word <- unlist(strsplit(topic_names$topic_name[i], " "))[1]
      first_word <- gsub(":", "", first_word)  # Remove colon if present
      
      topic_names$short_name[i] <- first_word
    }
  }
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create data result
  result_data <- list(
    topic_names = topic_names,
    topic_terms = topic_label_summary
  )
  
  # Add examples if generated
  if (!is.null(topic_examples)) {
    result_data$topic_examples <- topic_examples
  }
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    topic_count = k,
    naming_mode = mode,
    naming_method = method,
    include_examples = include_examples
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