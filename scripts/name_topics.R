#' @title Interactively name topics with representative passages and document lookup
#' @description Helps assign meaningful names to topics based on word lists and
#'   example passages. Provides document lookup functionality for manual exploration.
#'   
#' @param model The output from the fit_model() function
#' @param proposed_names Optional data frame with columns 'topic_id', 'topic_name', and 'short_name'
#' @param n_examples Number of example passages to extract per topic (default: 3)
#' @param n_docs Number of top documents to list per topic (default: 3)
#' @param example_length Maximum length of each example passage (default: 200)
#'   
#' @return A list containing:
#'   \item{data}{Data frame with topic IDs and names}
#'   \item{metadata}{List with timestamp and example passages}
#'   \item{diagnostics}{Processing information}
#'
#' @examples
#' \dontrun{
#' # Interactive topic naming
#' topic_names <- name_topics(model)
#' 
#' # Later, load existing names
#' saved_names <- readRDS("data/topic_names.rds")
#' topic_names <- name_topics(model, saved_names)
#' }
name_topics <- function(
    model,
    proposed_names = NULL,
    n_examples = 3,
    n_docs = 3,
    example_length = 200
) {
  # Start timing
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input model", "name_topics")
  
  # Extract topic information
  if (is.null(model) || !is.list(model) || !"data" %in% names(model) || 
      !"metadata" %in% names(model) || !"k" %in% names(model$metadata)) {
    error_msg <- "Invalid model format. Must be output from fit_model()."
    log_message(error_msg, "name_topics", "ERROR")
    stop(error_msg)
  }
  
  k <- model$metadata$k
  
  # Check if label summary exists
  if (is.null(model$data$label_summary)) {
    warning_msg <- "Model missing label_summary. Topic words may not be displayed."
    log_message(warning_msg, "name_topics", "WARNING")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, warning_msg)
    label_summary <- NULL
  } else {
    label_summary <- model$data$label_summary
  }
  
  ## --- Handle existing names --------------------------------------------------
  if (!is.null(proposed_names)) {
    log_message("Validating proposed names", "name_topics")
    
    # Validate proposed names
    if (!all(c("topic_id", "topic_name", "short_name") %in% names(proposed_names))) {
      error_msg <- "proposed_names must have columns 'topic_id', 'topic_name', and 'short_name'"
      log_message(error_msg, "name_topics", "ERROR")
      stop(error_msg)
    }
    
    # Ensure all topics are covered
    if (!all(1:k %in% proposed_names$topic_id)) {
      error_msg <- paste("proposed_names must include all topic IDs from 1 to", k)
      log_message(error_msg, "name_topics", "ERROR")
      stop(error_msg)
    }
    
    log_message("Using provided topic names", "name_topics")
    
    # Skip interactive naming, just return the validated names
    return(create_result(
      data = proposed_names,
      metadata = list(
        timestamp = start_time,
        source = "provided"
      ),
      diagnostics = diagnostics
    ))
  }
  
  ## --- Extract example passages -----------------------------------------------
  log_message("Extracting example passages", "name_topics")
  
  # Initialize storage for examples
  example_passages <- vector("list", k)
  have_examples <- FALSE
  
  # Check if STM model is available
  if (!is.null(model$data$model)) {
    tryCatch({
      stm_model <- model$data$model
      
      # Try to extract texts for findThoughts
      texts <- NULL
      if (!is.null(model$data$documents) && is.character(model$data$documents)) {
        texts <- model$data$documents
      } else if (!is.null(model$data$corpus_texts)) {
        texts <- model$data$corpus_texts
      }
      
      # Use findThoughts if texts are available
      if (!is.null(texts) && length(texts) > 0) {
        thoughts <- stm::findThoughts(
          stm_model,
          texts = texts,
          n = n_examples,
          topics = 1:k
        )
        
        # Store the examples
        for (i in 1:k) {
          if (length(thoughts$texts[[i]]) > 0) {
            # Truncate to max length
            example_passages[[i]] <- sapply(thoughts$texts[[i]], function(text) {
              substr(text, 1, example_length)
            })
          }
        }
        
        have_examples <- TRUE
        log_message("Successfully extracted example passages", "name_topics")
      } else {
        log_message("No document texts available for examples", "name_topics", "WARNING")
      }
    }, error = function(e) {
      error_msg <- paste("Error extracting example passages:", e$message)
      log_message(error_msg, "name_topics", "WARNING")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    })
  }
  
  ## --- Prepare top documents --------------------------------------------------
  log_message("Identifying top documents for each topic", "name_topics")
  
  # Initialize storage for top documents
  top_documents <- vector("list", k)
  have_documents <- FALSE
  
  # Check if topic proportions are available
  if (!is.null(model$data$topic_proportions)) {
    tryCatch({
      topic_props <- model$data$topic_proportions
      
      # Identify metadata columns
      meta_cols <- setdiff(names(topic_props), 
                           grep("^Topic_", names(topic_props), value = TRUE))
      
      # Process each topic
      for (i in 1:k) {
        topic_col <- paste0("Topic_", i)
        
        if (topic_col %in% names(topic_props)) {
          # Sort by topic proportion
          top_indices <- order(topic_props[[topic_col]], decreasing = TRUE)[1:min(n_docs, nrow(topic_props))]
          
          # Extract document info and format for display
          doc_info <- topic_props[top_indices, ]
          
          # Create display strings for console output
          formatted_docs <- sapply(1:length(top_indices), function(j) {
            idx <- top_indices[j]
            doc_id <- topic_props$doc_id[idx]
            proportion <- round(topic_props[[topic_col]][idx] * 100, 1)
            
            # Add country name if available
            country_info <- ""
            if ("country_name" %in% names(topic_props)) {
              country_info <- paste0(" (", topic_props$country_name[idx], ")")
            }
            
            paste0("Doc ", doc_id, country_info, ": ", proportion, "%")
          })
          
          # Store both display format and full data
          top_documents[[i]] <- list(
            display = formatted_docs,
            data = doc_info
          )
        }
      }
      
      have_documents <- TRUE
      log_message("Successfully identified top documents", "name_topics")
    }, error = function(e) {
      error_msg <- paste("Error identifying top documents:", e$message)
      log_message(error_msg, "name_topics", "WARNING")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    })
  }
  
  ## --- Interactive naming process ---------------------------------------------
  log_message("Starting interactive naming process", "name_topics")
  
  # Initialize empty data frame for topic names
  topic_names <- data.frame(
    topic_id = 1:k,
    topic_name = character(k),
    short_name = character(k),
    stringsAsFactors = FALSE
  )
  
  # Display instructions
  cat("\nInteractive Topic Naming\n")
  cat("=======================\n")
  cat("For each topic, you'll see the top words and be prompted to enter:\n")
  cat("1. A descriptive name (e.g., 'Climate Finance')\n")
  cat("2. A short name/abbreviation (e.g., 'Finance')\n")
  cat("Enter 'cancel' at any prompt to exit without saving.\n")
  cat("Enter 'more' to see additional document details for a topic.\n\n")
  
  # Process each topic
  for (i in 1:k) {
    cat(paste0("\n------ Topic ", i, " of ", k, " ------\n"))
    
    # Display topic words if available
    if (!is.null(label_summary)) {
      cat(paste0("Probability: ", label_summary$prob_words[i], "\n"))
      cat(paste0("FREX: ", label_summary$frex_words[i], "\n"))
      cat(paste0("Lift: ", label_summary$lift_words[i], "\n"))
      cat(paste0("Score: ", label_summary$score_words[i], "\n"))
    } else {
      cat("No word information available for this topic.\n")
    }
    
    # Show example passages if available
    if (have_examples && length(example_passages[[i]]) > 0) {
      cat("\nExample passages:\n")
      for (j in 1:length(example_passages[[i]])) {
        cat(paste0(j, ': "', example_passages[[i]][j], '..."\n'))
      }
    }
    
    # Show top documents if available
    if (have_documents && !is.null(top_documents[[i]]$display)) {
      cat("\nTop documents:\n")
      for (j in 1:length(top_documents[[i]]$display)) {
        cat(paste0(j, ": ", top_documents[[i]]$display[j], "\n"))
      }
    }
    
    cat("\n")
    
    # Prompt for topic name with support for additional document details
    repeat {
      cat("Enter descriptive name (or 'cancel' to exit, 'more' for details): ")
      topic_name <- readline()
      
      # Check for special commands
      if (tolower(topic_name) == "cancel") {
        cat("Topic naming cancelled. No names saved.\n")
        return(NULL)
      } else if (tolower(topic_name) == "more") {
        # Show detailed document information
        if (have_documents && !is.null(top_documents[[i]]$data)) {
          cat("\nDetailed document information for Topic", i, ":\n")
          print(top_documents[[i]]$data)
          cat("\n")
        } else {
          cat("No additional document information available.\n")
        }
        # Continue the loop to prompt again
        next
      } else {
        # Normal input - proceed
        break
      }
    }
    
    # If user entered nothing, use default
    if (topic_name == "") {
      topic_name <- paste0("Topic ", i)
      cat(paste0("Using default: '", topic_name, "'\n"))
    }
    
    # Prompt for short name
    cat("Enter short name/abbreviation (or 'cancel' to exit): ")
    short_name <- readline()
    
    # Check for cancellation
    if (tolower(short_name) == "cancel") {
      cat("Topic naming cancelled. No names saved.\n")
      return(NULL)
    }
    
    # If user entered nothing, use default or first word
    if (short_name == "") {
      # Extract first word of topic name as default short name
      first_word <- strsplit(topic_name, " ")[[1]][1]
      short_name <- ifelse(is.na(first_word) || length(first_word) == 0, paste0("T", i), first_word)
      cat(paste0("Using default: '", short_name, "'\n"))
    }
    
    # Store in data frame
    topic_names$topic_name[i] <- topic_name
    topic_names$short_name[i] <- short_name
  }
  
  ## --- Finalize and save results ----------------------------------------------
  log_message("Topic naming complete", "name_topics")
  
  cat("\nTopic naming complete! Summary:\n")
  print(topic_names)
  
  # Ask if user wants to save the names
  cat("\nDo you want to save these topic names for future use? (y/n): ")
  save_response <- readline()
  
  if (tolower(save_response) == "y") {
    tryCatch({
      # Ensure directory exists
      ensure_directory("data/topic_names.rds")
      
      # Create result structure
      result <- create_result(
        data = topic_names,
        metadata = list(
          timestamp = start_time,
          examples = example_passages,
          top_documents = top_documents,
          have_examples = have_examples,
          have_documents = have_documents
        ),
        diagnostics = diagnostics
      )
      
      # Save the result
      saveRDS(result, "data/topic_names.rds")
      cat("Topic names saved to 'data/topic_names.rds'\n")
      log_message("Topic names saved successfully", "name_topics")
    }, error = function(e) {
      error_msg <- paste("Error saving topic names:", e$message)
      cat(error_msg, "\n")
      log_message(error_msg, "name_topics", "ERROR")
      diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    })
  }
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "secs")
  
  # Return result
  log_message(paste("Completed in", round(processing_time, 2), "seconds"), "name_topics")
  
  return(create_result(
    data = topic_names,
    metadata = list(
      timestamp = start_time,
      processing_time_sec = processing_time,
      examples = example_passages,
      top_documents = top_documents,
      have_examples = have_examples,
      have_documents = have_documents
    ),
    diagnostics = diagnostics
  ))
}