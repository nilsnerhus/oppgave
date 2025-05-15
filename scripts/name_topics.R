#' @title Interactively name topics one by one
#' @description Helps assign meaningful names to topics based on word lists through
#'   an interactive process that shows one topic at a time and prompts for user input.
#'   When called with existing names, validates and returns those names.
#'   
#' @param model The output from the fit_model() function containing topic information
#' @param proposed_names Optional data frame with columns 'topic_id', 'topic_name', and 'short_name'
#'   containing human-interpreted topic names. If NULL, starts interactive naming.
#'   
#' @return A data frame with topic IDs and names that can be used in subsequent analysis
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
name_topics <- function(model, proposed_names = NULL) {
  # Extract topic information
  k <- model$metadata$k
  label_summary <- model$data$label_summary
  
  # If names provided, validate and return them
  if (!is.null(proposed_names)) {
    # Validate proposed names
    if (!all(c("topic_id", "topic_name", "short_name") %in% names(proposed_names))) {
      stop("proposed_names must have columns 'topic_id', 'topic_name', and 'short_name'")
    }
    
    # Ensure all topics are covered
    if (!all(1:k %in% proposed_names$topic_id)) {
      stop("proposed_names must include all topic IDs from 1 to ", k)
    }
    
    return(proposed_names)
  }
  
  # Initialize empty data frame for topic names
  topic_names <- data.frame(
    topic_id = 1:k,
    topic_name = character(k),
    short_name = character(k),
    stringsAsFactors = FALSE
  )
  
  # Interactive naming process, one topic at a time
  cat("\nInteractive Topic Naming\n")
  cat("=======================\n")
  cat("For each topic, you'll see the top words and be prompted to enter:\n")
  cat("1. A descriptive name (e.g., 'Climate Finance')\n")
  cat("2. A short name/abbreviation (e.g., 'Finance')\n\n")
  
  for (i in 1:k) {
    cat(paste0("\n------ Topic ", i, " of ", k, " ------\n"))
    cat(paste0("Probability: ", label_summary$prob_words[i], "\n"))
    cat(paste0("FREX: ", label_summary$frex_words[i], "\n"))
    cat(paste0("Lift: ", label_summary$lift_words[i], "\n"))
    cat(paste0("Score: ", label_summary$score_words[i], "\n\n"))
    
    # Prompt for topic name
    cat("Enter descriptive name: ")
    topic_name <- readline()
    
    # If user entered nothing, use default
    if (topic_name == "") {
      topic_name <- paste0("Topic ", i)
      cat(paste0("Using default: '", topic_name, "'\n"))
    }
    
    # Prompt for short name
    cat("Enter short name/abbreviation: ")
    short_name <- readline()
    
    # If user entered nothing, use default or first word
    if (short_name == "") {
      # Extract first word of topic name as default short name
      first_word <- strsplit(topic_name, " ")[[1]][1]
      short_name <- ifelse(is.na(first_word), paste0("T", i), first_word)
      cat(paste0("Using default: '", short_name, "'\n"))
    }
    
    # Store in data frame
    topic_names$topic_name[i] <- topic_name
    topic_names$short_name[i] <- short_name
  }
  
  cat("\nTopic naming complete! Summary:\n")
  print(topic_names)
  
  # Ask if user wants to save the names
  cat("\nDo you want to save these topic names for future use? (y/n): ")
  save_response <- readline()
  
  if (tolower(save_response) == "y") {
    saveRDS(topic_names, "data/topic_names.rds")
    cat("Topic names saved to 'data/topic_names.rds'\n")
  }
  
  return(topic_names)
}