#' @title Extract topic proportions from topic model
#' @description Processes a structural topic model to extract document-topic 
#'   proportions and topic labels, preparing them for analysis. Can work with
#'   either a pre-fit model or create a new one with specified parameters.
#'
#' @param input Either output from optimal_topics() or prepare_corpus() functions
#' @param k Number of topics to use (default: NULL, uses best_k from input if available)
#' @param best_k_path Path to load best k result if needed (default: "data/best_k.rds")
#' @param output_path Path to save results (default: "data/topic_props.rds")
#'
#' @return A list containing:
#'   \item{data}{Topic proportions data frame, topic labels, and model object}
#'   \item{metadata}{Information about topics and processing details}
#'   \item{diagnostics}{Processing issues and model validation information}
#'
#' @examples
#' \dontrun{
#' # Extract topic proportions using best k from optimization
#' topic_props <- extract_topic_props(best_k_result)
#' 
#' # Force a specific number of topics
#' topic_props <- extract_topic_props(corpus_data, k = 50)
#' }

extract_topic_props <- function(
    input, 
    k = NULL,
    best_k_path = "data/best_k.rds",
    output_path = "data/topic_props.rds"
) {
  # Start timing
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Create output directory if needed
  ensure_directory(output_path)
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    processing_issues = character(),
    model_validation = list()
  )
  
  ## --- Input validation and determination ------------------------------------
  log_message("Determining input type and extracting components", "extract_topic_props")
  
  # Variables we need to extract
  model <- NULL
  country_metadata <- NULL  # Renamed for clarity
  stm_data <- NULL
  input_type <- NULL
  
  tryCatch({
    # Determine input type and extract necessary components
    if (is.list(input)) {
      # Extract country_metadata first - try different potential locations
      if ("country_metadata" %in% names(input)) {
        country_metadata <- input$country_metadata
        log_message("Found country_metadata directly", "extract_topic_props")
      } else if ("data" %in% names(input) && "country_metadata" %in% names(input$data)) {
        country_metadata <- input$data$country_metadata
        log_message("Found country_metadata in input$data$country_metadata", "extract_topic_props")
      } else if ("data" %in% names(input) && "metadata" %in% names(input$data)) {
        country_metadata <- input$data$metadata  # Legacy format
        log_message("Found country_metadata as metadata in input$data$metadata", "extract_topic_props")
      } else if ("metadata" %in% names(input)) {
        country_metadata <- input$metadata  # Legacy format
        log_message("Using legacy metadata from input$metadata", "extract_topic_props")
      }
      
      # Case 1: Input has direct model object
      if ("best_model" %in% names(input)) {
        log_message("Using best model from input", "extract_topic_props")
        model <- input$best_model
        input_type <- "optimal_topics"
        
        # Case 2: Input has path to model file
      } else if ("best_model_path" %in% names(input) && is.character(input$best_model_path)) {
        model_path <- input$best_model_path
        log_message(paste("Found model path:", model_path), "extract_topic_props")
        
        if (file.exists(model_path)) {
          model <- readRDS(model_path)
          log_message("Successfully loaded model from file", "extract_topic_props")
        } else {
          log_message(paste("Model file not found:", model_path), "extract_topic_props", "WARNING")
        }
        input_type <- "optimal_topics"
      }
      
      # Similarly extract stm_data
      if ("data" %in% names(input) && "stm_data" %in% names(input$data)) {
        stm_data <- input$data$stm_data
      } else if ("stm_data" %in% names(input)) {
        stm_data <- input$stm_data
      }
      
      # Use provided k or best k from optimization
      if (is.null(k)) {
        if ("best_k" %in% names(input)) {
          k <- input$best_k
          log_message(paste("Using optimal k value:", k), "extract_topic_props")
        } else if ("data" %in% names(input) && "best_k" %in% names(input$data)) {
          k <- input$data$best_k
          log_message(paste("Using optimal k value from data:", k), "extract_topic_props")
        } else {
          stop("Could not determine k value from input")
        }
      }
    }
  }, error = function(e) {
    log_message(paste("Input validation error:", e$message), "extract_topic_props", "ERROR")
    stop(e$message)
  })

  ## --- Create model if needed ------------------------------------------------
  if (is.null(model)) {
    log_message(paste("Fitting model with k =", k), "extract_topic_props")
    
    # Check we have required data
    if (is.null(stm_data) || !all(c("documents", "vocab") %in% names(stm_data))) {
      log_message("Missing required stm_data components", "extract_topic_props", "ERROR")
      stop("Cannot create model: missing documents or vocabulary")
    }
    
    # Fit the model
    model <- time_operation({
      tryCatch({
        stm::stm(
          documents = stm_data$documents,
          vocab = stm_data$vocab,
          K = k,
          max.em.its = 100,
          init.type = "Spectral",
          seed = 1234
        )
      }, error = function(e) {
        log_message(paste("Model fitting error:", e$message), "extract_topic_props", "ERROR")
        diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                           paste("Model fitting failed:", e$message))
        stop(e$message)
      })
    }, "extract_topic_props")
  }
  
  ## --- Extract topic proportions ---------------------------------------------
  log_message("Extracting topic proportions", "extract_topic_props")
  
  topic_props <- tryCatch({
    as.data.frame(model$theta)
  }, error = function(e) {
    log_message(paste("Topic proportion extraction error:", e$message), 
                "extract_topic_props", "ERROR")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("Topic proportion extraction failed:", e$message))
    stop(e$message)
  })
  
  # Get actual number of topics from the model
  actual_topics <- ncol(topic_props)
  if (actual_topics != k) {
    log_message(paste("WARNING: Expected", k, "topics but model has", actual_topics, 
                      "topics. Adjusting to match model."), "extract_topic_props", "WARNING")
    k <- actual_topics
  }
  
  # Add document identifiers
  topic_props$doc_id <- rownames(topic_props)
  
  # Rename topic columns with simple numeric identifiers
  colnames(topic_props)[1:k] <- paste0("Topic_", 1:k)
  
  ## --- Reshape to long format ------------------------------------------------
  log_message("Reshaping to long format", "extract_topic_props")
  
  topic_props_long <- topic_props %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Topic_"),
      names_to = "Topic",
      values_to = "Proportion"
    )
  
  ## --- Join with document metadata ------------------------------------------
  if (!is.null(country_metadata)) {
    log_message("Joining with country metadata", "extract_topic_props")
    
    # Add debug info about metadata
    log_message(paste("Country metadata class:", class(country_metadata)[1]), "extract_topic_props")
    
    # Ensure metadata is a data frame before joining
    if (is.data.frame(country_metadata)) {
      result_df <- topic_props_long %>%
        dplyr::left_join(country_metadata, by = "doc_id", copy = TRUE)
    } else {
      log_message("Country metadata is not a data frame, skipping join", "extract_topic_props", "WARNING")
      result_df <- topic_props_long
    }
  } else {
    result_df <- topic_props_long
  }
  
  ## --- Extract topic labels -------------------------------------------------
  log_message("Generating topic labels", "extract_topic_props")
  
  top_terms <- tryCatch({
    stm::labelTopics(model, n = 1)
  }, error = function(e) {
    log_message(paste("Topic labeling error:", e$message), 
                "extract_topic_props", "WARNING")
    diagnostics$processing_issues <- c(diagnostics$processing_issues, 
                                       paste("Topic labeling failed:", e$message))
    NULL
  })
  
  if (!is.null(top_terms)) {
    topic_labels <- sapply(1:k, function(i) {
      paste(top_terms$frex[i,], collapse = ", ")
    })
    names(topic_labels) <- paste0("Topic_", 1:k)
  } else {
    topic_labels <- setNames(rep("Unknown", k), paste0("Topic_", 1:k))
  }
  
  ## --- Perform validation checks --------------------------------------------
  log_message("Validating results", "extract_topic_props")
  
  # Basic validation checks
  validation <- list(
    doc_count = nrow(topic_props),
    topics = k,
    avg_props_sum = mean(rowSums(topic_props[, 1:k])),
    missing_metadata = if (!is.null(doc_metadata)) {
      sum(is.na(match(topic_props$doc_id, doc_metadata$doc_id)))
    } else {
      NA
    }
  )
  
  # Check if proportions sum to approximately 1
  if (abs(validation$avg_props_sum - 1) > 0.01) {
    log_message(paste("Topic proportions don't sum to 1 (average sum:", 
                      round(validation$avg_props_sum, 3), ")"),
                "extract_topic_props", "WARNING")
    diagnostics$processing_issues <- c(diagnostics$processing_issues,
                                       paste("Topic proportions don't sum to 1"))
  }
  
  # Check for missing metadata
  if (!is.na(validation$missing_metadata) && validation$missing_metadata > 0) {
    log_message(paste(validation$missing_metadata, "documents missing metadata"),
                "extract_topic_props", "WARNING")
    diagnostics$processing_issues <- c(diagnostics$processing_issues,
                                       paste(validation$missing_metadata, "documents missing metadata"))
  }
  
  diagnostics$model_validation <- validation
  
  ## --- Prepare final output -------------------------------------------------
  result_data <- list(
    data = result_df,
    topic_labels = topic_labels,
    model = model
  )
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Prepare metadata
  model_metadata <- list(  # Renamed for clarity
    timestamp = start_time,
    processing_time_sec = processing_time,
    input_type = input_type,
    topics = k,
    documents = nrow(topic_props),
    success = TRUE
  )
  
  log_message(paste("Topic modeling complete!", k, "topics extracted for", 
                    nrow(topic_props), "documents"),
              "extract_topic_props")
  
  return(create_result(
    data = result_data,
    metadata = model_metadata,  # Renamed
    diagnostics = diagnostics,
    country_metadata = country_metadata  # Explicitly retain country_metadata
  ))
}