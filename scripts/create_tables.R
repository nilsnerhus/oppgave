#' @title Create summary tables for reporting
#' @description Creates standardized tables for reporting by formatting and 
#'   consolidating results from model fitting, topic naming, and dominance analysis.
#'   No recalculation is performed - tables are built using pre-computed values.
#'   
#' @param model Result from fit_model() containing model output and effects analysis
#' @param topic_names Result from name_topics() with topic names and metadata
#' @param dominance Result from find_dominance() with dominance analysis
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item topics_table - Table summarizing topics with names and top terms
#'       \item dominance_table - Formatted table with dominance by dimension 
#'       \item effects_table - Formatted table with variance explained by dimension
#'       \item model_summary - Formatted summary statistics about the model
#'     }
#'   }
#'   \item{metadata}{Processing information}
#'   \item{diagnostics}{Table creation issues}
#'
#' @examples
#' \dontrun{
#' # Generate tables from model outputs
#' tables <- create_tables(model, topic_names, dominance)
#' 
#' # Access specific values for inline reporting
#' overall_dominance <- tables$data$dominance_table$dominance[
#'   tables$data$dominance_table$category == "Overall"
#' ]
#' 
#' top_dimension <- tables$data$effects_table$dimension[1]
#' }
create_tables <- function(
    model,
    topic_names,
    dominance
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    table_issues = character(),
    input_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "create_tables")
  
  # Validate inputs with minimal checks
  if (!is.list(model) || !"data" %in% names(model)) {
    warning_msg <- "model not in expected format"
    diagnostics$input_issues <- c(diagnostics$input_issues, warning_msg)
    log_message(warning_msg, "create_tables", "WARNING")
  }
  
  if (!is.list(topic_names) || !"data" %in% names(topic_names)) {
    warning_msg <- "topic_names not in expected format"
    diagnostics$input_issues <- c(diagnostics$input_issues, warning_msg)
    log_message(warning_msg, "create_tables", "WARNING")
  }
  
  if (!is.list(dominance) || !"data" %in% names(dominance)) {
    warning_msg <- "dominance not in expected format"
    diagnostics$input_issues <- c(diagnostics$input_issues, warning_msg)
    log_message(warning_msg, "create_tables", "WARNING")
  }
  
  ## --- Create topics table ----------------------------------------------------
  log_message("Creating topics table", "create_tables")
  
  # Simply use topic_names data directly
  topics_table <- if (!is.null(topic_names) && 
                      is.list(topic_names$data) && 
                      "topic_names" %in% names(topic_names$data)) {
    # Start with basic topic names
    table <- topic_names$data$topic_names
    
    # Add top terms if available in a consolidated column for easier reporting
    if ("topic_terms" %in% names(topic_names$data)) {
      terms <- topic_names$data$topic_terms
      
      if ("frex_words" %in% names(terms)) {
        table$top_terms <- terms$frex_words
      } else if ("prob_words" %in% names(terms)) {
        table$top_terms <- terms$prob_words
      } else {
        table$top_terms <- NA_character_
      }
    }
    
    table
  } else {
    # Fallback: create minimal table
    data.frame(
      topic_id = integer(0),
      topic_name = character(0),
      short_name = character(0),
      top_terms = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  ## --- Create dominance table -------------------------------------------------
  log_message("Creating dominance table", "create_tables")
  
  # Simply use dominance data directly
  dominance_table <- if (!is.null(dominance) && 
                         is.list(dominance$data) && 
                         "dimension_dominance" %in% names(dominance$data)) {
    # Use the pre-formatted table directly
    dominance$data$dimension_dominance
  } else {
    # Fallback: create minimal table
    data.frame(
      category = character(0),
      level = character(0),
      docs = integer(0),
      dominance = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  ## --- Create effects table ---------------------------------------------------
  log_message("Creating effects table", "create_tables")
  
  # Format variance explained from model
  effects_table <- if (!is.null(model) && 
                       is.list(model$data) && 
                       "variance_explained" %in% names(model$data)) {
    # Get variance explained
    var_explained <- model$data$variance_explained
    
    if (is.list(var_explained) && length(var_explained) > 0) {
      # Convert to data frame
      table <- data.frame(
        dimension = names(var_explained),
        explained_var = as.numeric(unlist(var_explained)) * 100,  # Convert to percentage
        stringsAsFactors = FALSE
      )
      
      # Format for display
      table$explained_var <- round(table$explained_var, 1)
      
      # Use significance information if available in model data
      if ("effects_significance" %in% names(model$data)) {
        sig_data <- model$data$effects_significance
        table <- dplyr::left_join(table, sig_data, by = "dimension")
      } else {
        table$significant <- TRUE  # Default to assuming all are significant
      }
      
      # Sort by variance explained (descending)
      table <- table[order(table$explained_var, decreasing = TRUE), ]
      
      # Add display column for easier reporting
      table$display_value <- ifelse(
        table$significant,
        paste0(table$explained_var, "%"),
        "Not significant"
      )
      
      table
    } else {
      data.frame(
        dimension = character(0),
        explained_var = numeric(0),
        significant = logical(0),
        stringsAsFactors = FALSE
      )
    }
  } else {
    data.frame(
      dimension = character(0),
      explained_var = numeric(0),
      significant = logical(0),
      stringsAsFactors = FALSE
    )
  }
  
  ## --- Create model summary ---------------------------------------------------
  log_message("Creating model summary", "create_tables")
  
  # Format model quality metrics for easy access
  model_summary <- data.frame(
    metric = character(),
    value = character(),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(model)) {
    # Extract from diagnostics
    if (is.list(model$diagnostics) && "model_quality" %in% names(model$diagnostics)) {
      quality <- model$diagnostics$model_quality
      
      # Add each metric in a row
      if (!is.null(quality$coherence)) {
        model_summary <- rbind(model_summary, 
                               data.frame(metric = "coherence", 
                                          value = as.character(round(quality$coherence, 3)),
                                          stringsAsFactors = FALSE))
      }
      
      if (!is.null(quality$exclusivity)) {
        model_summary <- rbind(model_summary, 
                               data.frame(metric = "exclusivity", 
                                          value = as.character(round(quality$exclusivity, 3)),
                                          stringsAsFactors = FALSE))
      }
    }
    
    # Extract from metadata
    if (is.list(model$metadata)) {
      if (!is.null(model$metadata$k)) {
        model_summary <- rbind(model_summary, 
                               data.frame(metric = "topics", 
                                          value = as.character(model$metadata$k),
                                          stringsAsFactors = FALSE))
      }
      
      if (!is.null(model$metadata$documents)) {
        model_summary <- rbind(model_summary, 
                               data.frame(metric = "documents", 
                                          value = as.character(model$metadata$documents),
                                          stringsAsFactors = FALSE))
      }
    }
  }
  
  # Transform to wide format for easier access
  model_summary_wide <- setNames(
    as.list(model_summary$value), 
    model_summary$metric
  )
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create tables list
  tables <- list(
    topics_table = topics_table,
    dominance_table = dominance_table,
    effects_table = effects_table,
    model_summary = model_summary_wide
  )
  
  # Create metadata
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    table_counts = list(
      topics = nrow(topics_table),
      dominance = nrow(dominance_table),
      effects = nrow(effects_table),
      summary = length(model_summary_wide)
    )
  )
  
  log_message("Table creation complete", "create_tables")
  
  # Return standardized result
  return(create_result(
    data = tables,
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}