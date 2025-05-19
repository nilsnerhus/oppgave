#' @title Run STM's searchK to evaluate topic models
#' @description Executes STM's searchK function with minimal configuration to test
#'   different numbers of topics. This function is separated from the selection logic
#'   to improve error handling and enable separate caching.
#'
#' @param stm_data Result from prepare_stm_data() containing STM input data and metadata
#' @param min_k Minimum number of topics to try (default: 5)
#' @param max_k Maximum number of topics to try (default: 40)
#' @param step_k Step size for k search (default: 5)
#' @param seed Random seed for reproducibility (default: 1234)
#' @param iterations Maximum EM algorithm iterations (default: 100)
#'
#' @return A list containing:
#'   \item{data}{
#'     \itemize{
#'       \item searchK_results - Raw output from STM's searchK function
#'       \item k_values - Vector of k values that were tested
#'     }
#'   }
#'   \item{metadata}{Processing information and search parameters}
#'   \item{diagnostics}{Processing details and errors encountered}
#'
search_k <- function(
    stm_data,
    min_k = 5,
    max_k = 40,
    step_k = 5,
    seed = 1234,
    iterations = 100
) {
  ## --- Setup & Initialization -------------------------------------------------
  start_time <- Sys.time()
  
  # Initialize diagnostics tracking
  diagnostics <- list(
    k_values = NULL,
    processing_issues = character()
  )
  
  ## --- Input validation -------------------------------------------------------
  log_message("Validating input data", "search_k")
  
  # Validate stm_data
  if (!is.list(stm_data) || 
      !"data" %in% names(stm_data) ||
      !all(c("stm_input", "metadata") %in% names(stm_data$data))) {
    error_msg <- "stm_data must be the output from prepare_stm_data() with stm_input and metadata"
    diagnostics$processing_issues <- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "search_k", "ERROR")
    
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Extract data from input
  stm_input <- stm_data$data$stm_input
  metadata <- stm_data$data$metadata
  
  ## --- Prepare k values -------------------------------------------------------
  log_message(paste("Determining optimal number of topics (min =", min_k, 
                    ", max =", max_k, 
                    ", step =", step_k, ")"), "search_k")
  
  # Generate sequence of k values to test
  k_values <- seq(min_k, max_k, by = step_k)
  log_message(paste("Testing K values:", paste(k_values, collapse = ", ")), "search_k")
  
  # Store k values in diagnostics
  diagnostics$k_values <- k_values
  
  ## --- Create a simplified prevalence formula ---------------------------------
  log_message("Using intercept-only prevalence formula", "search_k")
  prevalence_formula <- stats::as.formula("~ 1")
  
  ## --- Run searchK ------------------------------------------------------------
  log_message("Running searchK to evaluate topic models", "search_k")
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Run searchK with error handling
  k_search <- tryCatch({
    stm::searchK(
      documents = stm_input$documents,
      vocab = stm_input$vocab,
      K = k_values,
      prevalence = prevalence_formula,
      data = metadata,
      max.em.its = iterations,
      init.type = "Spectral",
      verbose = FALSE,
      seed = seed
    )
  }, error = function(e) {
    error_msg <- paste("Error in searchK:", e$message)
    diagnostics$processing_issues <<- c(diagnostics$processing_issues, error_msg)
    log_message(error_msg, "search_k", "ERROR")
    return(NULL)
  })
  
  # Check if searchK ran successfully
  if (is.null(k_search)) {
    log_message("searchK failed to run", "search_k", "ERROR")
    return(create_result(
      data = NULL,
      metadata = list(
        timestamp = Sys.time(),
        success = FALSE,
        k_values = k_values
      ),
      diagnostics = diagnostics
    ))
  }
  
  # Log success
  log_message("Successfully completed searchK for all k values", "search_k")
  
  ## --- Create result object ---------------------------------------------------
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create metadata about the processing
  result_metadata <- list(
    timestamp = start_time,
    processing_time_sec = processing_time,
    k_values = k_values,
    iterations = iterations,
    seed = seed,
    success = TRUE
  )
  
  # Return raw searchK results
  return(create_result(
    data = list(
      searchK_results = k_search,
      k_values = k_values
    ),
    metadata = result_metadata,
    diagnostics = diagnostics
  ))
}