#' @title Calculate Topic Dominance Across Categories
#' @description Calculates dominance metrics for all category/subcategory combinations.
#'
#' @param model STM model result from fit_model()
#' @param topics Named topics result from name_topics()
#' @param n Number of top topics to consider (default: 3)
#' @param normalize Whether to normalize dominance values (default: TRUE)
#'
#' @return Data frame with dominance metrics for all category/subcategory combinations
calculate_dominance <- function(model, topics, n = 3, normalize = TRUE) {
  # Extract basic components
  theta <- model$data$model$theta
  meta <- model$data$aligned_meta
  category_map <- model$data$category_map
  topics_table <- topics$data$topics_table
  
  # Get dimensions
  n_docs <- nrow(theta)
  k <- ncol(theta)
  
  # Calculate normalization baseline
  uniform_baseline <- n/k
  
  # Initialize results data frame
  results <- data.frame(
    level_type = character(),
    category = character(),  
    subcategory = character(),
    documents = integer(),
    raw_dominance = numeric(),
    normalized_dominance = numeric(),
    variance = numeric(),
    top_topics = character(),
    stringsAsFactors = FALSE
  )
  
  # Process global metrics (all documents)
  # Document-level dominance
  doc_dominance <- apply(theta, 1, function(doc_props) {
    sum(sort(doc_props, decreasing = TRUE)[1:min(n, k)])
  })
  global_doc_mean <- mean(doc_dominance)
  global_doc_var <- var(doc_dominance)
  global_doc_norm <- if(normalize) (global_doc_mean - uniform_baseline) / (1 - uniform_baseline) else global_doc_mean
  
  # Corpus-level dominance
  global_props <- colMeans(theta)
  global_sorted <- sort(global_props, decreasing = TRUE)
  global_corpus_dom <- sum(global_sorted[1:min(n, k)])
  global_corpus_var <- var(global_sorted[1:min(n, k)])
  global_corpus_norm <- if(normalize) (global_corpus_dom - uniform_baseline) / (1 - uniform_baseline) else global_corpus_dom
  
  # Get top topics
  global_top_indices <- order(global_props, decreasing = TRUE)[1:min(n, k)]
  global_top_topics <- sapply(global_top_indices, function(idx) {
    topic_row <- which(topics_table$topic_id == idx)
    if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
  })
  global_top_topics_str <- paste(global_top_topics, collapse = ", ")
  
  # Add global metrics to results
  results <- rbind(results, 
                   data.frame(
                     level_type = "document",
                     category = "Overall", 
                     subcategory = "Overall",
                     documents = n_docs,
                     raw_dominance = global_doc_mean,
                     normalized_dominance = global_doc_norm,
                     variance = global_doc_var,
                     top_topics = global_top_topics_str,
                     stringsAsFactors = FALSE
                   ),
                   data.frame(
                     level_type = "corpus",
                     category = "Overall", 
                     subcategory = "Overall",
                     documents = n_docs,
                     raw_dominance = global_corpus_dom,
                     normalized_dominance = global_corpus_norm,
                     variance = global_corpus_var,
                     top_topics = global_top_topics_str,
                     stringsAsFactors = FALSE
                   )
  )
  
  # Process each category in map
  if (!is.null(category_map) && length(category_map) > 0) {
    for (category in names(category_map)) {
      # Get category column(s)
      cols <- category_map[[category]]
      
      # Handle different category types
      if (length(cols) == 1 && cols[1] %in% names(meta) && !is.logical(meta[[cols[1]]])) {
        # Categorical column (e.g., Region, Income)
        col <- cols[1]
        
        # Get unique subcategories
        subcats <- unique(meta[[col]])
        subcats <- subcats[!is.na(subcats)]
        
        # Track all documents in this category
        all_category_docs <- integer(0)
        
        # Process each subcategory
        for (subcat in subcats) {
          # Get documents in this subcategory
          doc_indices <- which(meta[[col]] == subcat)
          
          # Skip if too few documents
          if (length(doc_indices) < 3) {
            next
          }
          
          # Add to category documents
          all_category_docs <- c(all_category_docs, doc_indices)
          
          # Document-level metrics
          subcat_doc_values <- doc_dominance[doc_indices]
          subcat_doc_mean <- mean(subcat_doc_values)
          subcat_doc_var <- var(subcat_doc_values)
          subcat_doc_norm <- if(normalize) (subcat_doc_mean - uniform_baseline) / (1 - uniform_baseline) else subcat_doc_mean
          
          # Corpus-level metrics
          subcat_props <- colMeans(theta[doc_indices, , drop = FALSE])
          subcat_sorted <- sort(subcat_props, decreasing = TRUE)
          subcat_corpus_dom <- sum(subcat_sorted[1:min(n, k)])
          subcat_corpus_var <- var(subcat_sorted[1:min(n, k)])
          subcat_corpus_norm <- if(normalize) (subcat_corpus_dom - uniform_baseline) / (1 - uniform_baseline) else subcat_corpus_dom
          
          # Get top topics
          subcat_top_indices <- order(subcat_props, decreasing = TRUE)[1:min(n, k)]
          subcat_top_topics <- sapply(subcat_top_indices, function(idx) {
            topic_row <- which(topics_table$topic_id == idx)
            if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
          })
          subcat_top_topics_str <- paste(subcat_top_topics, collapse = ", ")
          
          # Add to results
          results <- rbind(results,
                           data.frame(
                             level_type = "document",
                             category = category,
                             subcategory = as.character(subcat),
                             documents = length(doc_indices),
                             raw_dominance = subcat_doc_mean,
                             normalized_dominance = subcat_doc_norm,
                             variance = subcat_doc_var,
                             top_topics = subcat_top_topics_str,
                             stringsAsFactors = FALSE
                           ),
                           data.frame(
                             level_type = "corpus",
                             category = category,
                             subcategory = as.character(subcat),
                             documents = length(doc_indices),
                             raw_dominance = subcat_corpus_dom,
                             normalized_dominance = subcat_corpus_norm,
                             variance = subcat_corpus_var,
                             top_topics = subcat_top_topics_str,
                             stringsAsFactors = FALSE
                           )
          )
        }
        
        # Calculate category metrics (all subcategories combined)
        if (length(all_category_docs) >= 3) {
          # Ensure unique documents
          all_category_docs <- unique(all_category_docs)
          
          # Document-level metrics
          cat_doc_values <- doc_dominance[all_category_docs]
          cat_doc_mean <- mean(cat_doc_values)
          cat_doc_var <- var(cat_doc_values)
          cat_doc_norm <- if(normalize) (cat_doc_mean - uniform_baseline) / (1 - uniform_baseline) else cat_doc_mean
          
          # Corpus-level metrics
          cat_props <- colMeans(theta[all_category_docs, , drop = FALSE])
          cat_sorted <- sort(cat_props, decreasing = TRUE)
          cat_corpus_dom <- sum(cat_sorted[1:min(n, k)])
          cat_corpus_var <- var(cat_sorted[1:min(n, k)])
          cat_corpus_norm <- if(normalize) (cat_corpus_dom - uniform_baseline) / (1 - uniform_baseline) else cat_corpus_dom
          
          # Get top topics
          cat_top_indices <- order(cat_props, decreasing = TRUE)[1:min(n, k)]
          cat_top_topics <- sapply(cat_top_indices, function(idx) {
            topic_row <- which(topics_table$topic_id == idx)
            if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
          })
          cat_top_topics_str <- paste(cat_top_topics, collapse = ", ")
          
          # Add to results
          results <- rbind(results,
                           data.frame(
                             level_type = "document",
                             category = category,
                             subcategory = "Overall",
                             documents = length(all_category_docs),
                             raw_dominance = cat_doc_mean,
                             normalized_dominance = cat_doc_norm,
                             variance = cat_doc_var,
                             top_topics = cat_top_topics_str,
                             stringsAsFactors = FALSE
                           ),
                           data.frame(
                             level_type = "corpus",
                             category = category,
                             subcategory = "Overall",
                             documents = length(all_category_docs),
                             raw_dominance = cat_corpus_dom,
                             normalized_dominance = cat_corpus_norm,
                             variance = cat_corpus_var,
                             top_topics = cat_top_topics_str,
                             stringsAsFactors = FALSE
                           )
          )
        }
        
      } else if (any(grepl("^is_", cols))) {
        # Binary flags (e.g., Geography with is_sids, is_lldc)
        
        # Track all documents in this category
        all_category_docs <- integer(0)
        
        # Process each binary column
        for (col in cols) {
          # Skip if column doesn't exist
          if (!col %in% names(meta)) {
            next
          }
          
          # Create subcategory name
          if (grepl("^is_", col)) {
            subcategory <- toupper(gsub("^is_", "", col))
          } else {
            subcategory <- col
          }
          
          # Get documents where flag is TRUE
          doc_indices <- which(meta[[col]] == TRUE)
          
          # Skip if too few documents
          if (length(doc_indices) < 3) {
            next
          }
          
          # Add to category documents
          all_category_docs <- c(all_category_docs, doc_indices)
          
          # Document-level metrics
          subcat_doc_values <- doc_dominance[doc_indices]
          subcat_doc_mean <- mean(subcat_doc_values)
          subcat_doc_var <- var(subcat_doc_values)
          subcat_doc_norm <- if(normalize) (subcat_doc_mean - uniform_baseline) / (1 - uniform_baseline) else subcat_doc_mean
          
          # Corpus-level metrics
          subcat_props <- colMeans(theta[doc_indices, , drop = FALSE])
          subcat_sorted <- sort(subcat_props, decreasing = TRUE)
          subcat_corpus_dom <- sum(subcat_sorted[1:min(n, k)])
          subcat_corpus_var <- var(subcat_sorted[1:min(n, k)])
          subcat_corpus_norm <- if(normalize) (subcat_corpus_dom - uniform_baseline) / (1 - uniform_baseline) else subcat_corpus_dom
          
          # Get top topics
          subcat_top_indices <- order(subcat_props, decreasing = TRUE)[1:min(n, k)]
          subcat_top_topics <- sapply(subcat_top_indices, function(idx) {
            topic_row <- which(topics_table$topic_id == idx)
            if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
          })
          subcat_top_topics_str <- paste(subcat_top_topics, collapse = ", ")
          
          # Add to results
          results <- rbind(results,
                           data.frame(
                             level_type = "document",
                             category = category,
                             subcategory = subcategory,
                             documents = length(doc_indices),
                             raw_dominance = subcat_doc_mean,
                             normalized_dominance = subcat_doc_norm,
                             variance = subcat_doc_var,
                             top_topics = subcat_top_topics_str,
                             stringsAsFactors = FALSE
                           ),
                           data.frame(
                             level_type = "corpus",
                             category = category,
                             subcategory = subcategory,
                             documents = length(doc_indices),
                             raw_dominance = subcat_corpus_dom,
                             normalized_dominance = subcat_corpus_norm,
                             variance = subcat_corpus_var,
                             top_topics = subcat_top_topics_str,
                             stringsAsFactors = FALSE
                           )
          )
        }
        
        # Calculate category metrics (all binary flags combined)
        if (length(all_category_docs) >= 3) {
          # Ensure unique documents
          all_category_docs <- unique(all_category_docs)
          
          # Document-level metrics
          cat_doc_values <- doc_dominance[all_category_docs]
          cat_doc_mean <- mean(cat_doc_values)
          cat_doc_var <- var(cat_doc_values)
          cat_doc_norm <- if(normalize) (cat_doc_mean - uniform_baseline) / (1 - uniform_baseline) else cat_doc_mean
          
          # Corpus-level metrics
          cat_props <- colMeans(theta[all_category_docs, , drop = FALSE])
          cat_sorted <- sort(cat_props, decreasing = TRUE)
          cat_corpus_dom <- sum(cat_sorted[1:min(n, k)])
          cat_corpus_var <- var(cat_sorted[1:min(n, k)])
          cat_corpus_norm <- if(normalize) (cat_corpus_dom - uniform_baseline) / (1 - uniform_baseline) else cat_corpus_dom
          
          # Get top topics
          cat_top_indices <- order(cat_props, decreasing = TRUE)[1:min(n, k)]
          cat_top_topics <- sapply(cat_top_indices, function(idx) {
            topic_row <- which(topics_table$topic_id == idx)
            if (length(topic_row) > 0) topics_table$topic_name[topic_row] else paste("Topic", idx)
          })
          cat_top_topics_str <- paste(cat_top_topics, collapse = ", ")
          
          # Add to results
          results <- rbind(results,
                           data.frame(
                             level_type = "document",
                             category = category,
                             subcategory = "Overall",
                             documents = length(all_category_docs),
                             raw_dominance = cat_doc_mean,
                             normalized_dominance = cat_doc_norm,
                             variance = cat_doc_var,
                             top_topics = cat_top_topics_str,
                             stringsAsFactors = FALSE
                           ),
                           data.frame(
                             level_type = "corpus",
                             category = category,
                             subcategory = "Overall",
                             documents = length(all_category_docs),
                             raw_dominance = cat_corpus_dom,
                             normalized_dominance = cat_corpus_norm,
                             variance = cat_corpus_var,
                             top_topics = cat_top_topics_str,
                             stringsAsFactors = FALSE
                           )
          )
        }
      }
    }
  }
  
  # Return the results wrapped in standard structure
  return(create_result(
    data = list(metrics = results),
    metadata = list(
      n_value = n,
      k = k,
      normalized = normalize,
      uniform_baseline = uniform_baseline,
      success = TRUE
    ),
    diagnostics = list()
  ))
}