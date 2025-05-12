#' @title Create dominance summary table
#' @description Aggregates document-level dominance values by dimension categories,
#'   calculating summary statistics for each group.
#'
#' @param doc_data Data frame containing document-level dominance values
#' @param dominance_col Name of column containing dominance values
#' @param category_map List mapping higher-level categories to dimensions
#'
#' @return A data frame with dominance statistics by category and level

create_dominance_table <- function(
    doc_data,
    dominance_col,
    category_map
) {
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize results table
  result_table <- data.frame(
    category = character(),
    level = character(),
    docs = integer(),
    raw_dominance = numeric(),
    norm_dominance = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Add overall statistics
  overall_stats <- doc_data %>%
    dplyr::summarize(
      docs = dplyr::n(),
      raw_dominance = mean(!!dplyr::sym(dominance_col), na.rm = TRUE),
      std_dev = stats::sd(!!dplyr::sym(dominance_col), na.rm = TRUE),
      ci_width = 1.96 * std_dev / sqrt(docs),
      norm_dominance = raw_dominance,  # For overall, these are the same
      ci_lower = raw_dominance - ci_width,
      ci_upper = raw_dominance + ci_width
    )
  
  result_table <- rbind(result_table, data.frame(
    category = "Overall",
    level = "Category",
    docs = overall_stats$docs,
    raw_dominance = round(overall_stats$raw_dominance, 3),
    norm_dominance = round(overall_stats$norm_dominance, 3),
    ci_lower = round(overall_stats$ci_lower, 3),
    ci_upper = round(overall_stats$ci_upper, 3),
    stringsAsFactors = FALSE
  ))
  
  # Process each category in the map
  for (category_name in names(category_map)) {
    # Get dimensions for this category
    category_dimensions <- category_map[[category_name]]
    
    # Skip empty categories
    if (length(category_dimensions) == 0) {
      next
    }
    
    # Add category row
    result_table <- rbind(result_table, data.frame(
      category = category_name,
      level = "Category",
      docs = overall_stats$docs,
      raw_dominance = round(overall_stats$raw_dominance, 3),
      norm_dominance = round(overall_stats$norm_dominance, 3),
      ci_lower = round(overall_stats$ci_lower, 3),
      ci_upper = round(overall_stats$ci_upper, 3),
      stringsAsFactors = FALSE
    ))
    
    # Process dimensions differently based on category
    if (category_name %in% c("Income", "Region")) {
      # For categorical dimensions (single column)
      dim <- category_dimensions[1]  # Should be only one dimension
      
      # Get unique values for this dimension
      values <- unique(doc_data[[dim]])
      values <- values[!is.na(values)]
      
      # Process each value
      for (val in values) {
        # Filter data
        group_data <- doc_data[doc_data[[dim]] == val, ]
        
        # Skip if too few documents
        if (nrow(group_data) < 3) {
          next
        }
        
        # Calculate stats
        group_stats <- group_data %>%
          dplyr::summarize(
            docs = dplyr::n(),
            raw_dominance = mean(!!dplyr::sym(dominance_col), na.rm = TRUE),
            std_dev = stats::sd(!!dplyr::sym(dominance_col), na.rm = TRUE),
            ci_width = 1.96 * std_dev / sqrt(docs),
            norm_dominance = raw_dominance,  # Can implement normalization if needed
            ci_lower = raw_dominance - ci_width,
            ci_upper = raw_dominance + ci_width
          )
        
        # Add to results with corrected formatting
        result_table <- rbind(result_table, data.frame(
          category = paste0("- ", val),
          level = "Sub-category",
          docs = group_stats$docs,
          raw_dominance = round(group_stats$raw_dominance, 3),
          norm_dominance = round(group_stats$norm_dominance, 3),
          ci_lower = round(group_stats$ci_lower, 3),
          ci_upper = round(group_stats$ci_upper, 3),
          stringsAsFactors = FALSE
        ))
      }
    } else if (category_name == "Geography") {
      # For binary dimensions (multiple columns)
      for (dim in category_dimensions) {
        # Only process TRUE values
        # Filter data for TRUE values
        if (!"logical" %in% class(doc_data[[dim]])) {
          # Convert to logical if needed
          if (is.character(doc_data[[dim]]) || is.factor(doc_data[[dim]])) {
            doc_data[[dim]] <- tolower(as.character(doc_data[[dim]])) %in% c("true", "yes", "1", "t")
          } else if (is.numeric(doc_data[[dim]])) {
            doc_data[[dim]] <- doc_data[[dim]] > 0
          }
        }
        
        group_data <- doc_data[doc_data[[dim]] == TRUE, ]
        
        # Skip if too few documents
        if (nrow(group_data) < 3) {
          next
        }
        
        # Calculate stats
        group_stats <- group_data %>%
          dplyr::summarize(
            docs = dplyr::n(),
            raw_dominance = mean(!!dplyr::sym(dominance_col), na.rm = TRUE),
            std_dev = stats::sd(!!dplyr::sym(dominance_col), na.rm = TRUE),
            ci_width = 1.96 * std_dev / sqrt(docs),
            norm_dominance = raw_dominance,  # Can implement normalization if needed
            ci_lower = raw_dominance - ci_width,
            ci_upper = raw_dominance + ci_width
          )
        
        # Format subcategory name based on dimension
        subcategory_display <- switch(dim,
                                      "is_sids" = "SIDS",
                                      "is_lldc" = "LLDC",
                                      gsub("is_", "", dim)  # Default formatting
        )
        
        # Add to results with corrected formatting
        result_table <- rbind(result_table, data.frame(
          category = paste0("- ", subcategory_display),
          level = "Sub-category",
          docs = group_stats$docs,
          raw_dominance = round(group_stats$raw_dominance, 3),
          norm_dominance = round(group_stats$norm_dominance, 3),
          ci_lower = round(group_stats$ci_lower, 3),
          ci_upper = round(group_stats$ci_upper, 3),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(result_table)
}