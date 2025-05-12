#' @title Create variance summary table
#' @description Aggregates document-level dominance variance measures by dimension 
#'   categories, calculating standard deviation and coefficient of variation.
#'
#' @param doc_data Data frame containing document-level dominance values
#' @param dominance_col Name of column containing dominance values
#' @param category_map List mapping higher-level categories to dimensions
#'
#' @return A data frame with variance statistics by category and level

create_variance_table <- function(
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
    std_dev = numeric(),
    coef_var = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Add overall statistics
  overall_stats <- doc_data %>%
    dplyr::summarize(
      docs = dplyr::n(),
      mean_dominance = mean(!!dplyr::sym(dominance_col), na.rm = TRUE),
      std_dev = stats::sd(!!dplyr::sym(dominance_col), na.rm = TRUE),
      coef_var = std_dev / mean_dominance
    )
  
  result_table <- rbind(result_table, data.frame(
    category = "Overall",
    level = "Category",
    docs = overall_stats$docs,
    std_dev = round(overall_stats$std_dev, 3),
    coef_var = round(overall_stats$coef_var, 3),
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
      std_dev = round(overall_stats$std_dev, 3),
      coef_var = round(overall_stats$coef_var, 3),
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
            mean_dominance = mean(!!dplyr::sym(dominance_col), na.rm = TRUE),
            std_dev = stats::sd(!!dplyr::sym(dominance_col), na.rm = TRUE),
            coef_var = std_dev / mean_dominance
          )
        
        # Add to results with corrected formatting
        result_table <- rbind(result_table, data.frame(
          category = paste0("- ", val),
          level = "Sub-category",
          docs = group_stats$docs,
          std_dev = round(group_stats$std_dev, 3),
          coef_var = round(group_stats$coef_var, 3),
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
            mean_dominance = mean(!!dplyr::sym(dominance_col), na.rm = TRUE),
            std_dev = stats::sd(!!dplyr::sym(dominance_col), na.rm = TRUE),
            coef_var = std_dev / mean_dominance
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
          std_dev = round(group_stats$std_dev, 3),
          coef_var = round(group_stats$coef_var, 3),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(result_table)
}