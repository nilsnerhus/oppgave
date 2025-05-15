#' @title Create explained variance table based on whole corpus
#' @description Calculates how much of the total variation in dominance values 
#'   can be explained by each category and subcategory, following the approach
#'   recommended by Roberts et al. (2019).
#'
#' @param doc_data Data frame containing document-level dominance values
#' @param dominance_col Name of column containing dominance values
#' @param category_map List mapping higher-level categories to dimensions
#'
#' @return A data frame showing percentage of total variance explained by each category
create_explained_table <- function(
    doc_data,
    dominance_col,
    category_map
) {
  # Import the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Initialize results table with same structure as other tables
  result_table <- data.frame(
    category = character(),
    level = character(),
    docs = integer(),
    explained_var = numeric(),  # Percentage of total variance explained
    stringsAsFactors = FALSE
  )
  
  ## --- Calculate total variance from whole corpus -------------------------
  # Determine the total variance in dominance values
  total_variance <- stats::var(doc_data[[dominance_col]], na.rm = TRUE)
  total_docs <- nrow(doc_data)
  total_mean <- mean(doc_data[[dominance_col]], na.rm = TRUE)
  
  ## --- Add overall row ----------------------------------------------------
  # Overall row has 0% explained variance (reference point)
  result_table <- rbind(result_table, data.frame(
    category = "Overall",
    level = "Category",
    docs = total_docs,
    explained_var = 0,  # Overall row has 0% explained by definition
    stringsAsFactors = FALSE
  ))
  
  ## --- Process each category and subcategory ------------------------------
  for (category_name in names(category_map)) {
    # Get dimensions for this category
    category_dimensions <- category_map[[category_name]]
    
    # Skip empty categories
    if (length(category_dimensions) == 0) {
      next
    }
    
    # For categorical dimensions (Income, Region)
    if (category_name %in% c("Income", "Region")) {
      dim <- category_dimensions[1]  # Should be one dimension per category
      
      # Use ANOVA to calculate category-level explained variance
      formula <- stats::as.formula(paste(dominance_col, "~", dim))
      model <- stats::aov(formula, data = doc_data)
      anova_table <- stats::anova(model)
      
      # Extract explained variance (R-squared)
      between_ss <- anova_table$`Sum Sq`[1]
      total_ss <- sum(anova_table$`Sum Sq`)
      explained <- between_ss / total_ss * 100  # Convert to percentage
      
      # Add category row
      result_table <- rbind(result_table, data.frame(
        category = category_name,
        level = "Category",
        docs = total_docs,
        explained_var = round(explained, 1),
        stringsAsFactors = FALSE
      ))
      
      # Get unique values for subcategories
      values <- unique(doc_data[[dim]])
      values <- values[!is.na(values)]
      
      # Process each subcategory as contribution to TOTAL variance
      for (val in values) {
        # Count documents in this subcategory
        subcat_docs <- sum(doc_data[[dim]] == val, na.rm = TRUE)
        
        # Skip if too few documents
        if (subcat_docs < 3) {
          next
        }
        
        # Calculate subcategory's contribution to TOTAL explained variance
        # This shows what percentage of overall variation this specific group explains
        group_mean <- mean(doc_data[[dominance_col]][doc_data[[dim]] == val], na.rm = TRUE)
        
        # Between group variance contribution formula
        # (group size / total) * ((group mean - overall mean)^2 / total variance) * 100
        contribution <- (subcat_docs / total_docs) * 
          ((group_mean - total_mean)^2 / total_variance) * 100
        
        # Add to results with corrected formatting
        result_table <- rbind(result_table, data.frame(
          category = paste0("- ", val),
          level = "Sub-category",
          docs = subcat_docs,
          explained_var = round(contribution, 1),
          stringsAsFactors = FALSE
        ))
      }
      
    } else if (category_name == "Geography") {
      # For Geography category (binary dimensions)
      
      # Calculate overall category explained variance with multiple regression
      formula_terms <- paste(category_dimensions, collapse = " + ")
      formula <- stats::as.formula(paste(dominance_col, "~", formula_terms))
      
      # Use robust method to handle potential collinearity
      tryCatch({
        model <- stats::lm(formula, data = doc_data)
        r_squared <- summary(model)$r.squared * 100  # Convert to percentage
      }, error = function(e) {
        # Fallback if regression fails: sum individual contributions
        r_squared <- 0
        for (dim in category_dimensions) {
          single_formula <- stats::as.formula(paste(dominance_col, "~", dim))
          single_model <- stats::aov(single_formula, data = doc_data)
          anova_table <- stats::anova(single_model)
          between_ss <- anova_table$`Sum Sq`[1]
          total_ss <- sum(anova_table$`Sum Sq`)
          r_squared <- r_squared + (between_ss / total_ss * 100)
        }
        # Cap at 100% if overlapping effects add up to more
        r_squared <- min(r_squared, 100)
      })
      
      # Add category row
      result_table <- rbind(result_table, data.frame(
        category = category_name,
        level = "Category",
        docs = total_docs,
        explained_var = round(r_squared, 1),
        stringsAsFactors = FALSE
      ))
      
      # Process each binary dimension individually
      for (dim in category_dimensions) {
        # Only process TRUE values for subcategories
        subcat_docs <- sum(doc_data[[dim]] == TRUE, na.rm = TRUE)
        
        # Skip if too few documents
        if (subcat_docs < 3) {
          next
        }
        
        # Calculate subcategory's contribution to TOTAL explained variance
        group_mean <- mean(doc_data[[dominance_col]][doc_data[[dim]] == TRUE], na.rm = TRUE)
        
        # Between group variance contribution formula
        contribution <- (subcat_docs / total_docs) * 
          ((group_mean - total_mean)^2 / total_variance) * 100
        
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
          docs = subcat_docs,
          explained_var = round(contribution, 1),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Sort the table for better presentation
  result_table <- result_table[order(match(result_table$category, 
                                           c("Overall", names(category_map)))), ]
  
  return(result_table)
}