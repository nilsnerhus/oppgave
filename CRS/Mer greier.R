library(httr)
library(jsonlite)
library(tidyverse)

url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.3/DAC..1000.100._T._T.D.Q._T..?startPeriod=2019&dimensionAtObservation=AllDimensions"

# Make the API request
response <- GET(url)
if (http_status(response)$category == "Success") {
  json_data <- fromJSON(content(response, "text"))
  
  # Extract dimensions
  dimensions <- json_data$data$structure$dimensions$observation
  
  # Print all dimension IDs to find climate adaptation
  print("Available dimensions:")
  print(dimensions$id)
  
  # Extract observations
  observations <- json_data$data$dataSets[[1]]$observations
  
  # Create data frame from the first few observations to examine structure
  sample_size <- min(10, length(observations))
  sample_obs <- observations[1:sample_size]
  
  # Process and display the sample
  sample_data <- data.frame()
  
  # Function to get dimension value name by index
  get_dim_value <- function(dim_index, value_index) {
    # Adjust for 0-based indexing
    value_index <- value_index + 1
    if (dim_index <= length(dimensions$values) && 
        value_index <= length(dimensions$values[[dim_index]]$name)) {
      return(dimensions$values[[dim_index]]$name[value_index])
    }
    return(NA)
  }
  
  # Process sample observations
  for (obs_key in names(sample_obs)) {
    # Parse key indices
    key_parts <- as.numeric(strsplit(obs_key, ":")[[1]])
    
    # Create row with dimension names and values
    row_data <- data.frame(key = obs_key, value = sample_obs[[obs_key]][[1]])
    
    # Add dimension values
    for (i in 1:length(dimensions$id)) {
      if (i <= length(key_parts)) {
        dim_name <- dimensions$id[i]
        dim_value <- get_dim_value(i, key_parts[i])
        row_data[[dim_name]] <- dim_value
      }
    }
    
    # Add to sample data
    sample_data <- rbind(sample_data, row_data)
  }
  
  # Display the sample
  print("Sample data structure:")
  print(sample_data)
  
  # Check if climate adaptation marker is present
  if ("CLIMATE_ADAPTATION" %in% dimensions$id) {
    print("Climate adaptation marker found!")
  } else {
    print("Climate adaptation marker not found. You may need a different API endpoint.")
    print("Consider checking for similar fields:")
    for (id in dimensions$id) {
      if (grepl("CLIMAT", id, ignore.case = TRUE) | 
          grepl("ADAPT", id, ignore.case = TRUE) |
          grepl("RIO", id, ignore.case = TRUE) |
          grepl("MARKER", id, ignore.case = TRUE)) {
        print(paste("  - Possible related field:", id))
      }
    }
  }
}