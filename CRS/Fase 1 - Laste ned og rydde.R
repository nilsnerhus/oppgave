library(readr)
library(dplyr)

# Read the CRS data 
crs_data <- read_delim("crs_2023.csv", delim = "|", show_col_types = FALSE)

# Create a sector mapping function using official CRS codes
create_sector_group <- function(code) {
  code_num <- as.numeric(code)
  
  # Map using the first digits of the purpose code
  if (is.na(code_num)) {
    return("Unspecified")
  } else if (code_num >= 11000 && code_num < 12000) {
    return("Education")
  } else if (code_num >= 12000 && code_num < 13000) {
    return("Health")
  } else if (code_num >= 13000 && code_num < 14000) {
    return("Population & Reproductive Health")
  } else if (code_num >= 14000 && code_num < 15000) {
    return("Water & Sanitation")
  } else if (code_num >= 15000 && code_num < 16000) {
    return("Government & Civil Society")
  } else if (code_num >= 16000 && code_num < 17000) {
    return("Other Social Infrastructure")
  } else if (code_num >= 21000 && code_num < 22000) {
    return("Transport & Storage")
  } else if (code_num >= 22000 && code_num < 23000) {
    return("Communications")
  } else if (code_num >= 23000 && code_num < 24000) {
    return("Energy")
  } else if (code_num >= 24000 && code_num < 25000) {
    return("Banking & Financial Services")
  } else if (code_num >= 25000 && code_num < 26000) {
    return("Business & Other Services")
  } else if (code_num >= 31000 && code_num < 32000) {
    return("Agriculture, Forestry & Fishing")
  } else if (code_num >= 32000 && code_num < 33000) {
    return("Industry, Mining & Construction")
  } else if (code_num >= 33000 && code_num < 34000) {
    return("Trade & Tourism")
  } else if (code_num >= 41000 && code_num < 42000) {
    return("Environmental Protection")
  } else if (code_num >= 43000 && code_num < 44000) {
    return("Disaster Prevention & Preparedness")
  } else if (code_num >= 70000 && code_num < 80000) {
    return("Humanitarian Aid")
  } else {
    return("Other")
  }
}

# Create a clean, analysis-ready dataset
clean_2023 <- crs_data %>%
  # Select only needed variables
  select(
    donor = DonorName,
    year = Year,
    sector_code = PurposeCode,
    sector_name = PurposeName,
    disbursement_amount = USD_Disbursement,
    adaptation_marker = ClimateAdaptation
  ) %>%
  # Handle data issues
  mutate(
    disbursement_amount = as.numeric(ifelse(disbursement_amount == "", NA, disbursement_amount)),
    adaptation_flag = ifelse(as.numeric(adaptation_marker) %in% c(1, 2), 1, 0),
    # Use log(x+1) transformation to handle zeros
    log_amount = log(pmax(disbursement_amount, 0) + 1),
    # Apply the sector grouping function
    sector_group = sapply(sector_code, create_sector_group)
  ) %>%
  # Select only the final variables you want
  select(donor, year, sector_group, sector_name, log_amount, adaptation_flag)

# Data quality checks
print(paste("Missing adaptation markers:", sum(is.na(clean_2023$adaptation_flag))))
print(paste("Unique sector groups created:", length(unique(clean_2023$sector_group))))

# Save the analysis dataset
write_csv(clean_2023, "clean_2023.csv")
