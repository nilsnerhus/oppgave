library(finalfit)
library(dplyr)

# Load your cleaned dataset
clean_2023 <- read_csv("clean_2023.csv", show_col_types = FALSE)

# Explanatory variables
explanatory <- c("donor", "sector_group", "log_amount", "year")

# Simple logistic regression with clear outputs
model_output <- clean_2023 %>%
  finalfit(dependent = "adaptation_flag", 
           explanatory = explanatory,
           metrics = TRUE)

# View the results
model_output

# Extract odds ratios for interpretation
or_table <- model_output$or
print(or_table)

# Save model results for reference in Phase 3
write_rds(model_output, "adaptation_model_results.rds")