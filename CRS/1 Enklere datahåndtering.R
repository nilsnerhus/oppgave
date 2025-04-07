library(rsdmx)
library(tidyverse)

# SDMX query for Rio Markers dataset with climate adaptation
sdmx_url <- "https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_RIOMRKR@DF_RIOMARKERS,1.3/DAC_EC.A+S+O+LDC+LMIC+UMIC+MADCT+HIPC+LLDC+SIDS+F+E+DPGC.110+120+130+140+150+160+210+220+230+240+250+310+320+331+332+410+430+510+520+530+720+730+740+910+930+1000..2.30...Q._T..?startPeriod=2015&endPeriod=2023"

# Fetch data
sdmx_data <- readSDMX(sdmx_url) %>%
  as.data.frame()

# Clean up column names
names(rio_data) <- gsub("CLIMATE_MARKER:|OBS_VALUE", "", names(rio_data))

# Transform data to be analysis-ready
rio_clean <- rio_data %>%
  rename(
    donor = DONOR,
    recipient = RECIPIENT,
    marker = MARKER,
    flow_type = FLOW_TYPE,
    amount = value,
    year = TIME_PERIOD
  ) %>%
  # Convert amount to numeric
  mutate(
    amount = as.numeric(amount),
    year = as.numeric(year),
    # Create adaptation flag (1 = adaptation, 0 = not adaptation)
    adaptation_flag = ifelse(marker == "ADAPTATION", 1, 0)
  )

# Preview the data
glimpse(rio_clean)

# Save the cleaned dataset
write_csv(rio_clean, "rio_adaptation_data.csv")

# Basic summary statistics
summary_stats <- rio_clean %>%
  group_by(year) %>%
  summarize(
    total_adaptation_aid = sum(amount, na.rm = TRUE),
    n_projects = n()
  )

print(summary_stats)