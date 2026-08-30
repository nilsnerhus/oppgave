# AGENTS.md

Guidance for AI agents (and humans) working in this repo. Keep this file current.

## What this is

This is a Master's thesis project (handed in Sept 2025). The thesis discusses discursive power and the possibility of just climate adaptation. As its empirical data it creates a structural topic modeling (STM) of National Adaptation Plans (NAPs) submitted to the UNFCCC [NAP Central](https://napcentral.org/submitted-naps).

The project is written as a Quarto book (see `quarto/`), with the analysis done in R-scripts. The current status is as follows:

1. The pipeline has been rewritten:

- [x] Scraping (`R/scrape.R`): Scrapes multiple UN websites to create `csv/naps.csv`, a clean and readable csv file with country, date, region, income_level, geography and text.
- [x] Modelling (`R/model.R`): Tokenizes and cleans the text column before it creates `rds/model.rds`, the stm-object.
- [x] Results (`R/results.R`): Takes the model output and creates the results based on the metadata and metrics we were after. Two outputs, `csv/topics.csv` and `csv/metrics.csv`, as well as metadata.

2. Deal with changes to the data:

- [x] Pin the handed-in thesis in git
- [ ] Remove the least "scientific" data (dominance and std_error)
- [ ] Write a function to get the inline values out of the csvs
- [ ] Lightly rewrite the thesis to the new values
- [ ] Spin the data analysis into its own package, so that it is accessible outside this project
- [ ] Render it and post the new verison online

## Conventions

- Plain tidyverse: tibbles; readr/dplyr/tidyr/forcats/purrr/stringr
- Stage scripts read top-to-bottom: data flow first (one named table per step), helpers grouped below in flow order, run at the end.
