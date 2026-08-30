# AGENTS.md

Guidance for AI agents (and humans) working in this repo. Keep this file current.

## What this is

This is a Master's thesis project (handed in Sept 2025). The thesis discusses discursive power and the possibility of just climate adaptation. As its empirical data it creates a structural topic modeling (STM) of National Adaptation Plans (NAPs) submitted to the UNFCCC [NAP Central](https://napcentral.org/submitted-naps).

The project is written as a Quarto book (see `quarto/`), with the analysis done in R-scripts. The current status is as follows:

## Conventions

- Plain tidyverse: tibbles; readr/dplyr/tidyr/forcats/purrr/stringr
- Stage scripts read top-to-bottom: data flow first (one named table per step), helpers grouped below in flow order, run at the end.

## The pipeline

- [x] Scraping (`R/scrape.R`): Scrapes multiple UN websites to create `csv/naps.csv`, a clean and readable csv file with country, date, region, income_level, geography and text.
- [x] Modelling (`R/model.R`): Tokenizes and cleans the text column before it creates `rds/model.rds`, the stm-object.
- [x] Results (`R/results.R`): Takes the model output and creates the results based on the metadata and metrics we were after. Two outputs, `csv/topics.csv` and `csv/metrics.csv`, as well as metadata.

## Future plans

The goal is to fork this repo, create an R-package and that way make the findings more accessible. The plan is to do a rewrite from "both sides":

For this repo:

- Remove large files from history
- Remove large file and and squash commits into a readable history
- Change directory- and file names to be easier to read
- Never break the data that the thesis relies on

For the fork:

- Remove the least "scientific" data (dominance and std_error)
- Use the `tidytext`-approach to preparing the text
- Add R package infrastructure

The current repo is a mix of both ends, and a start to the work would be to separate the changes into two branches. One *core*, where as much refacoring is done without changing the rendered text and its inline values, and one *fork*, where breaking changes are allowed.
