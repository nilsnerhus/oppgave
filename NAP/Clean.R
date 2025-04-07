library(napr)
library(tidytext)
library(quanteda)
library(stm)
library(dplyr)

naps_raw <- napr::nap_data

naps <- naps_raw %>%
  unnest_tokens(word,pdf_text) %>%
  anti_join(stop_words)
