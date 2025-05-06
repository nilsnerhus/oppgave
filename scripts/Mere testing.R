if(!exists("utils")) source("scripts/utils.R")
if(!exists("find_dissonance")) source("scripts/find_dissonance.R")
if(!exists("bullseye")) source("scripts/bullseye.R")

df_long <- readRDS("data/extract_topic_props.rds")
df <- df_long$data$data

# Calculate dissonance
dissonance <- find_dissonance(df, value_col = "Proportion")
dissonance

vis <- bullseye(dissonance)

vis

dis_africa <- find_dominance(topic_data, filter_col = "region", filter_value = "Sub-Saharan Africa")

vis_africa <- bullseye(dis_africa)

vis_africa

dis_sids <- find_dom