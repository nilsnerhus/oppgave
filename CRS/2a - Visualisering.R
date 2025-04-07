# Load necessary libraries
library(tidyverse)

# Load the prepared dataset from Phase 1
crs_analysis <- read_csv("clean_2023.csv")

# 1. Project size distribution comparison
size_dist_plot <- ggplot(crs_analysis, aes(x = log_amount, fill = as.factor(adaptation_flag))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of project sizes: Adaptation vs. Non-adaptation",
       x = "Log of disbursement amount", 
       fill = "Adaptation flag") +
  scale_fill_discrete(labels = c("Non-adaptation", "Adaptation")) +
  theme_minimal()

print(size_dist_plot)  # In Quarto, this will display in your document

# 2. Sector focus visualization
# First, summarize the data
sector_summary <- crs_analysis %>%
  group_by(sector_group, adaptation_flag) %>%
  summarize(total_amount = sum(exp(log_amount) - 1, na.rm = TRUE),
            count = n(),
            .groups = "drop") %>%
  # Calculate percentage within adaptation category for comparison
  group_by(adaptation_flag) %>%
  mutate(percent_of_total = total_amount / sum(total_amount) * 100)

# Create the plot
sector_focus_plot <- sector_summary %>%
  ggplot(aes(x = reorder(sector_group, percent_of_total), 
             y = percent_of_total, 
             fill = as.factor(adaptation_flag))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Adaptation vs. Non-adaptation funding by sector",
       subtitle = "As percentage of total funding in each category",
       y = "Percentage of total funding",
       x = "Sector",
       fill = "Adaptation flag") +
  scale_fill_discrete(labels = c("Non-adaptation", "Adaptation")) +
  theme_minimal()

print(sector_focus_plot)  # In Quarto, this will display in your document

# 3. Additional visualization: Top donors comparison
donor_summary <- crs_analysis %>%
  group_by(donor, adaptation_flag) %>%
  summarize(total_amount = sum(exp(log_amount) - 1, na.rm = TRUE),
            .groups = "drop") %>%
  # Keep only top donors for readability
  group_by(adaptation_flag) %>%
  arrange(desc(total_amount)) %>%
  slice_head(n = 10) %>%
  ungroup()

donor_plot <- ggplot(donor_summary, aes(x = reorder(donor, total_amount), 
                                        y = total_amount / 1e9, 
                                        fill = as.factor(adaptation_flag))) +
  geom_col() +
  facet_wrap(~adaptation_flag, scales = "free_y", 
             labeller = labeller(adaptation_flag = c("0" = "Non-adaptation", "1" = "Adaptation"))) +
  coord_flip() +
  labs(title = "Top donors by adaptation and non-adaptation funding",
       y = "Total amount (billion USD)",
       x = "Donor",
       fill = "Adaptation flag") +
  theme_minimal() +
  theme(legend.position = "none")

print(donor_plot)  # In Quarto, this will display in your document


# Calculate adaptation penetration by sector
sector_mainstreaming <- crs_analysis %>%
  group_by(sector_group, year) %>%
  summarize(
    adaptation_rate = mean(adaptation_flag, na.rm = TRUE),
    total_projects = n(),
    .groups = "drop"
  ) %>%
  filter(total_projects >= 10)  # Filter for statistical reliability

# 5. Create a heatmap showing mainstreaming over time by sector
ggplot(sector_mainstreaming, aes(x = year, y = sector_group, fill = adaptation_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen", 
                      labels = scales::percent_format()) +
  labs(title = "Climate Adaptation Mainstreaming by Sector",
       subtitle = "Percentage of projects flagged for adaptation within each sector",
       x = "Year",
       y = "Sector",
       fill = "Adaptation Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load the prepared dataset from Phase 1
crs_analysis <- read_csv("clean_2023.csv")

# 1. DISCURSIVE POWER: Adaptation by Sector Priority
adaptation_by_sector <- crs_analysis %>%
  group_by(sector_group) %>%
  summarize(
    adaptation_rate = mean(adaptation_flag, na.rm = TRUE),
    total_projects = n(),
    total_funding = sum(exp(log_amount) - 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_projects >= 10) %>%  # Filter for statistical reliability
  arrange(desc(adaptation_rate))

# Visualization showing which sectors are most "adaptationized"
ggplot(adaptation_by_sector, aes(x = reorder(sector_group, adaptation_rate), 
                                 y = adaptation_rate)) +
  geom_col(aes(fill = total_funding), alpha = 0.8) +
  scale_fill_viridis_c(trans = "log10") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Climate Adaptation Discourse Across Sectors",
       subtitle = "Percentage of projects flagged for adaptation by sector (2023)",
       x = "Sector",
       y = "Adaptation Rate",
       fill = "Total Funding\n(log scale)") +
  theme_minimal()

# 2. MAINSTREAMING: Donor Approaches to Adaptation
donor_approaches <- crs_analysis %>%
  group_by(donor) %>%
  summarize(
    total_projects = n(),
    adaptation_rate = mean(adaptation_flag, na.rm = TRUE),
    total_funding = sum(exp(log_amount) - 1, na.rm = TRUE),
    sectors_covered = n_distinct(sector_group),
    sectors_with_adaptation = n_distinct(ifelse(adaptation_flag == 1, sector_group, NA), na.rm = TRUE),
    adaptation_coverage = sectors_with_adaptation / sectors_covered,
    .groups = "drop"
  ) %>%
  filter(total_projects >= 20) %>%  # Only include donors with sufficient data
  arrange(desc(adaptation_rate))

# Create a scatterplot showing different mainstreaming approaches
ggplot(donor_approaches, aes(x = adaptation_rate, 
                             y = adaptation_coverage,
                             size = total_funding,
                             color = total_projects)) +
  geom_point(alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = donor), 
                           size = 3,
                           max.overlaps = 8,
                           box.padding = 0.5) +
  scale_size_continuous(trans = "log10", name = "Total Funding (log scale)") +
  scale_color_viridis_c(trans = "log10", name = "Number of Projects (log scale)") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Donor Approaches to Climate Adaptation Mainstreaming",
       subtitle = "Comparing adaptation intensity vs. sectoral breadth (2023)",
       x = "Overall Adaptation Rate (% of all projects)",
       y = "Adaptation Coverage (% of sectors with adaptation)") +
  theme_minimal()

# 3. DISCURSIVE POWER: Adaptation Project Size Analysis
# This shows if adaptation projects receive more/less funding (marker of prioritization)
project_size_comparison <- crs_analysis %>%
  group_by(adaptation_flag) %>%
  summarize(
    mean_log_size = mean(log_amount, na.rm = TRUE),
    median_log_size = median(log_amount, na.rm = TRUE),
    mean_size = mean(exp(log_amount) - 1, na.rm = TRUE),
    median_size = median(exp(log_amount) - 1, na.rm = TRUE),
    total_projects = n(),
    .groups = "drop"
  )

# Create a boxplot to compare adaptation vs non-adaptation project sizes
ggplot(crs_analysis, aes(x = factor(adaptation_flag), y = log_amount, fill = factor(adaptation_flag))) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_x_discrete(labels = c("Non-adaptation", "Adaptation")) +
  scale_fill_discrete(guide = "none") +
  labs(title = "Climate Adaptation vs Non-Adaptation Project Sizes",
       subtitle = "Comparing funding allocation patterns (2023)",
       x = "Project Type",
       y = "Log of Project Amount") +
  theme_minimal()

# 4. MAINSTREAMING: Sector-Donor Heatmap
# This shows which donors are mainstreaming in which sectors
sector_donor_heatmap <- crs_analysis %>%
  # Focus on major donors for readability
  filter(donor %in% head(donor_approaches$donor, 10)) %>%
  # And on sectors with significant adaptation rates
  filter(sector_group %in% head(adaptation_by_sector$sector_group, 10)) %>%
  group_by(donor, sector_group) %>%
  summarize(
    adaptation_rate = mean(adaptation_flag, na.rm = TRUE),
    total_projects = n(),
    .groups = "drop"
  ) %>%
  filter(total_projects >= 5)  # Only include combinations with sufficient data

# Create a heatmap
ggplot(sector_donor_heatmap, aes(x = donor, y = sector_group, fill = adaptation_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen", 
                      labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Climate Adaptation Mainstreaming Patterns by Donor and Sector",
       subtitle = "Percentage of projects flagged for adaptation (2023)",
       x = "Donor",
       y = "Sector",
       fill = "Adaptation Rate") +
  theme_minimal()


# 1. First, check how many NA or infinite values you have in log_amount
na_count <- sum(is.na(crs_analysis$log_amount))
inf_count <- sum(!is.finite(crs_analysis$log_amount))
zero_count <- sum(crs_analysis$disbursement_amount == 0, na.rm = TRUE)

print(paste("NA values:", na_count))
print(paste("Infinite values:", inf_count))
print(paste("Zero disbursements:", zero_count))

# Compare sector priorities between adaptation and non-adaptation projects
sector_allocation <- crs_analysis %>%
  group_by(sector_group, adaptation_flag) %>%
  summarize(
    total_amount = sum(exp(log_amount) - 1, na.rm = TRUE),
    project_count = n(),
    .groups = "drop"
  ) %>%
  # Calculate the proportion within each adaptation category
  group_by(adaptation_flag) %>%
  mutate(
    proportion = total_amount / sum(total_amount),
    proportion_count = project_count / sum(project_count)
  ) %>%
  ungroup()

# Calculate the difference in proportions
sector_shift <- sector_allocation %>%
  select(sector_group, adaptation_flag, proportion) %>%
  pivot_wider(names_from = adaptation_flag, values_from = proportion, 
              names_prefix = "adapt_") %>%
  mutate(
    # Calculate gain/loss in proportional funding
    proportion_change = adapt_1 - adapt_0,
    # Convert to percentage points for easier interpretation
    percentage_point_change = proportion_change * 100,
    # Direction of change
    direction = ifelse(proportion_change > 0, "Gain", "Loss")
  ) %>%
  arrange(desc(proportion_change))

# Visualize the shifts
ggplot(sector_shift, aes(x = reorder(sector_group, percentage_point_change), 
                         y = percentage_point_change,
                         fill = direction)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Gain" = "#2ca02c", "Loss" = "#d62728")) +
  labs(title = "Sectoral Winners and Losers from Climate Adaptation",
       subtitle = "Percentage point difference in funding allocation",
       x = "Sector",
       y = "Percentage Point Difference (Adaptation vs. Non-adaptation)",
       fill = "Direction") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
