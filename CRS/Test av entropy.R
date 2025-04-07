# Load libraries
library(entropy)
library(dplyr)
library(ggplot2)
library(reshape2)

# Let's use the built-in Titanic dataset as an example
# This is a good analog for categorical funding/discourse data
data(Titanic)
titanic_df <- as.data.frame(Titanic)

# Create some example groupings that could represent donors vs recipients in your research
# We'll look at survival patterns by class and sex

# First, let's calculate the entropy of survival by passenger class
class_survival <- titanic_df %>%
  group_by(Class) %>%
  summarize(
    total = sum(Freq),
    survived = sum(ifelse(Survived == "Yes", Freq, 0)),
    died = sum(ifelse(Survived == "No", Freq, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    prob_survived = survived / total,
    prob_died = died / total
  )

# Function to calculate entropy and normalized entropy for binary outcomes
calculate_binary_entropy <- function(prob_vector) {
  # Remove any 0s (which would cause log2(0) = -Inf)
  prob_vector <- prob_vector[prob_vector > 0]
  
  # Calculate entropy
  entropy_value <- -sum(prob_vector * log2(prob_vector))
  
  # Maximum possible entropy for binary outcome is 1 bit
  max_entropy <- log2(2)  # log2 of number of possible outcomes
  
  # Normalized entropy
  normalized_entropy <- entropy_value / max_entropy
  
  return(list(
    entropy = entropy_value,
    max_entropy = max_entropy,
    normalized_entropy = normalized_entropy
  ))
}

# Calculate entropy for each class
class_entropies <- class_survival %>%
  rowwise() %>%
  mutate(
    entropy_result = list(calculate_binary_entropy(c(prob_survived, prob_died))),
    entropy = entropy_result$entropy,
    normalized_entropy = entropy_result$normalized_entropy
  ) %>%
  select(Class, total, entropy, normalized_entropy)

# Let's also calculate entropy by Sex
sex_survival <- titanic_df %>%
  group_by(Sex) %>%
  summarize(
    total = sum(Freq),
    survived = sum(ifelse(Survived == "Yes", Freq, 0)),
    died = sum(ifelse(Survived == "No", Freq, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    prob_survived = survived / total,
    prob_died = died / total
  )

# Calculate entropy for each sex
sex_entropies <- sex_survival %>%
  rowwise() %>%
  mutate(
    entropy_result = list(calculate_binary_entropy(c(prob_survived, prob_died))),
    entropy = entropy_result$entropy,
    normalized_entropy = entropy_result$normalized_entropy
  ) %>%
  select(Sex, total, entropy, normalized_entropy)

# Let's also calculate overall entropy
overall <- titanic_df %>%
  summarize(
    total = sum(Freq),
    survived = sum(ifelse(Survived == "Yes", Freq, 0)),
    died = sum(ifelse(Survived == "No", Freq, 0))
  ) %>%
  mutate(
    prob_survived = survived / total,
    prob_died = died / total
  )

overall_entropy <- calculate_binary_entropy(c(overall$prob_survived, overall$prob_died))

# Print results
cat("Shannon Entropy by Passenger Class:\n")
print(class_entropies)

cat("\nShannon Entropy by Sex:\n")
print(sex_entropies)

cat("\nOverall Shannon Entropy:\n")
cat("Entropy:", overall_entropy$entropy, "bits\n")
cat("Normalized Entropy:", overall_entropy$normalized_entropy, "\n")

# Visualization of entropy values
class_plot_data <- class_entropies %>% select(Category = Class, NormalizedEntropy = normalized_entropy) %>% mutate(Group = "Class")
sex_plot_data <- sex_entropies %>% select(Category = Sex, NormalizedEntropy = normalized_entropy) %>% mutate(Group = "Sex")
overall_data <- data.frame(Category = "Overall", NormalizedEntropy = overall_entropy$normalized_entropy, Group = "Overall")

plot_data <- rbind(class_plot_data, sex_plot_data, overall_data)

# Create bar plot
ggplot(plot_data, aes(x = Category, y = NormalizedEntropy, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Normalized Entropy of Survival Outcomes",
       subtitle = "Values closer to 1 indicate more unpredictable/diverse outcomes",
       y = "Normalized Entropy",
       x = "") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom")

# Let's also show how we can use entropy for more complex categorical data (beyond binary)
# We'll look at survival distribution across all combinations of Class and Sex

class_sex_survival <- titanic_df %>%
  group_by(Class, Sex) %>%
  summarize(
    total_in_group = sum(Freq),
    .groups = "drop"
  )

# Calculate proportion in each category
class_sex_survival <- class_sex_survival %>%
  mutate(proportion = total_in_group / sum(total_in_group))

# Calculate entropy using the entropy package
complex_entropy <- entropy::entropy(class_sex_survival$proportion, unit = "log2")
max_complex_entropy <- log2(nrow(class_sex_survival))
normalized_complex_entropy <- complex_entropy / max_complex_entropy

cat("\nEntropy of distribution across Class-Sex categories:\n")
cat("Entropy:", complex_entropy, "bits\n")
cat("Maximum possible entropy:", max_complex_entropy, "bits\n")
cat("Normalized entropy:", normalized_complex_entropy, "\n")

# Create a heatmap to visualize the distribution
# This helps interpret what the entropy value means
distribution_matrix <- titanic_df %>%
  group_by(Class, Sex) %>%
  summarize(Count = sum(Freq), .groups = "drop") %>%
  mutate(Proportion = Count / sum(Count))

# Plot heatmap
ggplot(distribution_matrix, aes(x = Sex, y = Class, fill = Proportion)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Distribution Across Categories",
       subtitle = paste("Entropy =", round(complex_entropy, 2), "bits, Normalized =", round(normalized_complex_entropy, 2)),
       fill = "Proportion") +
  theme_minimal()
