# Methods {#sec-methods}

In this section, I explain the methods I will use to find and analyze data on climate adaptation discourse. First, I discuss the development of the Dissonance Index as the core analytical contribution of this thesis. Second, I explain the OECD CRS dataset, how it is prepared and analyzed using entropy calculations. Third, I explain the National Adaptation Plans at the UNFCCC, how they are prepared and analyzed through structural topic modeling. Finally, I describe how entropy calculations will be unified to create comparative measures of discourse fragmentation.

All data will be analyzed with the free and open-source statistical program `R` with the help of packages in the `tidyverse`. All source files for this project are available in a [GitHub repository](https://github.com/nilsnerhus/oppgave) and the documents are automatically produced in HTML, PDF, and DOCX when changes are pushed to the repository and posted on [the Internet](https://nilsnerhus.github.io/oppgave/).

## The dissonance index

The central contribution of this thesis is the development and application of a "Dissonance Index" that quantifies the degree of variation in how climate adaptation is conceptualized by different actors within the global aid system.

The Dissonance Index draws from information theory, specifically using Shannon entropy as the foundational measurement. Entropy, originally developed by Claude Shannon in 1948, measures the unpredictability or randomness in a system. When applied to discourse analysis, higher entropy values indicate greater dissonance (more diversity in conceptualizations), while lower entropy values indicate lower dissonance (more consensus in conceptualizations).

Mathematically, Shannon entropy is calculated as:

$$H = -\sum_{i=1}^{n} p(x_i) \log_2 p(x_i)$$

Where:

- $H$ is the entropy value
- $p(x_i)$ is the probability of category $i$ occurring
- $n$ is the number of possible categories

For example, if climate adaptation funding were perfectly evenly distributed across 16 sectors, each sector would receive 6.25% of the funding. The entropy calculation would be:

$$H = -16 \times (0.0625 \times \log_2 0.0625) = 4$$

This represents maximum entropy for a system with 16 categories. If funding were concentrated in just one sector (100%), the entropy would be 0, indicating complete consensus. Most real-world distributions fall somewhere between these extremes.

To normalize entropy values across datasets with different numbers of categories, I will use relative entropy, calculated as:

$$H_{rel} = \frac{H}{H_{max}} = \frac{H}{\log_2 n}$$

This scales entropy to a value between 0 and 1, where 0 represents complete concentration (minimum dissonance) and 1 represents perfect dispersion (maximum dissonance). This normalization will allow for direct comparison between different dimensions of analysis (sectors, regions, etc.) and between different datasets (OECD CRS and NAPs).

This approach bridges quantitative methods with post-structural critique from the anthropology of development, offering an empirical basis for examining discourse centralization in climate adaptation. The index allows me to quantitatively assess how centralized or diverse the understanding of climate adaptation is across donors, recipients, sectors, and regions.

## Modeling OECD data with entropy calculations

The OECD Creditor Reporting System is a collection of self-reported activities by members of the OECD. This reporting is, to the member states and officials who report, a serious exercise, but it is not coordinated beyond the guidance the OECD gives. The data I analyze covers the period from 2010 to 2023, during which donors have been able to mark projects as relevant to climate adaptation using the Rio markers system.

For the OECD data analysis, I begin by extracting all project-level data where climate adaptation is marked as either a "principal" (marker value 2) or "significant" (marker value 1) objective. Following the methodology in the provided R script, I convert adaptation markers to binary flags (1 for any adaptation relevance, 0 for none) and log-transform disbursement amounts to account for the wide range of funding magnitudes.

The data will be organized according to the standardized sector groups defined by the OECD, such as Education, Health, Water & Sanitation, Transport & Storage, Energy, Agriculture, Environmental Protection, and others. This sectoral classification follows the official OECD purpose codes and provides a consistent framework for analyzing how adaptation funding is distributed across different domains.

Using this prepared dataset, I will calculate entropy across various dimensions. The sectoral entropy measure will reveal how evenly or unevenly climate adaptation funding is distributed across sectors, with higher entropy values indicating more dispersed funding and lower values suggesting concentration in specific sectors. Similarly, geographic entropy will assess the concentration of adaptation funding across recipient countries and regions by grouping recipients by region and income level using World Bank classifications.

I will also examine donor entropy to understand how consistently different donors allocate adaptation funding across sectors. For each major donor, the sectoral entropy of their adaptation portfolio will help identify patterns of consensus or divergence among donors. Finally, comparative entropy analyses between adaptation funding and general aid patterns will reveal whether climate adaptation represents a distinct discourse or simply mirrors existing aid patterns.

For the calculation of sectoral entropy, I will use the following procedure:

```{r}
#| eval: false

# Calculate proportions of adaptation funding by sector
sector_proportions <- aid_data %>%
  filter(adaptation_flag == 1) %>%
  group_by(sector_group) %>%
  summarize(total_log_amount = sum(log_amount, na.rm = TRUE)) %>%
  mutate(proportion = total_log_amount / sum(total_log_amount))

# Calculate Shannon entropy
sectoral_entropy <- -sum(sector_proportions$proportion * 
                         log2(sector_proportions$proportion), na.rm = TRUE)

# Calculate maximum possible entropy for this number of categories
max_entropy <- log2(nrow(sector_proportions))

# Calculate relative entropy (normalized between 0 and 1)
relative_entropy <- sectoral_entropy / max_entropy
```

To validate the entropy calculations, I will compare the results to randomized data to establish statistical significance, testing whether observed patterns of dissonance differ significantly from random chance. This will involve creating multiple permutations of the dataset with randomly assigned adaptation flags and comparing the resulting entropy distributions to the observed values.

A key limitation of this dataset is that it relies on self-reported data from donors, with potential inconsistencies in how adaptation is coded. Different donors may interpret the climate adaptation marker differently, leading to variations that reflect reporting practices rather than substantive differences in approach. However, this is appropriate for my study as I am interested in what is coded as climate adaptation to analyze what actors think, not the other way around. I will not compare across donors, so inconsistency in reporting is accounted for. As I have worked with this dataset previously during my internship with CONCITO, I am familiar with its functioning.

The numbers will be retrieved by calling the OECD's API. This ensures updated and reproducible numbers for analysis.

## Analyzing National Adaptation Plans through structural topic modeling

The National Adaptation Plans (NAPs) are created through collaboration between various government actors and climate adaptation projects within international organizations like the World Bank. These documents represent recipient countries' official conceptualizations of climate adaptation and their planned responses.

For the NAP analysis, I will collect all available NAPs submitted to the UNFCCC, which comprises approximately 30-40 documents. I will then preprocess the text through tokenization, removal of stopwords, stemming, and creating a document-term matrix.

Each document will be tagged with metadata including country income level, geographic region, special status (SIDS, LDC, neither), and year of submission. This metadata will serve as the basis for grouping documents when calculating entropy measures.

To analyze discourse patterns in these documents, I will employ structural topic modeling (STM) using the `stm` package in R. Unlike traditional topic modeling, STM incorporates document metadata as covariates, allowing me to examine how topics correlate with factors like income level or regional location. I will determine the optimal number of topics (likely between 10-20) through model fit statistics including semantic coherence, exclusivity, and held-out likelihood.

The focus of this analysis is not on the substantive content of the topics but rather on their distribution patterns across different document categories. The output of the STM analysis will provide the prevalence of each topic within each document and its correlation with document metadata, creating the probability distributions needed for entropy calculations.

I will then convert these topic prevalences into probability distributions suitable for entropy calculation. For example, I might examine the distribution of topics within NAPs from different income groups:

```{r}
#| eval: false
# Example R code for calculating entropy in NAP topic distributions
library(stm)
library(tidyverse)

# Assuming 'nap_stm' contains our fitted structural topic model
# and 'nap_metadata' contains document metadata

# Extract topic proportions for each document
topic_props <- nap_stm$theta

# Calculate average topic distribution by income group
topic_by_income <- data.frame(
  topic_props,
  income_group = nap_metadata$income_level
) %>%
  group_by(income_group) %>%
  summarize(across(starts_with("X"), mean)) %>%
  pivot_longer(cols = -income_group, 
               names_to = "topic", 
               values_to = "proportion")

# Calculate entropy within each income group
entropy_by_income <- topic_by_income %>%
  group_by(income_group) %>%
  summarize(
    entropy = -sum(proportion * log2(proportion), na.rm = TRUE),
    max_entropy = log2(n()),
    relative_entropy = entropy / max_entropy
  )
```

This approach will allow me to calculate intra-group entropy (within defined groups such as low-income countries), inter-group entropy (between different groups), and overall corpus entropy (across the entire collection of documents).

Higher intra-group entropy would indicate more diverse conceptualizations of adaptation within that group, while lower values would suggest greater consensus. Large differences between intra-group and inter-group entropy would indicate distinctive discourse patterns associated with particular groups.

To ensure the reliability of topic models, I will use cross-validation by splitting the NAP corpus into training and testing sets to verify that identified topics are stable across different subsets of documents. I will also vary the parameters of topic models (number of topics, priors, etc.) to ensure findings are not artifacts of specific model settings.

A significant limitation in this approach is that NAPs represent official government perspectives and may not capture grassroots or local conceptualizations of adaptation. Power dynamics influence which perspectives are represented in official documents, potentially obscuring alternative or marginalized viewpoints. Additionally, topic modeling requires interpretive decisions in determining the meaning of topics, which may introduce researcher bias. Despite these limitations, the analysis of NAPs provides valuable insight into how recipient countries officially conceptualize climate adaptation, which is central to understanding discourse patterns in international climate governance.

## Unifying entropy calculations for comparative analysis

The final methodological step will be to unify the entropy calculations from both datasets to create a comprehensive picture of adaptation discourse fragmentation. This will allow me to compare the degree of centralization or diversification across different dimensions and between different actor types.

For each dimension of analysis (sectors, regions, topics, etc.), I will calculate and report absolute entropy values, relative entropy (normalized to 0-1 scale), benchmark comparisons between adaptation and general aid, and statistical significance based on comparison to random distributions.

To interpret these values, I will establish general thresholds for categorizing levels of discourse dissonance. Values between 0.00-0.33 indicate low dissonance with high consensus and centralized discourse. Values between 0.34-0.66 represent moderate dissonance with partial consensus. Values between 0.67-1.00 show high dissonance with low consensus and highly decentralized discourse.

These thresholds are preliminary and may be adjusted based on the empirical distributions observed in the data. The key analytical focus will be on comparative patterns rather than absolute values.

I will visualize these patterns through heat maps showing entropy values across different dimensions, comparative charts highlighting differences between actor types, and network diagrams illustrating patterns of discourse fragmentation.

The combination of entropy measurements across different datasets and dimensions will provide a comprehensive picture of how centralized or fragmented climate adaptation discourse is in global governance. This directly connects to the theoretical framework by empirically assessing the degree to which adaptation discourse reflects a diversity of epistemological and ontological positions or the dominance of a singular, "anglobal" discourse.

By focusing deliberately on official discourse in policy documents and funding decisions, this methodology does not seek to uncover some "true" or "authentic" local perspective on adaptation. Rather, it aims to establish how fragmented the official discourse actually is and along what lines this fragmentation occurs. This focus on formal discourse is appropriate given that these official conceptualizations shape funding flows, policy priorities, and ultimately, the material reality of adaptation interventions.

This methodological approach thus provides a novel way to empirically investigate the theoretical questions at the heart of this thesis: To what extent does climate adaptation discourse show dissonance across different actors, sectors, and regions, and what does this reveal about power dynamics in global climate governance?