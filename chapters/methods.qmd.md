# Methods {#sec-methods}

In this section, I explain the methods I will use to find and analyze data on climate adaptation discourse. First, I discuss the development of the Dissonance Index as the core analytical contribution of this thesis. Second, I explain the OECD CRS dataset, how it is prepared and analyzed using entropy calculations. Third, I explain the National Adaptation Plans at the UNFCCC, how they are prepared and analyzed through structural topic modeling.

All data will be analyzed with the free and open-source statistical program `R` with the help of packages in the `tidyverse`. All source files for this project are available in a [GitHub repository](https://github.com/nilsnerhus/oppgave) and the documents are automatically produced in HTML, PDF, and DOCX when changes are pushed to the repository and posted on [the Internet](https://nilsnerhus.github.io/oppgave/).

## The dissonance index

The central contribution of this thesis is the development and application of a "Dissonance Index" that quantifies the degree of variation in how climate adaptation is conceptualized by different actors within the global aid system.

The Dissonance Index draws from information theory, specifically using entropy as the foundational measurement. Entropy, originally developed by Claude Shannon in 1948, measures the unpredictability or randomness in a system. When applied to discourse analysis, higher entropy values indicate greater dissonance (more diversity in conceptualizations), while lower entropy values indicate lower dissonance (more consensus in conceptualizations).

This approach bridges quantitative methods with post-structural critique from the anthropology of development, offering an empirical basis for examining discourse centralization in climate adaptation. The index allows me to quantitatively assess how centralized or diverse the understanding of climate adaptation is across donors, recipients, sectors, and regions.

Despite the strengths of this approach, it is important to acknowledge that entropy as a measure captures dispersion but not the qualitative content of discourse. The index shows us patterns of agreement or disagreement, but not the substance of what is being agreed upon. Additionally, by focusing on formal, documented discourse, this approach may miss conceptualizations that exist outside official channels. 

## Modeling OECD data with entropy calculations

The OECD Creditor Reporting System is a collection of self-reported activities by members of the OECD. This reporting is, to the member states and officials who report, a serious exercise, but it is not coordinated beyond the guidance the OECD gives. The data I analyze covers the period from 2010 to 2023, during which donors have been able to mark projects as relevant to climate adaptation using the Rio markers system.

For the OECD data analysis, I begin by extracting all project-level data where climate adaptation is marked as either a "principal" or "significant" objective. I then convert adaptation markers to binary flags and log-transform dollar amounts to account for the wide range of funding magnitudes. This data is then organized by donor country, recipient country, sector, geographic region, and income group.

Using the prepared dataset, I calculate entropy across various dimensions to measure dissonance in climate adaptation funding patterns. For sectoral entropy, I measure how evenly or unevenly climate adaptation funding is distributed across sectors. For geographic entropy, I assess the concentration of adaptation funding across recipient countries. I also conduct comparative entropy analyses, comparing adaptation funding patterns with general aid patterns to identify sectors or regions with significant differences.

To validate the entropy calculations, I compare the results to randomized data to establish statistical significance, testing whether observed patterns of dissonance differ significantly from random chance. This helps ensure that the patterns identified represent meaningful variation rather than random noise in the data.

A key limitation of this dataset is that it relies on self-reported data from donors, with potential inconsistencies in how adaptation is coded. Different donors may interpret the climate adaptation marker differently, leading to variations that reflect reporting practices rather than substantive differences in approach. However, this is appropriate for my study as I am interested in what is coded as climate adaptation to analyze what actors think, not the other way around. I will not compare across donors, so inconsistency in reporting is accounted for. As I have worked with this dataset previously during my internship with CONCITO, I am familiar with its functioning.

The numbers will be retrieved by calling the OECD's API. This ensures updated and reproducible numbers for analysis.

## Analyzing National Adaptation Plans through structural topic modeling

The National Adaptation Plans are created through collaboration between various government actors and climate adaptation projects within international organizations like the World Bank. These documents represent recipient countries' official conceptualizations of climate adaptation and their planned responses.

For the NAP analysis, I collect all available NAPs submitted to the UNFCCC, which comprises approximately 30-40 documents. I then preprocess the text through tokenization (breaking text into individual words), removal of stopwords (common words with little semantic value), and stemming (reducing words to their root form). Each document is tagged with metadata including country income level, geographic region, special status (SIDS, LDC), and year of submission.

To analyze discourse patterns in these documents, I employ structural topic modeling (STM), which extends traditional topic modeling by incorporating document metadata. The `stm` package in R is used to identify latent topics across the corpus of NAP documents, with the optimal number of topics determined through model fit statistics. Key terms associated with each topic are examined to interpret their substantive meaning.

After identifying topics, I analyze how topic prevalence correlates with document metadata, identifying systematic variations in adaptation discourse across different country groups. The output of this process will be a set of topics and the likelihood of them being present within different categories.

I then calculate the distribution of topics within and across metadata groupings, applying entropy measurements to quantify discourse dissonance within groups (intra-group entropy) and between groups (inter-group entropy). This allows me to determine whether certain regions, income groups, or special categories like SIDS exhibit distinctive discourse patterns.

To ensure the reliability of topic models, I use cross-validation by splitting the NAP corpus into training and testing sets to verify that identified topics are stable across different subsets of documents. I also vary the parameters of topic models to ensure findings are not artifacts of specific model settings. This validation process helps confirm that the identified discourse patterns are robust and not simply the result of arbitrary analytical choices.

A significant limitation in this approach is that NAPs represent official government perspectives and may not capture grassroots or local conceptualizations of adaptation. Power dynamics influence which perspectives are represented in official documents, potentially obscuring alternative or marginalized viewpoints. Additionally, topic modeling requires interpretive decisions in determining the meaning of topics, which may introduce researcher bias. Despite these limitations, the analysis of NAPs provides valuable insight into how recipient countries officially conceptualize climate adaptation, which is central to understanding discourse patterns in international climate governance.

By focusing on reports and plans rather than observing outcomes or processes, this thesis aims to understand how actors perceive climate adaptation and how those perceptions might impact future climate governance. The combination of OECD data analysis and structural topic modeling of NAPs, organized through the lens of the Dissonance Index, provides a comprehensive view of climate adaptation discourse across different types of actors and contexts.