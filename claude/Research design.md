---
aliases: 
tags: notat
excalidraw-open-md: "true"
excalidraw-plugin: parsed
cssclass: 
---

# Part 2: Discourse centralization

Part 2 of the thesis establishes the research design, including the theoretical framework and methodological approach. This section builds upon the climate adaptation case established in Part 1, introducing the analytical lens and tools that will be used to examine discourse centralization.

The part contains two chapters: 

- Theory
- Methods

## Theory: Development Ontology and Future-Making

Climate adaptation is fundamentally oriented toward the future, contrasting with mitigation which seeks to prevent future impacts rather than prepare for them. This future orientation makes adaptation a rich site for examining how different actors imagine and construct possible futures. Through the lens of anticipatory governance, we can understand adaptation planning as an inherently political process where certain future imaginaries are privileged while others are marginalized.

The construction of "climate vulnerability" in adaptation discourse establishes particular relationships between actors, especially between the Global North and South. This discourse functions as a power technique that opens some future possibilities while foreclosing others. Critical future studies provides a valuable framework for examining these future-making practices, revealing how epistemological assumptions shape which futures are deemed possible or impossible.

Different knowledge systems—whether scientific, indigenous, or religious—generate different understandings of how change occurs and what future possibilities exist. Underlying these knowledge systems are deeper ontological assumptions about agency, causality, and the nature of time and space themselves. Although these assumptions typically remain implicit in policy documents and funding decisions, making them explicit is essential for critical analysis of adaptation discourse.

This theoretical framework connects to broader critiques of technoscience, Western capitalist modernity, neoliberalism, and capitalist realism. Of particular importance is the concept of "defuturing"—the active reduction of possible futures through discourses that naturalize certain development pathways while rendering alternatives unthinkable. When adaptation is framed primarily as a technical problem rather than a political-economic condition, deeper questions about systemic causes of vulnerability are sidelined.

The degree of discourse centralization serves as an indicator of the dominance of what might be called the "anglobal discourse." Low fragmentation (high centralization) suggests the dominance of particular ways of knowing and being, limiting the range of futures considered legitimate. Conversely, higher fragmentation would indicate greater epistemological and ontological plurality, potentially enabling a wider range of future possibilities.

## Methods: The Dominance Index

To empirically analyze discourse centralization in climate adaptation, this thesis develops a methodological approach called the "Dominance Index." This approach applies quantitative methods to questions typically addressed through qualitative means, creating an interdisciplinary bridge between critical theory and computational text analysis.

The primary corpus consists of 45 English-language National Adaptation Plans (NAPs) submitted to the UNFCCC. Each document is tagged with metadata including country income level, geographic region, and special status (SIDS, LDC, etc.). The text preprocessing pipeline includes tokenization, stopword removal, and lemmetization to prepare the documents for analysis.

Structural topic modeling allows for examination of how topics correlate with document metadata such as region or income level. Parameter optimization ensures the topic model effectively captures meaningful patterns in the discourse. Topic dominance is measured by the share of the discourse that happens in the top five topics identified.

Visualization techniques help make these patterns accessible and interpretable. The Dominance Index represents a unique methodological contribution that differs from existing approaches to discourse analysis by providing an empirical measurement of discourse centralization that can be compared across different actor groups and contexts.

This methodological framework maintains a reflexive stance on knowledge production, acknowledging the power dynamics inherent in research on North-South relations. It aims to make implicit assumptions explicit while aligning critical theoretical perspectives with quantitative methods—demonstrating that these approaches can complement rather than contradict each other.

By addressing methodological considerations throughout rather than relegating them to a "limitations" section, this research design integrates critical awareness into the entire analytical process, distinguishing between the overall research framework and the specific techniques employed.

The methods are defined in the code, which will be available on GitHub.

### Temporal Impact Analysis

In addition to cross-sectional analysis of discourse centralization, this research examines how individual documents impact the overall centralization over time. By employing a sliding window approach, we can measure the marginal impact of each new National Adaptation Plan on discourse dominance, revealing whether certain documents introduce greater fragmentation or centralization.

This temporal analysis provides insights into the evolution of adaptation discourse, identifying potential turning points or trends in how knowledge diversity has changed over time. It complements the dimensional analysis by adding a dynamic perspective to what would otherwise be a static snapshot.

### Dimension Ranking

To systematically assess which factors best explain patterns of discourse centralization, this research employs a dimension ranking approach. For each dimension (region, income level, etc.), we calculate multiple metrics of variation including:

1. Range of dominance values
2. Standard deviation of dominance values  
3. Coefficient of variation

These metrics quantify the degree to which each dimension differentiates discourse patterns. The dimension with the greatest variation in dominance values can be considered the most explanatory factor in discourse centralization. This approach enables direct comparison of the relative importance of regional, economic, and other factors in shaping adaptation discourse.