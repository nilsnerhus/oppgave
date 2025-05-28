
This document is meant to explain my style preferences when writing text and code. It is by no means definate, so please ask questions when stuff is unclear. 

# Text

## Introductions

All sections start with an outlined argument.

> This is to guide the reader, and help them understand the main point of the section they are reading. It should be 2-3 sentences, and perhaps the best writing ever written. 

The text then continues with an introduction/readers guide to the content of the section. This is meant to help the reader understand where they are in the thesis, so they do not get lost. 

## Main text:

- I prefer highly structured text with a casual tone, making it accesible to a wide audience. 
- Using metaphors and known concepts should be prioritized
- All citations should be written as "This argument is a sentence [@authorYYYY]", and be present in the references.bib-file. 
- You should always make sure that you and I are on the same page, ask many questions and write small sections at the time. 

This also means some thinks I do not like

- Repeating text, or just word-fluff. This is a common part of AI-generated code
- Hallucinated references. This creates much more work when its done wrong. Ask questions about what texts should be references to what arguments. Tell me when you don't know. 

## Code

All code is written in R, and in two ways:

- Functions in the scripts-repo are written with each function having a single responsibility, with minimal overhead and complexity. The goal is to write as thin wrappers for the `stm`-package as possible. 
- These functions are then either called in the `nap_pipeline.R` or in code chunks in the Quarto documents.
- This data is then used to generate the inline values and visualizations in the text. 

## Inline values:

```r
# First, extract and name values in code chunk
low_inc_dom <- income_corpus$normalized_dominance[income_corpus$subcategory == "Low income"]
sids_dom <- geo_corpus$normalized_dominance[geo_corpus$subcategory == "SIDS"]

# Then use inline with helper functions
"Low-income countries show dominance of `r pct(low_inc_dom)`"
"This affects `r num(sample_size)` countries"
```

**Helper functions from utils.R**:

- `pct()`: Converts to percentage with 1 decimal (0.648 → "64.8%")
- `num()`: Formats numbers with space separator for 10,000+ (45000 → "45 000")

# Visualizations

## Plots:

- Each plot should have a name, a reference and a caption
- Use theme_minimal()
- Bars for categorical comparisons
- Grouped by category with spacing between groups
- Values displayed as percentages at end of bars
- Confidence intervals as lighter shaded extensions of bars
- Each plot should take up 

## Tables

- Prefer as few values as possible.
- Each table  should have a name, a reference and a caption