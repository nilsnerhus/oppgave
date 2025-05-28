# Visualization and Writing Style Guide

## Plot Preferences

**Design principles**:

- Clean, minimal aesthetic (use theme_minimal())
- Easy readability is paramount
- Show uncertainty transparently (confidence intervals)
- Group similar items together visually

**Specific preferences**:

- Bars for categorical comparisons
- Grouped by category with spacing between groups
- Colors: Income (#0d6efd), Region (#6f42c1), Geography (#198754)
- Values displayed as percentages at end of bars
- Confidence intervals as lighter shaded extensions of bars
- No unnecessary chart junk

## Code and Value Usage

**Inline values approach**:

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

## Writing Style

**Academic with personal elements**:

- Personal experience as data, not autobiography
- Theory grounded in concrete examples
- Reflexivity about performing academic conventions

**Clarity over complexity**:

- Short sentences for key insights
- Technical concepts explained through accessible metaphors
- Star Wars metaphor used pedagogically but taken seriously

**Structure preferences**:

- Build from personal → theoretical → empirical
- Use transitions that connect ideas naturally
- End sections with clear implications

**Voice**:

- Confident but not dogmatic
- Critical but not cynical
- Honest about contradictions and uncertainties