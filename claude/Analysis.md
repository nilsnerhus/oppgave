---
aliases: 
tags: notat
excalidraw-open-md: "true"
excalidraw-plugin: parsed
cssclass: 
---

# Part 3: Analysis

Part 3 of the thesis combines the case and research design to produce an analysis to better inform them both. 

The part contains two chapters: 

- Findings
- Discussion

## Findings: Dominance 

The goal of the section is to, in a structured way, present the findings from running the analysis. 

Right now the findings might be broken down into four categories

- Overall
- Region (with subregions)
- Income (with income groups)
- Geography (with is_sids and is_lldc)

A possible fifth category will come if there is time to calculate it, but it is put on the shelf for now. 

To help the commitee understand the findings, the bullseye()-visualization should be used comparativly (to compare to the overall, or between categories). 

As well as plots giving the 

## Raw output

### Dominance at n=3

> knitr::kable(dominance_tbl)

|category                     |level        | docs| raw_dominance| norm_dominance| ci_lower| ci_upper|
|:----------------------------|:------------|----:|-------------:|--------------:|--------:|--------:|
|Overall                      |Category     |   44|         0.949|          0.949|    0.928|    0.970|
|Income                       |Category     |   44|         0.949|          0.949|    0.928|    0.970|
|- Upper middle income        |Sub-category |   17|         0.947|          0.947|    0.916|    0.978|
|- Lower middle income        |Sub-category |   15|         0.928|          0.928|    0.880|    0.975|
|- Low income                 |Sub-category |    9|         0.980|          0.980|    0.968|    0.992|
|- High income                |Sub-category |    3|         0.974|          0.974|    0.934|    1.015|
|Region                       |Category     |   44|         0.949|          0.949|    0.928|    0.970|
|- Europe & Central Asia      |Sub-category |    6|         0.969|          0.969|    0.939|    0.999|
|- Sub-Saharan Africa         |Sub-category |   13|         0.967|          0.967|    0.943|    0.991|
|- East Asia & Pacific        |Sub-category |    9|         0.924|          0.924|    0.865|    0.982|
|- Latin America & Caribbean  |Sub-category |    7|         0.941|          0.941|    0.889|    0.993|
|- Middle East & North Africa |Sub-category |    4|         0.919|          0.919|    0.773|    1.065|
|- South Asia                 |Sub-category |    5|         0.959|          0.959|    0.922|    0.996|
|Geography                    |Category     |   44|         0.949|          0.949|    0.928|    0.970|
|- SIDS                       |Sub-category |   11|         0.932|          0.932|    0.884|    0.981|
|- LLDC                       |Sub-category |   10|         0.971|          0.971|    0.952|    0.990|


> knitr::kable(variance_tbl)

|category                     |level        | docs| std_dev| coef_var|
|:----------------------------|:------------|----:|-------:|--------:|
|Overall                      |Category     |   44|   0.071|    0.075|
|Income                       |Category     |   44|   0.071|    0.075|
|- Upper middle income        |Sub-category |   17|   0.066|    0.070|
|- Lower middle income        |Sub-category |   15|   0.094|    0.101|
|- Low income                 |Sub-category |    9|   0.018|    0.019|
|- High income                |Sub-category |    3|   0.036|    0.037|
|Region                       |Category     |   44|   0.071|    0.075|
|- Europe & Central Asia      |Sub-category |    6|   0.037|    0.038|
|- Sub-Saharan Africa         |Sub-category |   13|   0.044|    0.046|
|- East Asia & Pacific        |Sub-category |    9|   0.089|    0.097|
|- Latin America & Caribbean  |Sub-category |    7|   0.071|    0.075|
|- Middle East & North Africa |Sub-category |    4|   0.149|    0.162|
|- South Asia                 |Sub-category |    5|   0.042|    0.044|
|Geography                    |Category     |   44|   0.071|    0.075|
|- SIDS                       |Sub-category |   11|   0.082|    0.088|
|- LLDC                       |Sub-category |   10|   0.030|    0.031|


> knitr::kable(explained_tbl)

|   |category                     |level        | docs| explained_var|
|:--|:----------------------------|:------------|----:|-------------:|
|1  |Overall                      |Category     |   44|           0.0|
|2  |Income                       |Category     |   44|           8.1|
|7  |Region                       |Category     |   44|           8.0|
|14 |Geography                    |Category     |   44|           3.7|
|3  |- Upper middle income        |Sub-category |   17|           0.0|
|4  |- Lower middle income        |Sub-category |   15|           3.1|
|5  |- Low income                 |Sub-category |    9|           3.9|
|6  |- High income                |Sub-category |    3|           0.9|
|8  |- Europe & Central Asia      |Sub-category |    6|           1.1|
|9  |- Sub-Saharan Africa         |Sub-category |   13|           2.0|
|10 |- East Asia & Pacific        |Sub-category |    9|           2.6|
|11 |- Latin America & Caribbean  |Sub-category |    7|           0.2|
|12 |- Middle East & North Africa |Sub-category |    4|           1.7|
|13 |- South Asia                 |Sub-category |    5|           0.2|
|15 |- SIDS                       |Sub-category |   11|           1.4|
|16 |- LLDC                       |Sub-category |   10|           2.2|


### Dominance at n=5

> knitr::kable(dominance_tbl)

|category                     |level        | docs| raw_dominance| norm_dominance| ci_lower| ci_upper|
|:----------------------------|:------------|----:|-------------:|--------------:|--------:|--------:|
|Overall                      |Category     |   44|         0.981|          0.981|    0.972|    0.989|
|Income                       |Category     |   44|         0.981|          0.981|    0.972|    0.989|
|- Upper middle income        |Sub-category |   17|         0.978|          0.978|    0.963|    0.993|
|- Lower middle income        |Sub-category |   15|         0.975|          0.975|    0.957|    0.992|
|- Low income                 |Sub-category |    9|         0.994|          0.994|    0.989|    0.998|
|- High income                |Sub-category |    3|         0.992|          0.992|    0.980|    1.003|
|Region                       |Category     |   44|         0.981|          0.981|    0.972|    0.989|
|- Europe & Central Asia      |Sub-category |    6|         0.989|          0.989|    0.978|    1.000|
|- Sub-Saharan Africa         |Sub-category |   13|         0.990|          0.990|    0.984|    0.997|
|- East Asia & Pacific        |Sub-category |    9|         0.970|          0.970|    0.947|    0.992|
|- Latin America & Caribbean  |Sub-category |    7|         0.974|          0.974|    0.944|    1.003|
|- Middle East & North Africa |Sub-category |    4|         0.969|          0.969|    0.913|    1.026|
|- South Asia                 |Sub-category |    5|         0.986|          0.986|    0.974|    0.999|
|Geography                    |Category     |   44|         0.981|          0.981|    0.972|    0.989|
|- SIDS                       |Sub-category |   11|         0.972|          0.972|    0.949|    0.995|
|- LLDC                       |Sub-category |   10|         0.990|          0.990|    0.982|    0.997|


> knitr::kable(variance_tbl)

|category                     |level        | docs| std_dev| coef_var|
|:----------------------------|:------------|----:|-------:|--------:|
|Overall                      |Category     |   44|   0.029|    0.029|
|Income                       |Category     |   44|   0.029|    0.029|
|- Upper middle income        |Sub-category |   17|   0.031|    0.032|
|- Lower middle income        |Sub-category |   15|   0.035|    0.036|
|- Low income                 |Sub-category |    9|   0.007|    0.007|
|- High income                |Sub-category |    3|   0.010|    0.010|
|Region                       |Category     |   44|   0.029|    0.029|
|- Europe & Central Asia      |Sub-category |    6|   0.014|    0.014|
|- Sub-Saharan Africa         |Sub-category |   13|   0.012|    0.013|
|- East Asia & Pacific        |Sub-category |    9|   0.034|    0.035|
|- Latin America & Caribbean  |Sub-category |    7|   0.039|    0.041|
|- Middle East & North Africa |Sub-category |    4|   0.057|    0.059|
|- South Asia                 |Sub-category |    5|   0.014|    0.014|
|Geography                    |Category     |   44|   0.029|    0.029|
|- SIDS                       |Sub-category |   11|   0.039|    0.040|
|- LLDC                       |Sub-category |   10|   0.012|    0.012|


> knitr::kable(explained_tbl)

|   |category                     |level        | docs| explained_var|
|:--|:----------------------------|:------------|----:|-------------:|
|1  |Overall                      |Category     |   44|           0.0|
|2  |Income                       |Category     |   44|           7.1|
|7  |Region                       |Category     |   44|          10.5|
|14 |Geography                    |Category     |   44|           4.7|
|3  |- Upper middle income        |Sub-category |   17|           0.5|
|4  |- Lower middle income        |Sub-category |   15|           1.6|
|5  |- Low income                 |Sub-category |    9|           3.9|
|6  |- High income                |Sub-category |    3|           1.0|
|8  |- Europe & Central Asia      |Sub-category |    6|           1.0|
|9  |- Sub-Saharan Africa         |Sub-category |   13|           3.2|
|10 |- East Asia & Pacific        |Sub-category |    9|           3.2|
|11 |- Latin America & Caribbean  |Sub-category |    7|           1.0|
|12 |- Middle East & North Africa |Sub-category |    4|           1.4|
|13 |- South Asia                 |Sub-category |    5|           0.4|
|15 |- SIDS                       |Sub-category |   11|           2.5|
|16 |- LLDC                       |Sub-category |   10|           2.0|