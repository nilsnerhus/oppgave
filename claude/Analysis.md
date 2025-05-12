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

| dimension       | category                   | docs | raw_dominance | norm_dominance | ci_lower | ci_upper |
| :-------------- | :------------------------- | ---: | ------------: | -------------: | -------: | -------: |
| Overall         | All documents              |   44 |         0.265 |          0.295 |    0.257 |    0.329 |
| region          | Europe & Central Asia      |    6 |         0.859 |          0.863 |    0.831 |    0.988 |
| region          | Sub-Saharan Africa         |   13 |         0.684 |          0.704 |    0.598 |    0.784 |
| region          | East Asia & Pacific        |    9 |         0.626 |          0.651 |    0.555 |    0.767 |
| region          | Latin America & Caribbean  |    7 |         0.747 |          0.747 |    0.669 |    0.814 |
| region          | Middle East & North Africa |    4 |         0.556 |             NA |       NA |       NA |
| region          | South Asia                 |    5 |         0.594 |          0.594 |    0.594 |    0.594 |
| wb_income_level | Upper middle income        |   17 |         0.393 |          0.438 |    0.373 |    0.498 |
| wb_income_level | Lower middle income        |   15 |         0.373 |          0.391 |    0.331 |    0.460 |
| wb_income_level | Low income                 |    9 |         0.747 |          0.749 |    0.677 |    0.913 |
| wb_income_level | High income                |    3 |         0.659 |             NA |       NA |       NA |
| is_sids         | No is_sids                 |   33 |         0.326 |          0.344 |    0.298 |    0.387 |
| is_sids         | Yes is_sids                |   11 |         0.575 |          0.613 |    0.546 |    0.727 |
| is_lldc         | No is_lldc                 |   34 |         0.289 |          0.298 |    0.248 |    0.351 |
| is_lldc         | Yes is_lldc                |   10 |         0.477 |          0.511 |    0.418 |    0.596 |
| is_ldc          | No is_ldc                  |   29 |         0.317 |          0.323 |    0.282 |    0.395 |
| is_ldc          | Yes is_ldc                 |   15 |         0.350 |          0.393 |    0.327 |    0.436 |
| Overall         | All documents              |   44 |         0.406 |          0.431 |    0.387 |    0.478 |
| region          | Europe & Central Asia      |    6 |         0.941 |          0.937 |    0.930 |    0.996 |
| region          | Sub-Saharan Africa         |   13 |         0.877 |          0.901 |    0.844 |    0.971 |
| region          | East Asia & Pacific        |    9 |         0.846 |          0.872 |    0.814 |    0.938 |
| region          | Latin America & Caribbean  |    7 |         0.896 |          0.952 |    0.902 |    0.990 |
| region          | Middle East & North Africa |    4 |         0.769 |             NA |       NA |       NA |
| region          | South Asia                 |    5 |         0.831 |          0.831 |    0.831 |    0.831 |
| wb_income_level | Upper middle income        |   17 |         0.587 |          0.655 |    0.575 |    0.721 |
| wb_income_level | Lower middle income        |   15 |         0.519 |          0.564 |    0.505 |    0.638 |
| wb_income_level | Low income                 |    9 |         0.916 |          0.938 |    0.897 |    0.967 |
| wb_income_level | High income                |    3 |         0.873 |             NA |       NA |       NA |
| is_sids         | No is_sids                 |   33 |         0.482 |          0.513 |    0.452 |    0.566 |
| is_sids         | Yes is_sids                |   11 |         0.747 |          0.781 |    0.688 |    0.876 |
| is_lldc         | No is_lldc                 |   34 |         0.424 |          0.446 |    0.391 |    0.492 |
| is_lldc         | Yes is_lldc                |   10 |         0.701 |          0.765 |    0.697 |    0.837 |
| is_ldc          | No is_ldc                  |   29 |         0.466 |          0.493 |    0.437 |    0.553 |
| is_ldc          | Yes is_ldc                 |   15 |         0.533 |          0.585 |    0.520 |    0.652 |
| Overall         | All documents              |   44 |         0.581 |          0.609 |    0.574 |    0.637 |
| region          | Europe & Central Asia      |    6 |         0.995 |          0.995 |    0.994 |    0.998 |
| region          | Sub-Saharan Africa         |   13 |         0.965 |          0.978 |    0.956 |    0.997 |
| region          | East Asia & Pacific        |    9 |         0.945 |          0.961 |    0.930 |    0.988 |
| region          | Latin America & Caribbean  |    7 |         0.989 |          0.993 |    0.986 |    0.998 |
| region          | Middle East & North Africa |    4 |         0.957 |             NA |       NA |       NA |
| region          | South Asia                 |    5 |         0.957 |          0.957 |    0.957 |    0.957 |
| wb_income_level | Upper middle income        |   17 |         0.825 |          0.852 |    0.789 |    0.916 |
| wb_income_level | Lower middle income        |   15 |         0.707 |          0.778 |    0.707 |    0.840 |
| wb_income_level | Low income                 |    9 |         0.986 |          0.989 |    0.983 |    0.996 |
| wb_income_level | High income                |    3 |         0.987 |             NA |       NA |       NA |
| is_sids         | No is_sids                 |   33 |         0.686 |          0.717 |    0.669 |    0.783 |
| is_sids         | Yes is_sids                |   11 |         0.891 |          0.920 |    0.875 |    0.965 |
| is_lldc         | No is_lldc                 |   34 |         0.592 |          0.630 |    0.584 |    0.681 |
| is_lldc         | Yes is_lldc                |   10 |         0.921 |          0.927 |    0.902 |    0.981 |
| is_ldc          | No is_ldc                  |   29 |         0.651 |          0.698 |    0.650 |    0.751 |
| is_ldc          | Yes is_ldc                 |   15 |         0.751 |          0.807 |    0.756 |    0.871 |


| dimension       |   std_dev |  coef_var |
| :-------------- | --------: | --------: |
| economic        | 0.1673101 | 0.3648180 |
| geography       | 0.1461587 | 0.3309083 |
| administrative  | 0.1021375 | 0.1434655 |
| wb_income_level | 0.1945216 | 0.3699680 |
| region          | 0.1021375 | 0.1434655 |
| is_sids         | 0.1897222 | 0.3965283 |
| is_lldc         | 0.1506148 | 0.3719612 |
| is_ldc          | 0.0495775 | 0.1385379 |
| economic        | 0.1725277 | 0.2665751 |
| geography       | 0.1717874 | 0.2743052 |
| administrative  | 0.0490637 | 0.0545882 |
| wb_income_level | 0.1950730 | 0.2712894 |
| is_lldc         | 0.2251935 | 0.3719656 |
| is_sids         | 0.1899524 | 0.2935383 |
| region          | 0.0490637 | 0.0545882 |
| is_ldc          | 0.0650389 | 0.1205730 |
| geography       | 0.1484632 | 0.1859264 |
| economic        | 0.1074313 | 0.1302471 |
| administrative  | 0.0176234 | 0.0180421 |
| is_lldc         | 0.2097476 | 0.2694530 |
| wb_income_level | 0.1068004 | 0.1223297 |
| is_sids         | 0.1432349 | 0.1749775 |
| is_ldc          | 0.0768735 | 0.1021594 |
| region          | 0.0176234 | 0.0180421 |
