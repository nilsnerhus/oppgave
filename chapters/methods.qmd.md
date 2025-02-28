# Methods {#sec-methods}

In this section, I explain the methods I will use to find data and to analyze it. First, I discuss the use of reported data as the empirical base of this study, 

Second, I will explain the OECD CRS dataset, how it is prepared and why, regressions and how I prepered the data for analysis.  

Third, I will explain the National Adaptation Plans at the UNFCCC, how it is prepared and why, structured topic models, and how I prepared the data for analysis. 

All data will be analysed with the help of the free and open source statstical programme `R` with the help of a number of packages in the `tidyverse`. All source files for this project are available in a [GitHub repository](https://github.com/nilsnerhus/oppgave) and the documents are automatically produced in .html, .pdf and .docx when changes are pushed to the repository and posted on [the Internet](https://nilsnerhus.github.io/oppgave/). 

## Reports (and plans) as the focus of the study 

By comparing the stated preferences of the donor-countries (as part of the UNFCCC) and comparing them to actual spending and plans, I can get close to a form of "revealed preference", where the actual preferences are shown as spending and plans, rather than the political statemens given at the Climate Summits, f.ex. 

I have chosen to focus on actors reporting, rather than observing outcomes or processes for this thesis. As the focus is on understanding how actors see climate adaptation and attempting to understand how that might impact the future.

## Modelling OECD data

The OECD Creditor Reporting System is a collection of self-reported activities by the members of the OECD. This is, to the member states and the officials that report, a very serious exersice, but it is also not coordinated beyond the guidance the OECD gives. Automatic (artificial intelligence) coding of the data will give different results. 

This is to this study fine, as I am after what is coded as climate adaptation to analyze what the actors think, and not the other way around. I will not compare across donors, so that inconsistency is handeled. As I have dealt with this dataset earlier during my internship with CONCITO, I was confident in how it functioned. 

I will retrieve the numbers by calling the OECDs API. This ensures I have updated and reproducible numbers for analysis. 

## Analyzing the National Adaptation plans

The plans are created by the climate adaptation project in one of the members of the World Bank group, in cooperation with different Government actors. 