# Findings {#sec-findings}

In this findings-section, I will present the results from the preliminary data collection I have done. It is by no means scientifically sound, nor it it reproducble, but it is something. I will use this data as a way of testing the structure of the thesis, and highlight my own lack of understanding.

The stucture is as follows: 

- First, I present my findings from the OECD CRS dataset 
- Second, I present my findings from the National Adaptation Plans
- Third, I present a "representative" project, and its funding, justification and planning

## OECD findings

The "real" data collection for this part will be done in `R`, following best practice from @yakir2019, helped by more advanced literature when I get that far [@kuhn2022;@kabacoff2024]. 

The data in this analysis is based on calculations I did while I was an intern at CONCITO in Copenhagen. I forgot to send myself the file, so I am doing the findings from memory. They are based on a lot of `=sumif()` calculations done in the 2022 OECD CRS dataset, and highlighted five "key" donors (at least in our contract): Germany, France, Japan, the EU and the UK. It is also based on transcripts of interviews that I still have. 

I found that, within the field of transportation, there was considerable variation between the different donors, both in what they decided to fund, and how they coded their funding. Even though France and Germany are neighboors and the OECD officers meet regulary, their interpretation of sustainability is very different.

Some thing very visible nonetheless. *All* train projects were coded as climate mitigation, as well as *all* road projects were coded as climate adaptation. 



## NAP findings

The "real" data collection is a review of texts. Here, I will create a structured topic model with the help of the `tidytext` `R`-package. This mathematical algorithm will incorporate the metadata for the documents, economy (low-/middle income), geography (region) and 

The output of this will be a set of topics, and the likelyhood of them being present in within the different categories. 