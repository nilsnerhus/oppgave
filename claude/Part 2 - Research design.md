This part builds my research design as a novel combination of future studies and post-colonial theory, with computuional text mining methods

# Theory 15,6/10 pages

## Critical future studies

Current version:

- Hard to follow.
- Too much direct reference of scholars, and too little on the concepts. 
- It references climate adaptation throughout.

Future versions:

- No reference to climate adaptation. This is the theory behind the method, not an expanded literature review. 
- New structure. Start with critical future studies, and what anticipatory governance means [@guston2014] in this context. and a walk-through of capitalist realism [@goode2017;@godhe2018], and then alternative futures. Capitalist realism should also include a discuission of western modernity [@escobar2018] and technoscience [@slaughter2024]
- Discuss the anticipatory ruination [@paprocki2018;@cons2018] as a form of governance as well. Defuturing could get this as well
- Go through the predictive, cultural and critical ways of understanding the future (with the goal of prepareing the argument for looking at discourse centralization)
- The fact that alternative futures are dismissed as either utopia or something else (consequense of the capitalist realism) and that this, together with anticipatory ruination, is defuturing [@fry2019]

The goal is to establish the future, and planning for the future, as something one can do differently that what is done today. 

## Epistemologies of the South

Current version

- Disorganized and random. 
- Barely any citations. 

Future version:

- Start with a discussion of epistemology and ontology! That is completely abscent now. Reference all the indiginous scholars I have read, to make the point that people have different worldviews. 
- Provincializing europe / anglobal area studies, to say that we barely investigate our own assumptions in northern social science. 
- Then go through relational and technocratic worldviews as inherently different, with their one world-world and a world where many worlds fit. Perhaps mention that we are not taking a traditionalist view of Southern epistemologies, but  recongnizing that it is something else that is created in the meeting of the worlds, and that that is a world that should exist as well. 
- Then, in the perhaps most difficult turn, I want to discuss design and tools [@escobar2018]. By discussing design and tools, I hope to get closer to a different way of imagining the future, and recognizing that the tools used (here interpreted broadly, *using* the state or a consultant is also a tool), are shown to influence the outcomes we get. The state is a very special tool that rarely represents all of society [@ferguson1994;@li2007;@escobar1995;@scott1998]. 
- Some of the large organizations use their own network for both producing and dissinating their knowledge, making them powerful, international epistemic communities [@defrancesco2020]

The goal here is to establish that domination though anticipatory governance is epistemicide.

## Domination 

Current version:

- Just full of AI-generated stuff I do not understand. 
- Not bringing together the context, literature and theory to make a compelling case for the method. Thinking in roots and options, why is this a natural continuation of the thesis, and not a deliberate choice?
- No part of my education is in computational methods, nor are they in lingustics. That is really showing in this section.

Future version

- First step: Explain how these plans are used. This is important, as they are anticipatory governance, and are in many cases intented to be a guiding document for both aid donors and the country that writes them. The largest donor of them all, the world bank, is the co-author, and the largest climate organization, the UNFCCC is the publisher. Doesn't get more international and governance-y than that! Understanding the shifts in these large organizations, and what this means for the communities they intervene in, is also important [@defrancesco2020]
- Second step: They are many, they are long, and they are *expensive* to read. Coding manually would take a long time, and not be responsive. Also, it would be less reproducible, as my manual coding is, yes, manual. This code (or framework) could be used on any corpus, doesn't even have to be national plans, with very little change. Some have used similar methods (more manual) to analyze speaches to the UN Security Council [@schoenfeld2018]
- Third step: These documents contain some kernels of insight into what these plans actually mean. They could be used to compare between contexts. They could also be used to investigate the critique of the *stadium tour* of consultant, seeing if the documents are just poorly filled out templates, like Peace Inc. writers claim [@autesserre2014;@craze2021].
- Fouth step: Set some expectations for what we find, and what that will mean. This is critical discourse analysis [@mullet2018], by attempting to break down assumptions and the power behind them. Quite literarly the dominance of some actors over our discourse, language and minds. Despite the number of new actors, diversification in urban climate action narratives has lead to homogenization of the discourse [@westman2023], that corporate funding changes the thematic content in US climate discourse [@farrell2016], the tweets by judges [@curry2019], speeches by popes [@genovese2015]

The goal of this section is to very clearly and transparently write out *why* I have spent the last four months learning to code. 

# Methods

The intro should somehow explain that I have developed this pipeline from the ground up. Considerable effort spent on learning the tools, and refining the process. Using known methods as much as possible, with improvized adaptations to deal with the specific stuff in the corpus.

One of the key changes have been as I have discovered more about the package, and reduced the size of the computations. The full complite used to take 3-4 hours, but it is done in less that 30 minutes now. 

## Corpus collection and Preparation

Current section:

- A lot of the same stuff going on
- No mention of the development process
- Unclear structure, and no mention of the actual work behind it. It sound very simple
- Highlight that it is not normal to have a dictinary validation approach, but it is needed because of the pdf-format creating artifacts.

Future section:

- Walk through each of the functions, not by name, but by what they are contributing to the setup, and why I have done as I have. 
- Here it will be: The goal is to make a comparable corpus that can say something about climate adaptation planning, with enough metadata to say something about how it relates to country categories. 
- For the text data: I automatically gather the pdfs from the website, so the data is always up to data, I then extract the texts that are tagged with english. Thus, remove document specific artifacts, remove non-english words, Then we do the normal stuff with cleaning. Lastly, I went trough and removed words that appeared only in one document (national specific) and in all documents (general beyond interest). This made the corpus considerably smaller, and better suited for analysis.
- I kept the country name and date posted from the UNFCCC website, and used the world bank package to add the wb-region and wb income data. I then scraped the UN websites for the countries listed as sids/lldc, and added that as a new category. I used a category map for the metadata to get the same categories calculated throughout the analysis, with a global, income, region, geography and year posted -category. These categories where chosen because they feature on the UNFCCC website, hinting at a link between the categories and the texts


## Structural topic modeling



## Text analysis

Current section:

- Also disorganized and hard to follow
- Random citations

Add the following as new paragraphs in the analysis section:

- The topic naming approach. To make sure the topics get human-readable names, I use the topic label approach, and utilize a limited large language model to name them based on the frequent and exlusive words that the stm-model outputs. This is to make rapid iteration more possible, as each change in the data processing and preperation, creates a need for renaming into a human-readable name. This is a comprimize to make the pipeline more usable. In the section, we are going through to validate the names, in the name of transparancy, not trying to analyze them as our main finding. 
- Both statistical methods are validated using bootstrapping [@roberts2019]. This method resamples the corpus with replacement (some documents may appear multiple times in a sample, others not at all) 1,000 times to estimate the uncertainty in our findings. This generates confidence intervals (set to 95%) that indicate the range of values we would expect if the study were repeated with different document samples. This does not explicitly control for group sizes, that would need other methods like rarefication, but they do shown clearly when group sizes influence the confidence in the point estimate. When those were tried the confidence intervals were too large to be able to be interpreted. 
- We calculated dominance by identifying the top 3 most prevalent topics for each group, then averaging those topics' proportions within that group using the STM topic proportions. We bootstrap this calculation across 1,000 resamples to generate confidence intervals, quantifying uncertainty in our dominance estimates. The choice of 3 topics is robust because the document-centric nature of the themes means dominance values remain stable whether using different concentration thresholds or different specific topics.
- We calculated variance explained using analysis of variance (ANOVA) logic: measuring how much of the total variation in dominance values can be attributed to differences between groups (income, region, etc.) versus differences within groups. This reveals whether structural factors systematically shape discourse patterns. The calculation partitions total variance into between-group and within-group components, with the ratio indicating explanatory power. Confidence intervals are generated through bootstrap resampling.