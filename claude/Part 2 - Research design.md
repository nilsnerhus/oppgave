This part builds my research design as a novel combination of future studies and post-colonial theory, with computuional text mining methods

## Theory 15,6/10 pages

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

## Methods




Add the following as new paragraphs in the analysis section:

- The topic naming approach. To make sure the topics get human-readable names, they have been qualitativly named using frex, lift and prob words, as well as what the STM-model deems as relevant text chunks, and the top 2 countries for that topic. 
- The rarefaction approach to make the values comparable across sample size. Has a high minimum group size (8). Calculates all groups with a random sample of 8 from the group 1000 times, and averages it at the end. Also produces a confidence interval that will be visible 
- The variance partitioning approach. For one topic: Calculates the mean and overall variation across all categories. Then calculates the group mean, and the distance to the overall mean weighted per country. Then divide the between group variance by the total variance. It is then averaged per group, across all topics. Also produces a confidence interval that will be visible

