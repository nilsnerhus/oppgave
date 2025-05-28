This part seeks to combine the case fr
# Findings:

The structure is quite simple, and should be predictable. For each measurement:

1. The general value, and the main categories (Income, region and geography)
2. Then the subcategories
3. Then possible methodological weaknesses we haven't controlled for

## Topics (must be named asap)


## Dominance

| level_type | category  | subcategory               | documents | raw_dominance | normalized_dominance |  variance |  ci_lower |  ci_upper | top_topics                                                                                        | top_topic_ids |
| :--------- | :-------- | :------------------------ | --------: | ------------: | -------------------: | --------: | --------: | --------: | :------------------------------------------------------------------------------------------------ | :------------ |
| document   | Overall   | Overall                   |        45 |     0.9699236 |            0.9700666 | 0.0003723 | 0.9264797 | 0.9969644 | Mate: entri, prohibit, pro, Counti: linkag, ongo, wildlif, Unoffici: translat, output, pillar     | 2,6,3         |
| corpus     | Overall   | Overall                   |        45 |     0.3228994 |            0.5294005 | 0.0073019 | 0.3843309 | 0.7120259 | Mate: entri, prohibit, pro, Counti: linkag, ongo, wildlif, Unoffici: translat, output, pillar     | 2,6,3         |
| document   | Income    | Upper middle income       |        17 |     0.9693483 |            0.9689806 | 0.0001421 | 0.9461802 | 0.9901076 | Frost: deficit, hail, yes, Mountain: bangkok, meter, norm, Mate: entri, prohibit, pro             | 7,4,2         |
| corpus     | Income    | Upper middle income       |        17 |     0.4259110 |            0.5409669 | 0.0052564 | 0.4085564 | 0.6918899 | Frost: deficit, hail, yes, Mountain: bangkok, meter, norm, Mate: entri, prohibit, pro             | 7,4,2         |
| document   | Income    | Lower middle income       |        15 |     0.9610106 |            0.9622521 | 0.0004562 | 0.9292190 | 0.9939956 | Counti: linkag, ongo, wildlif, Atol: typhoon, pluvial, immedi, Unoffici: translat, output, pillar | 6,9,3         |
| corpus     | Income    | Lower middle income       |        15 |     0.4195776 |            0.5439935 | 0.0047357 | 0.4215601 | 0.6804861 | Counti: linkag, ongo, wildlif, Atol: typhoon, pluvial, immedi, Unoffici: translat, output, pillar | 6,9,3         |
| document   | Income    | Low income                |         9 |     0.9761690 |            0.9762617 | 0.0000132 | 0.9732227 | 0.9850052 | Les: des, refuge, pour, Counti: linkag, ongo, wildlif, Yahoo: timber, royal, page                 | 8,6,12        |
| corpus     | Income    | Low income                |         9 |     0.6241638 |            0.6484031 | 0.0009228 | 0.5773113 | 0.6947812 | Les: des, refuge, pour, Counti: linkag, ongo, wildlif, Yahoo: timber, royal, page                 | 8,6,12        |
| document   | Region    | Latin America & Caribbean |         8 |     0.9659966 |            0.9659966 | 0.0026168 | 0.9659966 | 0.9659966 | Unoffici: translat, output, pillar, Outer: cent, director, money, Biom: ana, deploy, northeast    | 3,14,1        |
| corpus     | Region    | Latin America & Caribbean |         8 |     0.5953091 |            0.5953091 | 0.0083910 | 0.5953091 | 0.5953091 | Unoffici: translat, output, pillar, Outer: cent, director, money, Biom: ana, deploy, northeast    | 3,14,1        |
| document   | Region    | Sub-Saharan Africa        |        13 |     0.9694166 |            0.9691383 | 0.0000674 | 0.9547963 | 0.9859379 | Counti: linkag, ongo, wildlif, Les: des, refuge, pour, Unoffici: translat, output, pillar         | 6,8,3         |
| corpus     | Region    | Sub-Saharan Africa        |        13 |     0.5947996 |            0.6639720 | 0.0043616 | 0.5499697 | 0.7966865 | Counti: linkag, ongo, wildlif, Les: des, refuge, pour, Unoffici: translat, output, pillar         | 6,8,3         |
| document   | Region    | East Asia & Pacific       |         9 |     0.9852339 |            0.9852919 | 0.0000068 | 0.9836499 | 0.9912976 | Atol: typhoon, pluvial, immedi, Mountain: bangkok, meter, norm, Counti: linkag, ongo, wildlif     | 9,4,6         |
| corpus     | Region    | East Asia & Pacific       |         9 |     0.5595184 |            0.5848216 | 0.0017789 | 0.5058123 | 0.6294370 | Atol: typhoon, pluvial, immedi, Mountain: bangkok, meter, norm, Counti: linkag, ongo, wildlif     | 9,4,6         |
| document   | Geography | SIDS                      |        11 |     0.9627029 |            0.9629341 | 0.0000895 | 0.9494529 | 0.9815567 | Outer: cent, director, money, Unoffici: translat, output, pillar, Commenc: yet, ref, regret       | 14,3,10       |
| corpus     | Geography | SIDS                      |        11 |     0.5911894 |            0.6486521 | 0.0036464 | 0.5430868 | 0.7920666 | Outer: cent, director, money, Unoffici: translat, output, pillar, Commenc: yet, ref, regret       | 14,3,10       |
| document   | Geography | LLDC                      |        10 |     0.9747076 |            0.9748023 | 0.0000408 | 0.9686257 | 0.9890699 | Mate: entri, prohibit, pro, Yahoo: timber, royal, page, Mountain: bangkok, meter, norm            | 2,12,4        |
| corpus     | Geography | LLDC                      |        10 |     0.4810221 |            0.5378583 | 0.0016802 | 0.4452690 | 0.5996884 | Mate: entri, prohibit, pro, Yahoo: timber, royal, page, Mountain: bangkok, meter, norm            | 2,12,4        |

## Variance:

|   |level_type  |category       |subcategory                | variance_explained|
|:--|:-----------|:--------------|:--------------------------|------------------:|
|20 |overall     |ALL_CATEGORIES |AVERAGE                    |          0.0877733|
|16 |subcategory |Geography      |SIDS                       |          0.0310075|
|18 |subcategory |Geography      |LLDC                       |          0.0244102|
|15 |dimension   |Geography      |is_sids                    |          0.0310075|
|17 |dimension   |Geography      |is_lldc                    |          0.0244102|
|19 |category    |Geography      |Overall                    |          0.0277089|
|2  |subcategory |Income         |High income                |          0.0495815|
|3  |subcategory |Income         |Low income                 |          0.0391070|
|5  |subcategory |Income         |Upper middle income        |          0.0255569|
|4  |subcategory |Income         |Lower middle income        |          0.0249294|
|1  |dimension   |Income         |wb_income_level            |          0.1089815|
|6  |category    |Income         |Overall                    |          0.1089815|
|9  |subcategory |Region         |Europe & Central Asia      |          0.0499998|
|13 |subcategory |Region         |Sub-Saharan Africa         |          0.0408209|
|10 |subcategory |Region         |Latin America & Caribbean  |          0.0392876|
|11 |subcategory |Region         |Middle East & North Africa |          0.0335147|
|12 |subcategory |Region         |South Asia                 |          0.0321487|
|8  |subcategory |Region         |East Asia & Pacific        |          0.0286467|
|7  |dimension   |Region         |region                     |          0.1866942|
|14 |category    |Region         |Overall                    |          0.1866942|



# Discussion

The introduction walks through (and references) the main findings:

1. **Topics**: The most popular topics are connected to finance and planning 
2. **Dominance**: The lowest income countries are the most centralized
3. **Variance**: Geography is a non-factor and region is much more important

Then, it presents the other parts. How epistemolical and ontological assumptions shape what we think is reasonable (or possible, preferable). Straying too far from reasonable, in your group, makes you dimissed.

## Politics of Reasonable

Goes through my experience of being educated since 2006, making the following points:

- Knowledge is power, and the teacher-expert and the student-learner. Through 11 different institutes, I have felt how differently reality is portraid by different experts. 
- Everyone has a fear of dismissal by their peers. That comes when one ventures too far from the beaten path. This thesis is also in this tension. It must stay a thesis, but also communicate something different.
- Politics is this process, but at a sociatal level. Most change has been deemed unreasonable at one point. My own experience with oil exploration policy in Norway, where first the idea of oil exploration leading to climate change was deemed totally and completely unreasonable, something only read tree-huggers and idiots would think. Now, that is the position of almost all parties in politics. And also how women sufferage and civil rights for black people, also was totally unreasonable at one point. 

This section should be peppered with references to the theory section. This is what critical future studies really focuses on. This also challenges the idea that we somehow have finished political or other preferences. We are shaped by the world we live in, as much as we shape it. Politics of the future could also be completely unreasonable. 

The expertise on climate adaptation and development is centralized. In the physical sense, most happens in Washington DC, and in english, and by the same people travelling around. This stadium tour of consultants are described vivdly [@paprocki2018;@dewan2021;@craze2021;@mosse2011], as it suspends local reality, hires local hands, and takes all political and financial focus.

The empirical backing for this is the variance numbers showing how important region is. These regions are huge, and mostly an aidland-made category. Development banks, regional experts etc.

## Adaptation empire

Revisists the Star Wars quote from earlier. Attempting to explain that this system does not work by explisit coordination (no emperor), but rather by the politics of being reasonable. This leads to *control*. 

Walk through the adaptation nexus approach, with its discourse of focusing more on certain topics, being more culturally sensitive and otherwise attempting to save reasonableness, or hoping the attention will pass. The nexus naturalizes and explains away the *control*, in a way that often is convincing. The Human Security student Partagaz. Maybe break the fourth wall again, and say that Human Security students might find work in the Intergalatic Security Burau (ISB)

This then creates new problems:

1. Vulnerability becomes the new underdevelopment (dominance)
2. Adaptation becomes an anti-politics machine (variance)
3. National adaptation plans become an epistemicide

## Beyond control

Revisit the Star Wars again, and Nemiks vision of freedom. Then, we start talking about finding ways of lessening the control by the North over the South, by referencing decolonial theory. 

1. That decolonialism is about land-back [@tuck2012], about autonomy [@escobar2018]
2. Decolonialism is also about turning away from the state [@corntassel2021] (even small steps is a resurgence)
3. Less *control* will re-politicize communities, not solve them
4. Climate adaptation for communities: To most communities, climate adaptation is nothing more or less important than all other adaptations that have to be made. It is a political decition to define adaptation or not [@hulme2011;@hulme2023] 
5. Climate justice - the best way of reducing emissions, by replanning and restructuring the North, rather than unmaking and remaking the Third World (again) - reference all the econ-books I have been looking at: [@mazzucato2021;@vettese2022;@bastani2019;@raworth2017], mainly to make the point that it *is* possible to rethink this stuff. 
