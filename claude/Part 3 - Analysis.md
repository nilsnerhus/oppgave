This part seeks to combine the case fr
# Findings:

The structure is quite simple, and should be predictable. For each measurement:

1. The general value, and the main categories (Income, region and geography)
2. Then the subcategories
3. Then possible methodological weaknesses we haven't controlled for

Each subsection should also have a simple ggplot2 minimal-theme graph highlighting the findings.

## Topics 

| topic_id | topic_name              | frex_terms                                                                       | top_countries                                 | topic_proportion |
| -------: | :---------------------- | :------------------------------------------------------------------------------- | :-------------------------------------------- | ---------------: |
|        1 | Territorial Instruments | territori, transit, famili, nativ, para, spanish, citi, instrument, biom, view   | Brazil (0.999), Argentina (0.917)             |        0.0971370 |
|        2 | Policy Arrangements     | pro, counti, chapter, mandat, mate, readi, entri, guidanc, mainstream, arrang    | Albania (0.954), Armenia (0.927)              |        0.2720010 |
|        3 | Rangeland Management    | rangeland, unoffici, moham, secretariat, box, money, gulf, director, dust, ahm   | Sri Lanka (0.998), Sudan (0.913)              |        0.0732592 |
|        4 | WASH Disability         | wash, hill, disabl, wildlif, nationwid, youth, percent, smart, coverag, tier     | Nepal (0.988), Bangladesh (0.824)             |        0.1115922 |
|        5 | Frost Risk              | strip, ensembl, appendix, centuri, frost, confid, calcul, paramet, day, yes      | Serbia (0.999), West Bank and Gaza (0.997)    |        0.0822447 |
|        6 | Mountain Security       | defens, task, mountain, norm, basin, river, advocaci, herder, background, welfar | Israel (0.999), Azerbaijan (0.798)            |        0.0948995 |
|        7 | Tropical Cyclones       | atol, typhoon, cyclon, pathway, pluvial, immedi, nanc, tropic, super, yet        | Philippines (0.981), Marshall Islands (0.845) |        0.1266059 |
|        8 | Hurricane Aid           | des, les, hurrican, outer, divis, refuge, cent, pour, aid, offic                 | St. Lucia (0.992), Kiribati (0.927)           |        0.1422605 |

## Dominance

| category  | subcategory               | documents | dominance | top_topics                                      | top_topic_ids |   p_value | significant |
| :-------- | :------------------------ | --------: | --------: | :---------------------------------------------- | :------------ | --------: | :---------- |
| Global    | Global                    |        46 | 0.5068198 | UNFCCC_NAPA_document, budget, agriculture       | 7,3,4         |        NA | FALSE       |
| Global    | Overall                   |        46 | 0.5068198 | Average of subcategories                        |               |        NA | NA          |
| Income    | Upper middle income       |        18 | 0.5150289 | UNFCCC_NAPA_document, budget, climate-modeling  | 7,3,6         | 0.0231411 | TRUE        |
| Income    | Lower middle income       |        15 | 0.6035719 | budget, agriculture, UNFCCC_NAPA_document       | 3,4,7         | 0.0000015 | TRUE        |
| Income    | Low income                |         9 | 0.6121351 | UNFCCC_NAPA_document, poverty, budget           | 7,1,3         | 0.0000000 | TRUE        |
| Income    | Overall                   |        46 | 0.5769120 | Average of subcategories                        |               |        NA | NA          |
| Region    | Latin America & Caribbean |         8 | 0.6736050 | UNFCCC_NAPA_document, municipal, budget         | 7,5,3         | 0.0000000 | TRUE        |
| Region    | Sub-Saharan Africa        |        13 | 0.6270248 | UNFCCC_NAPA_document, poverty, budget           | 7,1,3         | 0.0000000 | TRUE        |
| Region    | East Asia & Pacific       |        10 | 0.5686706 | budget, UNFCCC_NAPA_document, tropical cyclones | 3,7,2         | 0.0000000 | TRUE        |
| Region    | Overall                   |        46 | 0.6231001 | Average of subcategories                        |               |        NA | NA          |
| Geography | SIDS                      |        12 | 0.6981987 | UNFCCC_NAPA_document, budget, coastal           | 7,3,8         | 0.0000000 | TRUE        |
| Geography | LLDC                      |        11 | 0.6026048 | UNFCCC_NAPA_document, agriculture, poverty      | 7,4,1         | 0.0000019 | TRUE        |
| Geography | Overall                   |        46 | 0.6504017 | Average of subcategories                        |               |        NA | NA          |
| Time      | Middle                    |        16 | 0.6388820 | UNFCCC_NAPA_document, budget, coastal           | 7,3,8         | 0.0000532 | TRUE        |
| Time      | Late                      |        22 | 0.4603500 | budget, UNFCCC_NAPA_document, climate-modeling  | 3,7,6         | 0.0006223 | TRUE        |
| Time      | Early                     |         8 | 0.5199502 | UNFCCC_NAPA_document, agriculture, coastal      | 7,4,8         | 0.0138874 | TRUE        |
| Time      | Overall                   |        46 | 0.5397274 | Average of subcategories                        |               |        NA | NA          |

# Discussion

The introduction walks through (and references) the main findings:

1. **Topics**: The most popular topic is planning, and the next two are disasters 
2. **Dominance**: Grows as incomes fall. Very time-based also
3. **Variance**: Almost nothing of the variance, despite the overall high centralization within categories, is because of the categories a country is in. 

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

- Through the international lens (that we established in the unfccc-subchapter), the state is the only legible level, and together with securitization, all fingers are pointing to the state as the saviour and redeemer, when in fact, it is neither

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
