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

| category  | subcategory               | documents | raw_dominance | normalized_dominance |  variance |  ci_lower |  ci_upper | top_topics                                                   | top_topic_ids |
| :-------- | :------------------------ | --------: | ------------: | -------------------: | --------: | --------: | --------: | :----------------------------------------------------------- | :------------ |
| Global    | Global                    |        45 |     0.5408674 |            0.5408674 | 0.0018755 | 0.4971752 | 0.6613511 | Policy Arrangements, Hurricane Aid, Tropical Cyclones        | 2,8,7         |
| Income    | Upper middle income       |        17 |     0.5808008 |            0.5808008 | 0.0036301 | 0.5487293 | 0.7802584 | Policy Arrangements, Mountain Security, Hurricane Aid        | 2,6,8         |
| Income    | Lower middle income       |        15 |     0.6746967 |            0.6746967 | 0.0046779 | 0.5867098 | 0.8502363 | Policy Arrangements, WASH Disability, Tropical Cyclones      | 2,4,7         |
| Income    | Low income                |         9 |     0.7419699 |            0.7419699 | 0.0028863 | 0.6864042 | 0.8870398 | Policy Arrangements, Hurricane Aid, Rangeland Management     | 2,8,3         |
| Income    | Overall                   |        45 |     0.6658225 |            0.6658225 | 0.0000000 | 0.6658225 | 0.6658225 | Average of subcategories                                     |               |
| Region    | Latin America & Caribbean |         8 |     0.8638138 |            0.8638138 | 0.0028900 | 0.7798435 | 0.9733186 | Territorial Instruments, Hurricane Aid, Policy Arrangements  | 1,8,2         |
| Region    | Sub-Saharan Africa        |        13 |     0.7181141 |            0.7181141 | 0.0030702 | 0.6584482 | 0.8784470 | Policy Arrangements, Hurricane Aid, Territorial Instruments  | 2,8,1         |
| Region    | East Asia & Pacific       |         9 |     0.7800914 |            0.7800914 | 0.0055730 | 0.6733727 | 0.9710995 | Tropical Cyclones, Policy Arrangements, Hurricane Aid        | 7,2,8         |
| Region    | Overall                   |        45 |     0.7873398 |            0.7873398 | 0.0000000 | 0.7873398 | 0.7873398 | Average of subcategories                                     |               |
| Geography | SIDS                      |        11 |     0.9232473 |            0.9232473 | 0.0013677 | 0.8408107 | 0.9767986 | Hurricane Aid, Policy Arrangements, Tropical Cyclones        | 8,2,7         |
| Geography | LLDC                      |        11 |     0.7661754 |            0.7661754 | 0.0035871 | 0.7243193 | 0.9502598 | WASH Disability, Policy Arrangements, Mountain Security      | 4,2,6         |
| Geography | Overall                   |        45 |     0.8447114 |            0.8447114 | 0.0000000 | 0.8447114 | 0.8447114 | Average of subcategories                                     |               |
| Time      | Middle                    |        15 |     0.7477841 |            0.7477841 | 0.0033470 | 0.6722211 | 0.8946319 | Policy Arrangements, Hurricane Aid, WASH Disability          | 2,8,4         |
| Time      | Late                      |        22 |     0.5314728 |            0.5314728 | 0.0026549 | 0.5114015 | 0.7082729 | Tropical Cyclones, Mountain Security, WASH Disability        | 7,6,4         |
| Time      | Early                     |         8 |     0.6600920 |            0.6600920 | 0.0081219 | 0.6253330 | 0.9644196 | Hurricane Aid, Rangeland Management, Territorial Instruments | 8,3,1         |
| Time      | Overall                   |        45 |     0.6464497 |            0.6464497 | 0.0000000 | 0.6464497 | 0.6464497 | Average of subcategories                                     |               |

## Variance:

| category  | subcategory               | documents | variance_explained |  ci_lower |  ci_upper |
| :-------- | :------------------------ | --------: | -----------------: | --------: | --------: |
| Income    | Upper middle income       |        17 |          0.0021985 | 0.0018660 | 0.0724664 |
| Income    | Lower middle income       |        15 |          0.0126890 | 0.0021588 | 0.1000620 |
| Income    | Low income                |         9 |          0.0306587 | 0.0077854 | 0.1204674 |
| Income    | Overall                   |        45 |          0.0151821 | 0.0151821 | 0.0151821 |
| Region    | Latin America & Caribbean |         8 |          0.0386113 | 0.0060296 | 0.1486862 |
| Region    | Sub-Saharan Africa        |        13 |          0.0512812 | 0.0145108 | 0.1515714 |
| Region    | East Asia & Pacific       |         9 |          0.0814157 | 0.0185943 | 0.2130853 |
| Region    | Overall                   |        45 |          0.0571027 | 0.0571027 | 0.0571027 |
| Geography | SIDS                      |        11 |          0.1109196 | 0.0540747 | 0.2439472 |
| Geography | LLDC                      |        11 |          0.0306106 | 0.0203485 | 0.1025350 |
| Geography | Overall                   |        45 |          0.0707651 | 0.0707651 | 0.0707651 |
| Time      | Middle                    |        15 |          0.1082696 | 0.0374081 | 0.2160699 |
| Time      | Late                      |        22 |          0.0701569 | 0.0222626 | 0.1631145 |
| Time      | Early                     |         8 |          0.0395444 | 0.0066538 | 0.1513828 |
| Time      | Overall                   |        45 |          0.0726570 | 0.0726570 | 0.0726570 |


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
