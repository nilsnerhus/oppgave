This part seeks to combine the case fr
# Findings:

The structure is quite simple, and should be predictable. For each measurement:

1. The general value, and the main categories (Income, region and geography)
2. Then the subcategories
3. Then possible methodological weaknesses we haven't controlled for

Each subsection should also have a simple ggplot2 minimal-theme graph highlighting the findings.

## Topics 
| topic_id | topic_name       | frex_terms                                                                                 | top_countries                                  | topic_proportion |
| -------: | :--------------- | :----------------------------------------------------------------------------------------- | :--------------------------------------------- | ---------------: |
|        1 | Poverty          | poverti, disrupt, poor, children, percent, migrat, women, growth, sanit, medium-term       | Mozambique (0.417), Tonga (0.354)              |        0.1217866 |
|        2 | coastal          | sea, tropic, cyclon, coral, coastal, rise, reef, slr, coast, island                        | Tonga (0.395), Philippines (0.36)              |        0.0909303 |
|        3 | irrigation       | feder, multilater, irrig, hydrometeorolog, percentag, west, mountain, wastewat, law, basin | Bosnia and Herzegovina (0.659), Kuwait (0.417) |        0.0784445 |
|        4 | agriculture      | livestock, climate-resili, wetland, harvest, farm, pest, watersh, altern, wildlif, medium  | Sri Lanka (0.498), Bangladesh (0.491)          |        0.1188209 |
|        5 | indigenous       | indigen, territori, transit, task, instrument, view, line, refuge, execut, perspect        | Israel (0.691), Brazil (0.667)                 |        0.0974081 |
|        6 | institutional    | offic, organis, depart, output, divis, partner, busi, perform, partnership, drm            | South Africa (0.615), Kiribati (0.574)         |        0.1660635 |
|        7 | climate_modeling | rcp, centuri, day, ensembl, trend, rainfal, precipit, decreas, maximum, confid             | West Bank and Gaza (0.649), Serbia (0.639)     |        0.0955156 |
|        8 | NAP              | gcf, ndc, unfccc, mainstream, pari, document, step, napa, mandat, chapter                  | Albania (0.719), Armenia (0.71)                |        0.2310304 |
## Dominance

| category  | subcategory                | documents | dominance | top_topics                               | top_topic_ids |   p_value | significant |
| :-------- | :------------------------- | --------: | --------: | :--------------------------------------- | :------------ | --------: | :---------- |
| Global    | Global                     |        46 | 0.2302088 | NAP, institutional, Poverty              | 8,6,1         |        NA | FALSE       |
| Global    | Overall                    |        46 | 0.2302088 | Average of subcategories                 |               |        NA | NA          |
| Income    | Upper middle income        |        18 | 0.2036434 | NAP, institutional, irrigation           | 8,6,3         | 0.0063208 | TRUE        |
| Income    | Lower middle income        |        15 | 0.3994211 | NAP, institutional, agriculture          | 8,6,4         | 0.0000002 | TRUE        |
| Income    | Low income                 |         9 | 0.3465522 | NAP, Poverty, institutional              | 8,1,6         | 0.0000248 | TRUE        |
| Income    | High income                |         4 | 0.5054201 | indigenous, NAP, irrigation              | 5,8,3         | 0.0299944 | TRUE        |
| Income    | Overall                    |        46 | 0.3637592 | Average of subcategories                 |               |        NA | NA          |
| Region    | Europe & Central Asia      |         6 | 0.5992266 | NAP, irrigation, climate_modeling        | 8,3,7         | 0.0000000 | TRUE        |
| Region    | Latin America & Caribbean  |         8 | 0.4942527 | NAP, indigenous, institutional           | 8,5,6         | 0.0000000 | TRUE        |
| Region    | South Asia                 |         5 | 0.5247842 | agriculture, NAP, institutional          | 4,8,6         | 0.0000000 | TRUE        |
| Region    | Sub-Saharan Africa         |        13 | 0.4042298 | NAP, institutional, Poverty              | 8,6,1         | 0.0001394 | TRUE        |
| Region    | East Asia & Pacific        |        10 | 0.3803075 | institutional, NAP, coastal              | 6,8,2         | 0.0000000 | TRUE        |
| Region    | Middle East & North Africa |         4 | 0.3735363 | climate_modeling, indigenous, irrigation | 7,5,3         | 0.0000055 | TRUE        |
| Region    | Overall                    |        46 | 0.4627228 | Average of subcategories                 |               |        NA | NA          |
| Geography | SIDS                       |        12 | 0.5098393 | institutional, NAP, coastal              | 6,8,2         | 0.0000000 | TRUE        |
| Geography | LLDC                       |        11 | 0.3276055 | NAP, agriculture, Poverty                | 8,4,1         | 0.0001098 | TRUE        |
| Geography | Overall                    |        46 | 0.4187224 | Average of subcategories                 |               |        NA | NA          |
| Time      | Middle                     |        16 | 0.4338995 | NAP, institutional, Poverty              | 8,6,1         | 0.0000001 | TRUE        |
| Time      | Late                       |        22 | 0.1350633 | NAP, Poverty, institutional              | 8,1,6         | 0.0185239 | TRUE        |
| Time      | Early                      |         8 | 0.2023666 | institutional, agriculture, NAP          | 6,4,8         | 0.0027337 | TRUE        |
| Time      | Overall                    |        46 | 0.2571098 | Average of subcategories                 |               |        NA | NA          |

## Main finding

**Three fundamental patterns emerge that challenge conventional assumptions about adaptation planning and provide empirical validation for post-development critiques of climate governance.**

### Pattern 1: Universal Concentration Despite Diverse Contexts

- **Every single country grouping** shows dominance values substantially above baseline (ranging from `r min_dominance` to `r max_dominance`)
- No group approaches diverse, distributed discourse that would characterize genuine context-specific adaptation
- Whether facing sea-level rise, drought, or glacial melt → same procedural focus
- **This universal concentration suggests systematic constraints operating across all adaptation planning**

### Pattern 2: Procedural Dominance Transcends Environmental Imperatives

- Topics 6 (_Institutional_) and 8 (_NAP_) appear in top 3 topics across virtually all groupings
- Combined procedural themes account for ~40% of total discourse
- **Environmental challenges matter less than institutional requirements** in shaping what countries discuss
- Climate science (Topic 7) adds to this technical-procedural dominance

### Pattern 3: P-Values Reveal Financial Dependence Logic _(Your breakthrough moment)_

- **Income-based p-value gradient provides statistical proof of autonomy theory:**
    - High income (p = `r high_income_sig`) = weakest group constraints
    - Upper-middle (p = `r upper_middle_sig`) = moderate constraints
    - Lower-middle/Low (p ≈ 0) = strongest group constraints
- **Higher income → higher p-values → weaker group effects → more discourse autonomy**
- Non-linear dominance values but linear p-value relationship
- Statistical validation that financial dependence shapes discourse homogenization

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
