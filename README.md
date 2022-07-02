# Neighborhood Legalization

This repo contains all data files realted to the paper "Neighborhood legalizationn's effect on informal settlems" (2023) by G. Camargo and P. Acosta.

# 1. Abstract

# 2. Methods

## 2.1. Main methodological task

### 2.1.1. Selecting the "geographical unit of analysis"

The main challenge when analyzing the impact of neighborhood legalization in Bogotá is that the program that legalized "neighborhoods" does not fit into cadastral neighborhoods' geographical limits. Thus, it is hard to define comparable units other than the rest of legalized neighborhoods (therefore, there wouldn't be "never treated units"). Conversely, if one decides to select the cadastral neighborhood as the unit of analysis, the treatment status would be incremental, including those that by cartographic discrepancies intersect with treated areas.

### 2.1.2. Historical concerns

Another critical aspect is that all the geographic and time-variant data about Bogota's development started in 2005, but most of the legalization process occurred before this date. At the same time, it's vital to consider that legalization as a public policy often happens with the "*Comprehensive Neighborhood Improvement*" policy. Then, a good empirical strategy would analyze the treated units' past and future in Bogota's urban policy history and logic.

### 2.1.3. Undersanding inter-insitutinal processes and anti-processes

The management of the informal city (or the attempt to manage it) is a multi-institutional and inter-institutional endeavor. In a practical sense, this means that there are multiple sources of information and more than one way to interpret each piece. Sometimes the interpretation of "something unexpected" (such as a formal settlement inside an environmentally protected area) could be seen as a part of a complex legal and institutional history or as a lack of informational synchronization between public entities: it could be rather hard to determine.

Thus, a good empirical strategy must deeply understand the institutional and legal processes behind the data: what the data represents and what does not. It is easy (for example) to make asumptions of what a piece of cadastral information means, but it's essential to stay away from these assumptions.

[!alt text](https://github.com/Guibi1994/Neighborhood_Legalization/blob/main/04_figures/00_revision_general_1.png)  

.
## 2.2. The model strategy

$$
Y_{i,t}= \alpha_i +\alpha_t + \gamma^{-K}_k*D^{<-K}_{i,t} + \sum_{k=-K}^{-2}{\color{orange}{\gamma^{lead}_kD^k_{i,t}}}+ \sum_{k = 0}^{L}{\color{purple}{\gamma^{lags}_kD^k_{i,t}}} + \gamma^{L+}_k*D^{>L}_{i,t} + \epsilon_{i,t}
$$
