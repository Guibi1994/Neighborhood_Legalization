# Neighborhood Legalization

This repo contains all data files realted to the paper "Neighborhood legalizationn's effect on informal settlems" (2023) by G. Camargo and P. Acosta.

# 1. Abstract

# 2. Methods

## 2.1. Main methodological task

### 2.1.1. Selecting the "geographical unit of analysis"

The main challenge when analysing the impact of neighborhood legalization in Bogotá, is that the porgram is that legalized "neighborhoods" do not feet into the geografical limits of cadastral neighborhoods. Thus, is hard to define comparable units other than the rest of legalized neighborhoods (therefore there wouldn't be "never treated units"). In the other hand, if ones decide to select the cadastral neighborhood as the unit of analysis, therefore the treatment status would be incremental, including those that by cartographic discrepancies intersect with treated areas.

### 2.1.2. Historiacal 

## 2.2. The model strategy

$$
Y_{i,t}= \alpha_i +\alpha_t + \gamma^{-K}_k*D^{<-K}_{i,t} + \sum_{k=-K}^{-2}{\color{orange}{\gamma^{lead}_kD^k_{i,t}}}+ \sum_{k = 0}^{L}{\color{purple}{\gamma^{lags}_kD^k_{i,t}}} + \gamma^{L+}_k*D^{>L}_{i,t} + \epsilon_{i,t}
$$
