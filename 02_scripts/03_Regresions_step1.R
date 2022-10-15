## Análisis inicial
library(stargazer)
library(dplyr)
library(ggplot2)
library(did)
`%!in%` = Negate(`%in%`)

# Cargar matriz inicial
M0_raw <- readRDS("00_data/M01_hexagonal_matrix_simple.RDS")



# Filtro inicial
M1_incial <- M0_raw %>% 
  filter(continus_monitoring == T, total_monitoring >= 15, 
         general_group != "tratados antes de 2005", 
         year != 2019) %>%
  # Variables de tratamiento
  mutate(T0_treated_group = ifelse(is.na(legalization_year),0,1),
         
         T1_treated_period = ifelse(year>= legalization_year ,1,0),
         T1_treated_period = ifelse(is.na(T1_treated_period),0,
                                    T1_treated_period),
          
         T2_treatment_dynamic = year-legalization_year,
         T2_treatment_dynamic = ifelse(is.na(T2_treatment_dynamic),0,
                                       T2_treatment_dynamic),
         did_simple = T0_treated_group*T1_treated_period,
         legalization_year = ifelse(is.na(legalization_year),0,legalization_year))

table(M1_incial$legalization_year[M1_incial$year == 2005]) # Tratados por año


# Regresiones básicas




M2_incial <- M1_incial %>% 
  filter(legalization_year == 2007 | is.na(legalization_year)) %>% 
  mutate(T1_treated_period = ifelse(year >= 2009,1,0),
         did_simple = T0_treated_group*T1_treated_period)

m2007 <- 

lm(iligal_ocupations~T0_treated_group*T1_treated_period,M2_incial)




# Callaway Sant' Anna

M2_prueba <- M1_incial 
table(M2_prueba$legalization_year[M2_prueba$year == 2005])

atts <- att_gt(yname = "iligal_ocupations", # LHS variable
               tname = "year", # time variable
               idname = "ID_hex", # id variable
               gname = "legalization_year", # first treatment period variable
               data = M2_prueba, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "ipw", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 10000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "ID_hex", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional

ggdid(atts) +
  theme(text = element_text(family = "serif"))


# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es) +
  theme_minimal()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")


summary(atts)



# Tratados Ejemplos 44555 47339 55257 73868 
# Contorles Ejemplos 41386 44146 43366 

pr <- M1_incial %>% filter(
  ID_hex %in% c(44555, 47339, 55257,91741, 73868, 41386, 44146, 43366)) %>%
  select(ID_hex, year, legalization_year, general_group,
         neighborhood_order, T2_treatment_dynamic, T0_isin_treatment_group, 
         T1_treatment_simple, did_simple,iligal_ocupations)
