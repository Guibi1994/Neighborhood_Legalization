## Análisis inicial
library(stargazer)
library(dplyr)
library(ggplot2)
library(did)

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
         did_simple = T0_treated_group*T1_treated_period)


# Tratados por año
table(M1_incial$legalization_year[M1_incial$year == 2005])

M2_incial <- M1_incial %>% 
  filter(legalization_year == 2008 | is.na(legalization_year)) %>% 
  mutate(T1_treated_period = ifelse(year >= 2008,1,0),
         did_simple = T0_treated_group*T1_treated_period)




stargazer(
  lm(iligal_ocupations~T0_treated_group*T1_treated_period,M2_incial)
  ,type = "text")

stargazer(
  lm(iligal_ocupations~T0_isin_treatment_group+T1_treatment_simple+did_simple,
     M1_incial),
  lm(iligal_ocupations~T1_treatment_simple+did_simple,
     M1_incial),
  lm(iligal_ocupations~T0_isin_treatment_group+did_simple,
     M1_incial),
  type = "text")




table(M1_incial$T1_treatment_simple, M1_incial$T0_isin_treatment_group)
table(M1_incial$T1_treatment_simple, M1_incial$did_simple)







sum(M1_incial$did_simple == M1_incial$T1_treatment_simple)














# Some stuff 


M1_incial %>% group_by(general_group, year) %>% 
  summarise(iligal_ocupations = sum(iligal_ocupations)) %>% 
  as.data.frame() %>% 
  ggplot(aes(year, iligal_ocupations, color = general_group))+
  geom_point()+
  geom_path()

M1_incial %>% group_by(neighborhood_order, year) %>% 
  summarise(iligal_ocupations = sum(iligal_ocupations)) %>% 
  as.data.frame() %>% 
  ggplot(aes(year, iligal_ocupations, color = neighborhood_order))+
  geom_point()+
  geom_path()



M1_incial$ID_hex[M1_incial$T0_isin_treatment_group ==1 & M1_incial$legalization_year == 2015]

# Tratados Ejemplos 44555 47339 55257 73868 
# Contorles Ejemplos 41386 44146 43366 

pr <- M1_incial %>% filter(
  ID_hex %in% c(44555, 47339, 55257,91741, 73868, 41386, 44146, 43366)) %>%
  select(ID_hex, year, legalization_year, general_group,
         neighborhood_order, T2_treatment_dynamic, T0_isin_treatment_group, 
         T1_treatment_simple, did_simple,iligal_ocupations)
