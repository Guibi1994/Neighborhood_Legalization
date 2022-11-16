# Creación Matriz MIB y PreMIB


library(dplyr)
library(stringr)
library(ggplot2)


# 0. Matriz de MIB, controles y adiciones

a00_raw <-  readRDS("00_data/M02_Hexonos_puntos_MIB_controles_y_adicionales.RDS")

# 1. Matriz MIB ----

m01_MIB <- a00_raw %>% 
  select(ID_hex, starts_with(c("MIB"))) %>%
  # Agregar años en los que no hay MIB
  mutate(MIB_2005 = 0,MIB_2006 = 0,MIB_2007 = 0,MIB_2017 = 0,
         MIB_2019 = 0) %>% 
  reshape2::melt(id.vars = "ID_hex", value.name = "MIB_treated_period",
                 variable.name = "year") %>% 
  mutate(year = as.numeric(str_sub(year,-4,-1))) %>% 
  arrange(ID_hex,year,-MIB_treated_period ) %>% 
  mutate(pr = ifelse(MIB_treated_period == 1,year, NA)) %>% 
  group_by(ID_hex) %>% 
  mutate(MIB01_times_treted = sum(MIB_treated_period))


m01_MIB <- m01_MIB %>% 
  merge((m01_MIB %>% 
  group_by(ID_hex) %>% 
  summarise(MIB00_first_time_treated = min(pr, na.rm = T),
            MIB00_first_time_treated = ifelse(
              is.infinite(MIB00_first_time_treated),NA,MIB00_first_time_treated),
            MIB02_TI_dummy_treatment_group = ifelse(
              is.na(MIB00_first_time_treated),0,1)) %>% 
  as.data.frame()), by = "ID_hex") %>% 
  
  # Tratment varaibles
  mutate(
    MIB03_TV_dummy_treatment_group = ifelse(
    year >= MIB00_first_time_treated,1,0),
    MIB03_TV_dummy_treatment_group = ifelse(
      is.na(MIB03_TV_dummy_treatment_group),0,MIB03_TV_dummy_treatment_group)) %>%
  select(ID_hex, year,MIB00_first_time_treated, MIB01_times_treted, 
         MIB02_TI_dummy_treatment_group,MIB03_TV_dummy_treatment_group)



# 2. Matriz PreMIB ----

m02_PreMIB <- a00_raw %>% 
  select(ID_hex, starts_with(c("PreMIB"))) %>%
  mutate(PreMIB_2005 = 0,PreMIB_2006 = 0,PreMIB_2007 = 0,
         PreMIB_2009 = 0,PreMIB_2010 = 0,PreMIB_2011 = 0,
         PreMIB_2013 = 0,PreMIB_2014 = 0,PreMIB_2015 = 0,
         PreMIB_2017 = 0,PreMIB_2018 = 0,PreMIB_2019 = 0) %>% 
  reshape2::melt(id.vars = "ID_hex", value.name = "PreMIB_treated_period",
                 variable.name = "year") %>% 
  mutate(year = as.numeric(str_sub(year,-4,-1))) %>% 
  arrange(ID_hex,year,-PreMIB_treated_period ) %>% 
  mutate(pr = ifelse(PreMIB_treated_period == 1,year, NA)) %>% 
  group_by(ID_hex) %>% 
  mutate(PreMIB01_times_Pre_treted = sum(PreMIB_treated_period))


m02_PreMIB <- m02_PreMIB %>% 
  merge((m02_PreMIB %>% 
           group_by(ID_hex) %>% 
           summarise(PreMIB00_first_time_Pre_treated = min(pr, na.rm = T),
                     PreMIB00_first_time_Pre_treated = ifelse(
                       is.infinite(PreMIB00_first_time_Pre_treated),NA,PreMIB00_first_time_Pre_treated),
                     PreMIB02_TI_dummy_Pre_treatment_group = ifelse(
                       is.na(PreMIB00_first_time_Pre_treated),0,1)) %>% 
           as.data.frame()), by = "ID_hex") %>% 
  
  # Tratment varaibles
  mutate(
    PreMIB03_TV_dummy_Pre_treatment_group = ifelse(
      year >= PreMIB00_first_time_Pre_treated,1,0),
    PreMIB03_TV_dummy_Pre_treatment_group = ifelse(
      is.na(PreMIB03_TV_dummy_Pre_treatment_group),0,PreMIB03_TV_dummy_Pre_treatment_group)) %>%
  select(ID_hex, year,PreMIB00_first_time_Pre_treated, PreMIB01_times_Pre_treted, 
         PreMIB02_TI_dummy_Pre_treatment_group,PreMIB03_TV_dummy_Pre_treatment_group)
        


# 2. Matriz Agregada ----

m03_MIB_y_PreMIB <- merge(m01_MIB,m02_PreMIB, by = c("ID_hex","year"))










# 3. Arreglos finales y sintaxis


Matriz_FINAL <- m03_MIB_y_PreMIB %>% 
  merge((read.csv("00_geographic_data/00_Exportaciones_tabulares/Distacina_legalizacion_MIB.csv") %>% 
                         select(ID_hex, starts_with("Dis"))), by = "ID_hex")


Matriz_FINAL <- Matriz_FINAL %>% 
  mutate(T2_MIB00_was_treated = MIB02_TI_dummy_treatment_group,
         T2_MIB01_MIB_year = ifelse(is.na(MIB00_first_time_treated),0,MIB00_first_time_treated), 
         T2_MIB02_pre_MIB_year =PreMIB00_first_time_Pre_treated,
         T2_MIB03_general_group = ifelse(T2_MIB00_was_treated == 1,"tratados","no tratados"),
         T2_MIB04_neighborhood_order= 
           case_when(
             between(Dis_MIB,0,80)~"1st neighborhood",
             between(Dis_MIB,80,140)~"2nd neighborhood",
             between(Dis_MIB,140,210)~"3rd neighborhood",
             between(Dis_MIB,210,280)~"4th neighborhood",
             T~"outsider"),
         T2_MIB05_times_treated=MIB01_times_treted,
         T2_MIB06_times_PreTreated=PreMIB01_times_Pre_treted,
         T2_MIB07_was_pre_teated=PreMIB02_TI_dummy_Pre_treatment_group,
         T2_MIB08_was_treated_dynamic=MIB03_TV_dummy_treatment_group,
         T2_MIB09_was_PreTreated_dynamic=PreMIB03_TV_dummy_Pre_treatment_group,
         T2_MIB10_distance_towards_treated = Dis_MIB,
         
         T2_MIB04_neighborhood_order = ifelse(T2_MIB03_general_group != "no tratados",
                                     "treated",T2_MIB04_neighborhood_order)) %>% 
  select(ID_hex, year, starts_with("T2_"))
  
  


saveRDS(Matriz_FINAL,"00_data/Matrices_listas_por_componente/C01_MIB_y_PreMIB.RDS")


Matriz_FINAL 
ID_hex==188054

