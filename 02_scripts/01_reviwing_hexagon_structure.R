# DEscriptivie statistic monitorin years
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
# 0. Cargar base
a0_hexagons <- read.csv("00_geographic_data/00_Bases_de_datos/t01_hexagonos_seleccion_final_monitoreos.csv", sep = ";",
                        dec = ",")

a1_treated <- a0_hexagons %>% 
  select(ID_hex, legalization_year = Legalizacion, 
         legalization_act = NUMERO_ACT_1, starts_with("Moni")) %>%
  mutate(was_monitored = ifelse(Monitoreo_total >0,1,0),
         legalization_year = as.numeric(legalization_year),
         treated = ifelse(!is.na(legalization_year),T,F))


# 1. Preguntas generales

## 1.1. Cuantos hexágonos fueron tratados
sum(a1_treated$treated)

## 1.2. Cuantos hexágnos fueron monitoreados
sum(a2_treated$was_monitored)

## 1.3. Cuantos hexágos tratados fueron monitoreados
table(a1_treated$treated, a1_treated$was_monitored) # M. Confusión neta
table(a1_treated$treated, a1_treated$was_monitored)/nrow(a1_treated) # M. Confusión porcentual
table(a1_treated$treated, a2_treated$was_monitored)[4] # Tratados monitoreados
table(a1_treated$treated, a1_treated$was_monitored)[3] # NO Tratados monitoreados


## 1.3.

a1_treated %>% group_by(legalization_year) %>% 
  summarise(n= n()) %>% View()



# Panel Inicial con monitoreos y tratamientos
a2_treated <- a1_treated %>% 
  filter(was_monitored == T) %>% 
  melt(id.vars = c("ID_hex","legalization_year","legalization_act",
                   "was_monitored","Monitoreo_total","treated"), 
       variable.name = "year", value.name = "monitoring") %>% 
  arrange(ID_hex) %>% 
  mutate(year = as.integer(str_sub(year,-4,-1)),
         # Detección del primer año monitoreado
         monitoring_years = ifelse(monitoring ==0,NA,year)) %>% 
  group_by(ID_hex) %>% 
  mutate(first_monitoring = min(monitoring_years, na.rm = T),
         last_monitoring = max(monitoring_years, na.rm = T)) %>% 
  ungroup() %>% as.data.frame() %>% 
  filter(last_monitoring == 2019)


# Resuemn de estructura de datos:
a3_resume <- a2_treated %>% 
  group_by(ID_hex) %>% 
  summarise(legalization_year = mean(legalization_year, na.rm = T),
            Monitoreo_total = mean(Monitoreo_total, na.rm = T),
            treated = mean(treated, na.rm = T),
            first_monitoring = mean(first_monitoring, na.rm = T),
            last_monitoring = mean(last_monitoring, na.rm = T)) %>% 
  as.data.frame() %>% 
  mutate(legalization_group = 
           ifelse(is.nan(legalization_year),"Control",
                  ifelse(legalization_year <2005,"Tratado antes de 2005",
                         legalization_year)))
            


pr <- table(a3_resume$legalization_group, a3_resume$first_monitoring) %>% 
  as.data.frame() %>% dcast(Var1~Var2, value.var = "Freq")

write.csv(pr,"00_data/002_resumen_hexagonos_legalizados_monitoreados.csv",
          row.names = F)

a2_treated %>% group_by(year) %>% summarise(n = sum(monitoring)) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(year, n, group = "")) +
  geom_point()+
  geom_path()


