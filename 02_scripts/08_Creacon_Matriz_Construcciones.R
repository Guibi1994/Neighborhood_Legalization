#

library(dplyr)

usos <- read.csv("00_data/003_Usos_Suelo_Bogota.csv")






lotes <- read.csv("00_geographic_data/00_Exportaciones_tabulares/A2_Usos_areas_2013.csv",
                  colClasses = c("UsoCLote" = "character")) %>% 
  select(ID_lote = UsoCLote, ID_Uso = UsoTUso)

construcciones <- read.csv("00_geographic_data/00_Exportaciones_tabulares/cons2013.csv",
                           sep = ";", colClasses = c("LoteCodigo" = "character")) %>%  
  select(ID_hex,ID_lote = LoteCodigo, ConNPisos, m2) %>% 
  mutate(m2 = as.numeric(str_replace_all(m2,",","."))) %>% 
  # Primer merge (lotes)
  merge((lotes), by = "ID_lote", all.x = T) %>% 
  
  # Segudo merge (usos)
  merge(usos, by = "ID_Uso", all.x = T) %>% 
  as.data.frame()


# area
pr <- construcciones %>% 
  sample_n(1000) %>% 
  group_by(ID_hex, Uso_Grupo) %>% 
  summarise(built_area = sum(m2, na.rm = T)) %>% 
  as.data.frame() %>% 
  mutate(Uso_Grupo = paste0("area_",Uso_Grupo)) %>% 
  reshape2::dcast(ID_hex~Uso_Grupo, value.var = c("built_area")) %>%
  mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)),
         area_TOTAL = rowSums(across(2:ncol(.)), na.rm = T))

# Construcciones
pr <- construcciones %>% 
  sample_n(10000) %>% 
  group_by(ID_hex, Uso_Grupo) %>% 
  summarise(constructions = n()) %>% 
  as.data.frame() %>% 
  mutate(Uso_Grupo = paste0("buildings_",Uso_Grupo)) %>% 
  reshape2::dcast(ID_hex~Uso_Grupo, value.var = c("constructions")) %>%
  mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)),
         buildings_TOTAL = rowSums(across(2:ncol(.)), na.rm = T))

# Pisos
pr <- construcciones %>% 
  sample_n(10000) %>% 
  group_by(ID_hex, Uso_Grupo) %>% 
  summarise(floors = mean(ConNPisos, na.rm = T)) %>% 
  as.data.frame() %>% 
  mutate(Uso_Grupo = paste0("floors_",Uso_Grupo)) %>% 
  reshape2::dcast(ID_hex~Uso_Grupo, value.var = c("floors"))# %>%
  mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)),
         buildings_TOTAL = rowSums(across(2:ncol(.)), na.rm = T))
