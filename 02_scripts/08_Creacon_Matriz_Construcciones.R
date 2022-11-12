# Matriz de construcciones


# librerias
library(dplyr)
library(stringr)



# 0. Datos base ----
usos <- read.csv("00_data/003_Usos_Suelo_Bogota.csv")





# 1. Super-loop ----

FINAL <- data.frame()


for (i in 2012:2019) {
  print(paste0("Correindo base del ", i))
  
  ## 1.1. Bases iniciales ----
  message("leyendo base de lotes")
  ### 1.1.1. Lotes ----
  lotes <- readRDS(paste0("00_data/Matriz_construcciones/Usos_areas_",i,".RDS")) %>% 
    select(ID_lote = UsoCLote, ID_Uso = UsoTUso)
  
  ### 1.1.2. Construcciones ----
  message("leyendo bases de construcciones")
  construcciones <- readRDS(paste0("00_data/Matriz_construcciones/cons",i,".RDS")) %>%  
    select(ID_hex,ID_lote = LoteCodigo, ConNPisos, m2) %>% 
    # Primer merge (lotes)
    merge((lotes), by = "ID_lote", all.x = T) %>% 
    # Segudo merge (usos)
    merge(usos, by = "ID_Uso", all.x = T) %>% 
    as.data.frame()
  
  
  ## 1.2. Sub-bases ----
  
  ### 1.2.1. Base Lotes ----
  message("Panel de lotes ...")
  b1_lotes <- lotes %>%
    #sample_n(2000) %>% 
    # Primer merge (lotes)
    merge((construcciones %>% select(ID_lote,ID_hex)), by = "ID_lote", all.x = T) %>% 
    # Segudo merge (usos)
    merge(usos, by = "ID_Uso", all.x = T) %>% 
    as.data.frame() %>% 
    mutate(Uso_Grupo = ifelse(is.na(Uso_Grupo),"Others",Uso_Grupo)) %>% 
    group_by(ID_hex, Uso_Grupo) %>% 
    summarise(lots = n()) %>% 
    as.data.frame() %>% 
    mutate(Uso_Grupo = paste0("lots_",Uso_Grupo)) %>% 
    reshape2::dcast(ID_hex~Uso_Grupo, value.var = c("lots")) %>%
    mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)),
           lots_TOTAL = rowSums(across(2:ncol(.)), na.rm = T)) %>% 
    mutate(year = i) %>% 
    select(1,11,10,8,2,3,4,5,6,9,7) %>% 
    filter(ID_hex != "")
  
  names(b1_lotes)[3:11] <-paste0(
    "O03_lots",str_pad(1:9,width = 2,pad = 0),"_",names(b1_lotes)[3:11])
  
  
  ### 1.2.2. Base area construida ----
  message("Panel de area construida ...")
  b2_area_cons <- construcciones %>% 
    #sample_n(4000) %>% 
    mutate(Uso_Grupo = ifelse(is.na(Uso_Grupo),"Others",Uso_Grupo)) %>% 
    group_by(ID_hex, Uso_Grupo) %>% 
    summarise(built_area = sum(m2, na.rm = T)) %>% 
    as.data.frame() %>% 
    mutate(Uso_Grupo = paste0("area_",Uso_Grupo)) %>% 
    reshape2::dcast(ID_hex~Uso_Grupo, value.var = c("built_area")) %>%
    mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)),
           area_TOTAL = rowSums(across(2:ncol(.)), na.rm = T)) %>% 
    mutate(year = i) %>% 
    select(1,11,10,8,2,3,4,5,6,9,7) %>% 
    filter(ID_hex != "")
  
  names(b2_area_cons)[3:11] <-paste0(
    "O04_built_area",str_pad(1:9,width = 2,pad = 0),"_",names(b2_area_cons)[3:11])
  
  
  ### 1.2.3. Base número de construcciones ----
  message("Panel de número de construcciones ...")
  b3_num_cons <- construcciones %>% 
   # sample_n(2000) %>% 
    mutate(Uso_Grupo = ifelse(is.na(Uso_Grupo),"Others",Uso_Grupo)) %>%
    group_by(ID_hex, Uso_Grupo) %>% 
    summarise(constructions = n()) %>% 
    as.data.frame() %>% 
    mutate(Uso_Grupo = paste0("buildings_",Uso_Grupo)) %>% 
    reshape2::dcast(ID_hex~Uso_Grupo, value.var = c("constructions")) %>%
    mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)),
           buildings_TOTAL = rowSums(across(2:ncol(.)), na.rm = T)) %>% 
    mutate(year = i) %>% 
    select(1,11,10,8,2,3,4,5,6,9,7) %>% 
    filter(ID_hex != "")
  
  
  names(b3_num_cons)[3:11] <-paste0(
    "O05_buildings",str_pad(1:9,width = 2,pad = 0),"_",names(b3_num_cons)[3:11])
  
  
  ### 1.2.4. Bases pisos ----
  message("Panel de número de pisos")
  b4_pisos <- construcciones %>% 
    #sample_n(2000) %>% 
    mutate(Uso_Grupo = ifelse(is.na(Uso_Grupo),"Others",Uso_Grupo)) %>%
    group_by(ID_hex) %>% 
    summarise(O06_floors00_Avg = mean(ConNPisos, na.rm = T),
              O06_floors01_TOTAL = sum(ConNPisos, na.rm = T)) %>% 
    mutate(year = i) %>% 
    select(ID_hex, year, O06_floors00_Avg,O06_floors01_TOTAL) %>% 
    filter(ID_hex != "")
  
  
  
  ## 1.3. Unir resultados
  message("Uniones finales ...")
  pr <- 
    merge(b1_lotes, b2_area_cons, by =
            c("ID_hex", "year"), all = T) %>% 
    merge(b3_num_cons,by =
            c("ID_hex", "year"), all = T) %>% 
    merge(b4_pisos, by =
            c("ID_hex", "year"), all = T) %>% 
    mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)))
  
  FINAL <- rbind(FINAL, pr)
  message(paste0("Final de la base de ",i,"-----------"))
  rm(lotes, construcciones, b1_lotes, b2_area_cons,b3_num_cons,b4_pisos)
  
}









# 2. Union Panel Crudo ----

Matriz_construcciones <- 
  merge(
  readRDS("00_data/M00_Panel_vacio_Hexagonos.RDS"),
  FINAL, by = c("ID_hex","year"), all.x = T) %>% 
  mutate(across(2:ncol(.),~ifelse(is.na(.),0,.)))

saveRDS(Matriz_construcciones, "00_data/Matrices_listas_por_componente/C03_Construcciones.RDS")







