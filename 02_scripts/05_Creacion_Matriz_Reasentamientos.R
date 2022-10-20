# Creación Matriz de resentamientos


library(dplyr)
library(stringr)
library(ggplot2)
library(rgdal)
library(sp)
library(sf)

my_GBD = "00_geographic_data\\01_Bases_finales_WGS1984.gdb"


m00_resent_raw <- readOGR(my_GBD, "s02_programa_resentamientos_IDGER_2022_join_hexagons") %>% 
  as("sf") %>% st_set_crs(4326)  %>% as.data.frame() %>% select(-geometry) %>% 
  as.data.frame()

m1_hex_base <- readRDS("00_data/M00_Panel_vacio_Hexagonos.RDS")

m2_Matriz_reasentamientos <- 
  merge(m1_hex_base,(
    m00_resent_raw %>% 
      select(ID_hex, fehca_indentificación, m2_reasentamiento) %>% 
      mutate(count = 1) %>% 
      group_by(ID_hex,year = fehca_indentificación) %>% 
      summarise(RES00_identificaion_count = sum(count, na.rm = T),
                RES01_identificaion_m2 = sum(m2_reasentamiento, na.rm = T))),
    by =  c("ID_hex","year"), all.x = T) %>% 
  mutate(across(RES00_identificaion_count:RES01_identificaion_m2,~
                  ifelse(is.na(.),0,.))) %>% 
  mutate(RES02_identificaion_dummy = ifelse(RES00_identificaion_count>0,1,0)) %>% 
  merge(
    
    
    
    merge(m1_hex_base,(
      m00_resent_raw %>% 
        select(ID_hex, fecha_ingreso, m2_reasentamiento) %>% 
        mutate(count = 1) %>% 
        group_by(ID_hex,year = fecha_ingreso) %>% 
        summarise(RES03_ingresos_count = sum(count, na.rm = T),
                  RES04_ingresos_m2 = sum(m2_reasentamiento, na.rm = T))),
      by =  c("ID_hex","year"), all.x = T) %>% 
      mutate(across(RES03_ingresos_count:RES04_ingresos_m2,~
                      ifelse(is.na(.),0,.))) %>% 
      mutate(RES05_ingresos_dummy = ifelse(RES03_ingresos_count>0,1,0)),
    by =  c("ID_hex","year"))

saveRDS(m2_Matriz_reasentamientos,
        "00_data/Matrices_listas_por_componente/C02_Reasentamientos.RDS")
