# Analisis espacial general
library(rgdal)
library(dplyr)
library(geojsonsf)
library(sp)
library(sf)
library(stringr)
library(ggplot2)
library(leaflet)

my_GBD = "00_geographic_data\\01_Bases_finales_WGS1984.gdb"





# 0. Cargar información ----


## 0.1. Hexagonos ----

### 0.1.1. Polígonos ----
HP1_polygones <- readOGR(my_GBD, "a00_Matriz_Hexagonal_con_tratados") %>% as("sf") %>% 
  st_set_crs(4326) %>% select(GRID_ID, ID_hex, general_group = tratados,
                              legalization_act = NUMERO_ACT_1,
                              legalization_own_code = Mi_codigo_1,
                              pre_legalization_year = Pre_legalizacion,
                              legalization_year = Legalizacion,
                              legalization_type = Tipo_Legalizacion)

### 0.1.2. Puntos ----
HT1_puntos <- readOGR(my_GBD, "a00_Matriz_Hexagonal_con_tratados_puntos") %>% as("sf") %>% 
  st_set_crs(4326) %>% select(GRID_ID, ID_hex, general_group = tratados,
                              legalization_act = NUMERO_ACT_1,
                              legalization_own_code = Mi_codigo_1,
                              pre_legalization_year = Pre_legalizacion,
                              legalization_year = Legalizacion,
                              legalization_type = Tipo_Legalizacion,
                              LG01_dist_legal_all)

## 0.3. Barrios ----
g1_Barrios <- readOGR(my_GBD, "s02_Barrios_UPZ_Localidades") %>% as("sf") %>% 
  st_set_crs(4326) %>% select(3:9)





# 1. Definir grupos vecinales ----


## 1.1. Construir matriz de distancias ----
  # Definir grupos de vecindad

HP2_polygones <- merge(
  HP1_polygones,
  (HT1_puntos %>% st_join(g1_Barrios) %>% 
     mutate(neighborhood_order = 
              case_when(
                between(LG01_dist_legal_all,0,80)~"1st neighborhood",
                between(LG01_dist_legal_all,80,140)~"2nd neighborhood",
                between(LG01_dist_legal_all,140,210)~"3rd neighborhood",
                between(LG01_dist_legal_all,210,280)~"4th neighborhood",
                T~"outsider"),
            neighborhood_order = ifelse(general_group != "no tratados",
                                        "treated",neighborhood_order)) %>% 
     as.data.frame() %>% 
     select(ID_hex,ID_Localid:ID_Barr2, neighborhood_order, -geometry) %>% 
     as.data.frame()), 
  by = "ID_hex")


# 2. Variables de monitoreo ----

## 2.1. Monitoreo del 2005 ----
PM1_monitoring_2005 <- readOGR(my_GBD, "PMon_2005") %>% as("sf") %>% 
  st_set_crs(4326)

PM1_monitoring_2005 <- bind_rows(
  (PM1_monitoring_2005 %>% filter(Tipo == "CONTROL") %>% 
  st_union() %>% st_sf() %>% mutate(monitoring_2005_type = "control")),
  
  (PM1_monitoring_2005 %>% filter(Tipo == "PREVENCION") %>% 
     st_union() %>% st_sf() %>% mutate(monitoring_2005_type = "prevention")),
  
  (PM1_monitoring_2005 %>% filter(Tipo == " ") %>% 
     st_union() %>% st_sf() %>% mutate(monitoring_2005_type = "other"))) %>%
  mutate(m2005 = 1)
  

## 2.2. Otros años ----
HT2_puntos <- HT1_puntos %>% 
  st_join(PM1_monitoring_2005) %>% 
  ### 2006 ----
  st_join((readOGR(my_GBD, "PMon_2006") %>% as("sf") %>% st_set_crs(4326) %>% 
                  st_union() %>% st_sf() %>% mutate(m2006 = 1))) %>% 
  ### 2007 ----
  st_join((readOGR(my_GBD, "PMon_2007") %>% as("sf") %>% st_set_crs(4326) %>% 
       st_union() %>% st_sf() %>% mutate(m2007 = 1))) %>% 
  ### 2008 ----
  st_join((readOGR(my_GBD, "PMon_2008") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2008 = 1))) %>% 
  ### 2009 ----
  st_join((readOGR(my_GBD, "PMon_2009") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2009 = 1))) %>% 
  ### 2010 ----
  st_join((readOGR(my_GBD, "PMon_2010") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2010 = 1))) %>% 
  ### 2011 ----
  st_join((readOGR(my_GBD, "PMon_2011") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2011 = 1))) %>% 
  ### 2012 ----
  st_join((readOGR(my_GBD, "PMon_2012") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2012 = 1))) %>% 
  ### 2013 ----
  st_join((readOGR(my_GBD, "PMon_2013") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2013 = 1))) %>% 
  ### 2014 ----
  st_join((readOGR(my_GBD, "PMon_2014") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2014 = 1))) %>% 
  ### 2015 ----
  st_join((readOGR(my_GBD, "PMon_2015") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2015 = 1))) %>% 
  ### 2016 ----
  st_join((readOGR(my_GBD, "PMon_2016") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2016 = 1))) %>% 
  ### 2017 ----
  st_join((readOGR(my_GBD, "PMon_2017") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2017 = 1))) %>% 
  ### 2018 ----
  st_join((readOGR(my_GBD, "PMon_2018") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2018 = 1))) %>% 
  ### 2019 ----
  st_join((readOGR(my_GBD, "PMon_2019") %>% as("sf") %>% st_set_crs(4326) %>% 
             st_union() %>% st_sf() %>% mutate(m2019 = 1))) %>%
  mutate(across(starts_with("m20"),~ifelse(!is.na(.),.,0)))




HP3_polygones <- merge(
  HP2_polygones,
  (HT2_puntos %>% 
     mutate(monitoring_2005_type = ifelse(is.na(monitoring_2005_type),"not monitored",
                                          monitoring_2005_type),
            total_monitoring = rowSums(across(starts_with("m20")),na.rm = T),
            always_monitored = ifelse(total_monitoring >= 10,T,F)) %>% 
     as.data.frame() %>% 
     select(ID_hex, monitoring_2005_type, starts_with("m20"), 
            total_monitoring, always_monitored,-geometry) %>% 
     as.data.frame() %>% distinct(ID_hex,.keep_all = T)), 
  by = "ID_hex")






# 3. Metricas de monitoreo adicionales ----
PM2_mon_metrics <-  HP3_polygones %>% as.data.frame() %>%  
  filter(total_monitoring != 0) %>% 
  select(ID_hex, starts_with("m20"), total_monitoring)  %>% 
  reshape2::melt(id.vars = c("ID_hex","total_monitoring"),
                 variable.name = "year", value.name = "monitoring") %>% 
  mutate(year = as.numeric(str_remove(as.character(year), "m")),
         year_mon = ifelse(monitoring == 1,year,NA)) %>% 
  group_by(ID_hex) %>% 
  summarise(
    total_monitoring = mean(total_monitoring),
    ## 3.1. Primera vez monitoredo ----
    first_monitored = min(year_mon, na.rm = T),
    ## 3.2. Última vez monitoredo ----
    last_monitored = max(year_mon, na.rm = T)) %>% 
  as.data.frame() %>% 
  mutate(
    ## 3.3. Continuamente monitoreado ----
    continus_monitoring  = 1+(last_monitored-first_monitored),
    continus_monitoring = ifelse(continus_monitoring == total_monitoring,T,F)) %>% 
  select(-total_monitoring)


HP4_polygones <- merge(HP3_polygones,PM2_mon_metrics, by = "ID_hex",
                       all.x = T)



# 4. Preración matriz ocupaciones informales ---- 
PI1_ocupations <- read.csv("00_data/001_ilegal_ocupations_Bogota_2005_2019.csv") %>% 
  st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(4326) %>% 
  st_join((HP1_polygones %>% select(ID_hex))) %>% 
  as.data.frame() %>% 
  select(ID_hex, year, -geometry) %>% as.data.frame() %>%
  group_by(ID_hex, year) %>% 
  summarise(iligal_ocupations = n()) %>% 
  as.data.frame()
  

# 5. Construcción de la Matriz Inicial ----
M1_final <- HP4_polygones %>%
  as.data.frame() %>% select(-geometry) %>% as.data.frame() %>% 
  ## 5.1. Transponer matriz ----
  reshape2::melt(
    id.vars = c(
      "ID_hex","GRID_ID","general_group","legalization_act",
      "legalization_own_code","pre_legalization_year", "legalization_year",
      "legalization_type","ID_Localid" ,"ID_UPZ","ID_Barrio","Nombre_Loc",
      "Nombre_UPZ","Nombre_Bar","ID_Barr2","neighborhood_order",
      "monitoring_2005_type","total_monitoring","always_monitored",
      "first_monitored", "last_monitored","continus_monitoring"), 
    variable.name = "year", value.name = "M1_being_monitored") %>% 
  mutate(year = as.numeric(str_remove(as.character(year), "m"))) %>% 
  
  ## 5.2. Agregar variables de ocupación informal ----
  merge(PI1_ocupations, by = c("ID_hex","year"), all.x = T) %>% 
  mutate(iligal_ocupations =ifelse(
    M1_being_monitored == 1 & is.na(iligal_ocupations),0,iligal_ocupations))
  
# 6. Exportar matriz inicial ----  
saveRDS(M1_final,"00_data/M01_hexagonal_matrix_simple.RDS")






