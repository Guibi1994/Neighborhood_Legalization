# Creación de la matriz final
library(stargazer)
library(dplyr)
library(ggplot2)
library(did)
library(kableExtra)
library(stringr)
`%!in%` = Negate(`%in%`)

# 0. Cargar y unir matrices iniciales ----
M0_raw <- readRDS("00_data/M01_hexagonal_matrix_simple.RDS") %>% 
  
  ## 0.1. Unir con matriz de distancias Legalización y MIB ----
  merge((read.csv("00_geographic_data/00_Exportaciones_tabulares/Distacina_legalizacion_MIB.csv") %>% 
           select(ID_hex, starts_with("Dis"))), by = "ID_hex", all.x = T) %>% 
  ## 0.2. Unir con Matriz MIB
  merge(readRDS("00_data/Matrices_listas_por_componente/C01_MIB_y_PreMIB.RDS"),
        by = c("ID_hex","year"), all.x = T) %>% 
  ## 0.3. Unir con matriz de Restanamtienos IDGER ----
  merge(readRDS("00_data/Matrices_listas_por_componente/C02_Reasentamientos.RDS"),
        by = c("ID_hex", "year"), all.x = T) %>% 
  ## 0.4. Unir con matriz de Construcciones ----
  merge(readRDS("00_data/Matrices_listas_por_componente/C03_Construcciones.RDS"),
        by = c("ID_hex", "year"), all.x = T) %>% 
  ## 0.5. Unir con matriz de Controles y PCA ----
  merge(readRDS("00_data/Matrices_listas_por_componente/C04_Controles_y_PCA.RDS"), 
        by = "ID_hex", all.x = T) %>% 
  ## 0.5. Unir con matriz de Placebos ----
  merge(readRDS("00_data/Matrices_listas_por_componente/C05_placebos_legalizacion_y_MIB.RDS"), 
        by = "ID_hex", all.x = T)
  
  
pr <- readRDS("00_data/M03_Matriz_Inicial_Tesis.RDS")


# 1. Ediciones Finales ----
pr <- M0_raw %>%
  mutate(
    ## 1.1. Legaliación ----
    ### 1.1.1. Si es NA en grupo de legalizaicón cambiar por un cero (0) ----
    T0_treated_group = ifelse(is.na(legalization_year),0,1),
    
    ### 1.1.2. Sí es NA en año de legalizaicón cambiar por un cero (0) ----
    legalization_year = ifelse(is.na(legalization_year),0,legalization_year),
    
    ### 1.1.3. Cambiar nombre de "outsider" a "5th neighborhood" ----
    neighborhood_order = ifelse(neighborhood_order=="outsider","5th neighborhood",neighborhood_order),
    neighborhood_order = ifelse(neighborhood_order=="treated","Treated",neighborhood_order),
    
    ### 1.1.4. Legalizaciones "No egalizadas" o "En trámite", cambiarlas a cero (0) en tratamineto y fecha de tratamiento ----
    T0_treated_group = ifelse(legalization_type %in% c("No legalizado","En tramite"),0,T0_treated_group),
    
    ### 1.1.5. Cambair grupo general de español a ingles
    general_group = case_when(
      general_group =="tratados despues de 2005"~as.character(legalization_year),
      general_group == "no tratados"~"Never trated",
      T~"Treated before 2005"),
    
    ## 1.2. MIB ----
    ### 1.2.1. Cambiar nombre de "outsider" a "5th neighborhood" ----
    T2_MIB04_neighborhood_order = ifelse(T2_MIB04_neighborhood_order=="outsider","5th neighborhood",T2_MIB04_neighborhood_order),
    T2_MIB04_neighborhood_order = ifelse(T2_MIB04_neighborhood_order=="treated","Treated",T2_MIB04_neighborhood_order),
    
    ### 1.2.2. Cambiar grupos generales de español a ingles ----
    T2_MIB03_general_group = ifelse(T2_MIB03_general_group == "tratados", as.character(T2_MIB01_MIB_year), "Never trated"),
    
    ## 1.3. Monitoreo ----
    ### 1.3.1. Redefinir grupo de simempre monitoreados ----
    always_monitored = ifelse(total_monitoring ==15,T,F),
    M06_monitoring_group = 
        case_when(total_monitoring == 15~"I. Always",
                  total_monitoring >0~"II. Not always",
                  T~"III. Never")) %>% 
  
  # 2. Reorganizar y renombrar ----
  select(
    ## 2.1. Básicas ----
    ID_hex, GRID_ID, year, 
    ID_Localid, ID_UPZ, ID_Barrio, ID_Barr2, 
    Nombre_Loc, Nombre_UPZ, Nombre_Bar, 
    ## 2.2. Legalización y MIB----
    T1_LG00_was_treated = T0_treated_group, 
    T1_LG01_legalization_year = legalization_year, 
    T1_LG02_pre_legalization_year = pre_legalization_year, 
    T1_LG03_general_group = general_group, 
    T1_LG04_neighborhood_order = neighborhood_order, 
    T1_LG05_legalization_type = legalization_type, 
    T1_LG06_legalization_act = legalization_act, 
    T1_LG07_legalization_own_code = legalization_own_code, 
    T1_LG08_distance_towards_treated = Dis_legalizacion,
    starts_with("T2_"),
    ## 2.3. Monitoreo ----
    M00_always_monitored = always_monitored, 
    M01_total_monitoring = total_monitoring, 
    M02_continus_monitoring = continus_monitoring, 
    M03_first_monitored = first_monitored, 
    M04_last_monitored = last_monitored, 
    M05_monitoring_2005_type = monitoring_2005_type, 
    M06_monitoring_group,
    ## 2.4. Ocupaciones iligales ----
    O01_ILO00_iligal_ocupations = iligal_ocupations,
    ## 2.5. Resentamientgos ----
    O02_RES01_identificaion_count = RES00_identificaion_count,
    O02_RES02_identificaion_m2 = RES01_identificaion_m2,
    O02_RES03_identificaion_dummy = RES02_identificaion_dummy,
    O02_RES04_ingresos_count = RES03_ingresos_count,
    O02_RES05_ingresos_m2 = RES04_ingresos_m2,
    O02_RES06_ingresos_dummy = RES05_ingresos_dummy,
    ## 2.6. Construcciones ----
    starts_with("O03"),
    starts_with("O04"),
    starts_with("O05"),
    starts_with("O06"),
    ## 2.7. Controles  ----
    starts_with("C0"),
    starts_with("C1"),
    starts_with("C2"),
    starts_with("PC"),
    ## 2.8. Placebos ----
    starts_with("TP"))
  


# 3. Exportar la matriz FINAL ----
write.csv(names(pr), "Varaibles_matriz_tesis.csv")
saveRDS(pr, "00_data/M03_Matriz_Inicial_Tesis.RDS")
 