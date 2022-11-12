# Creación de la matriz final
library(stargazer)
library(dplyr)
library(ggplot2)
library(did)
library(kableExtra)
library(stringr)
`%!in%` = Negate(`%in%`)

# 0. Cargar matriz inicial ----
M0_raw <- readRDS("00_data/M01_hexagonal_matrix_simple.RDS") %>% 
  
  ## 0.1. Unir con matriz de distancias Legalización y MIB
  merge((read.csv("00_geographic_data/00_Exportaciones_tabulares/Distacina_legalizacion_MIB.csv") %>% 
           select(ID_hex, starts_with("Dis"))), by = "ID_hex") %>% 
  ## 0.2. Unir con Matriz MIB
  merge(readRDS("00_data/Matrices_listas_por_componente/C01_MIB_y_PreMIB.RDS"),
        by = c("ID_hex","year")) %>% 
  ## 0.3. Unir con matriz de Restanamtienos IDGER
  merge(readRDS("00_data/Matrices_listas_por_componente/C02_Reasentamientos.RDS"),
        by = c("ID_hex", "year")) %>% 
  ## 0.4. Unir con matriz de Construcciones
  merge(readRDS("00_data/Matrices_listas_por_componente/C03_Construcciones.RDS"),
        by = c("ID_hex", "year")) %>% 
  ## 0.5. Unir con matriz de Controles y PCA
  merge(readRDS("00_data/Matrices_listas_por_componente/C04_Controles_y_PCA.RDS"), 
        by = "ID_hex")


names(M1_MIB) %>% write.csv("names.csv", row.names = F)








pr <- M0_raw %>%
  mutate(
    # Reorganización ----
    # Legaliación
    
    ## Dummy TI de tratamiento de legalización
    T0_treated_group = ifelse(is.na(legalization_year),0,1),
    
    ## Año de legalización = 0 para never treateds
    legalization_year = ifelse(is.na(legalization_year),0,legalization_year),
    
    ## Año de legalización = 0 para never treateds
    legalization_year = ifelse(is.na(legalization_year),0,legalization_year),
    
    ## Cambiar nombre de "outsider" a 
    
    
    ## Pasar a no legalzagos y en proceso a "0" en vairable de tratamineto
    
    
    # Monitoreo de barrios informales (SDH)
    ## Redefinir grupo de simempre monitoreados
    always_monitored = ifelse(total_monitoring ==15,T,F)) %>% 
  
  # Reorganizar y renombrar  
  select(
    # Básicas
    ID_hex, GRID_ID, year, 
    ID_Localid, ID_UPZ, ID_Barrio, ID_Barr2, 
    Nombre_Loc, Nombre_UPZ, Nombre_Bar, 
    # Legalización
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
    # Monitoreo
    M00_always_monitored = always_monitored, 
    M01_total_monitoring = total_monitoring, 
    M02_continus_monitoring = continus_monitoring, 
    M03_first_monitored = first_monitored, 
    M04_last_monitored = last_monitored, 
    M05_monitoring_2005_type = monitoring_2005_type, 
    # Ocupaciones iligales
    O01_ILO00_iligal_ocupations = iligal_ocupations,
    # Resentamientgos
    O02_RES01_identificaion_count = RES00_identificaion_count,
    O02_RES02_identificaion_m2 = RES01_identificaion_m2,
    O02_RES03_identificaion_dummy = RES02_identificaion_dummy,
    O02_RES04_ingresos_count = RES03_ingresos_count,
    O02_RES05_ingresos_m2 = RES04_ingresos_m2,
    O02_RES06_ingresos_dummy = RES05_ingresos_dummy,
    # Construcciones
    starts_with("O03"),
    starts_with("O04"),
    starts_with("O05"),
    starts_with("O06"),
    # Controles 
    starts_with("C0"),
    starts_with("C1"),
    starts_with("C2"),
    starts_with("PC"))
  

write.csv(names(pr), "Varaibles_matriz_tesis.csv")
saveRDS(pr, "00_data/M03_Matriz_Inicial_Tesis.RDS")
 