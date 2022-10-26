# Creación de la matriz final
library(stargazer)
library(dplyr)
library(ggplot2)
library(did)
library(kableExtra)
library(stringr)
`%!in%` = Negate(`%in%`)

# 0. Cargar matriz inicial ----
M0_raw <- readRDS("00_data/M01_hexagonal_matrix_simple.RDS") 


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
    # Monitoreo
    M00_always_monitored = always_monitored, 
    M01_total_monitoring = total_monitoring, 
    M02_continus_monitoring = continus_monitoring, 
    M03_first_monitored = first_monitored, 
    M04_last_monitored = last_monitored, 
    M05_monitoring_2005_type = monitoring_2005_type, 
    # Ocupaciones iligales
    O01_ILO00_iligal_ocupations = iligal_ocupations)
    
  

  