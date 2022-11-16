# Matriz de valoreación de Spill-overs geográficos
library(dplyr)
library(stringr)

# 1. Cargar la matriz inicial
a0_Matriz_inicial <- readRDS("00_data/M03_Matriz_Inicial_Tesis.RDS") %>% 
  ## 1.1. Selccionr únicamente los años de legalización y MIB
  group_by(ID_hex_imp = ID_hex) %>% 
  summarise(across(c(T1_LG01_legalization_year, T2_MIB01_MIB_year),~first(.))) %>% 
  as.data.frame()


# 2. Placebos de legalización
a1_legalización <- read.csv("00_geographic_data/00_Exportaciones_tabulares/Vecinos_imputaciones_legalizacion.csv") %>% 
  ## 2.1. Eliminar NULLs
  filter(T1_LG04_neighborhood_order !="") %>% 
  ## 2.2. Renombrar ID imputado
  rename(ID_hex_imp = ID_hex_2) %>% 
  ## 2.3. Unir con la base original
  merge(a0_Matriz_inicial, by = "ID_hex_imp", all.x = T) %>% 
  ## 2.4. Crear varaible placebo (año de legalización)
  mutate(TP1_leg_1st_neighborhood = ifelse(T1_LG04_neighborhood_order == "1st neighborhood",T1_LG01_legalization_year,0),
         TP1_leg_2nd_neighborhood = 
           case_when(T1_LG04_neighborhood_order == "1st neighborhood"~as.numeric(NA),
                     T1_LG04_neighborhood_order == "2nd neighborhood"~T1_LG01_legalization_year,
                     T~0),
         TP1_leg_3rd_neighborhood = 
           case_when(T1_LG04_neighborhood_order %in% c("1st neighborhood","2nd neighborhood")~as.numeric(NA),
                     T1_LG04_neighborhood_order == "3rd neighborhood"~T1_LG01_legalization_year,
                     T~0),
         TP1_leg_4th_neighborhood = 
           case_when(T1_LG04_neighborhood_order %in% c("1st neighborhood","2nd neighborhood","3rd neighborhood")~as.numeric(NA),
                     T1_LG04_neighborhood_order == "4th neighborhood"~T1_LG01_legalization_year,
                     T~0)) %>% 
  ## 2.5. Seleccionar variables relevantes
  select(ID_hex, starts_with("TP"))


# 3. Placebos de MIB
a2_MIB <- read.csv("00_geographic_data/00_Exportaciones_tabulares/Vecinos_imputaciones_MIB.csv") %>% 
  ## 2.1. Eliminar NULLs
  filter(T2_MIB04_neighborhood_order !="") %>% 
  ## 2.2. Renombrar ID imputado
  rename(ID_hex_imp = ID_hex_1) %>% 
  ## 2.3. Unir con la base original
  merge(a0_Matriz_inicial, by = "ID_hex_imp", all.x = T) %>% 
  ## 2.4. Crear varaible placebo (año de MIB)
  mutate(TP2_MIB_1st_neighborhood = ifelse(T2_MIB04_neighborhood_order == "1st neighborhood",T2_MIB01_MIB_year,0),
         TP2_MIB_2nd_neighborhood = 
           case_when(T2_MIB04_neighborhood_order == "1st neighborhood"~as.numeric(NA),
                     T2_MIB04_neighborhood_order == "2nd neighborhood"~T2_MIB01_MIB_year,
                     T~0),
         TP2_MIB_3rd_neighborhood = 
           case_when(T2_MIB04_neighborhood_order %in% c("1st neighborhood","2nd neighborhood")~as.numeric(NA),
                     T2_MIB04_neighborhood_order == "3rd neighborhood"~T2_MIB01_MIB_year,
                     T~0),
         TP2_MIB_4th_neighborhood = 
           case_when(T2_MIB04_neighborhood_order %in% c("1st neighborhood","2nd neighborhood","3rd neighborhood")~as.numeric(NA),
                     T2_MIB04_neighborhood_order == "4th neighborhood"~T2_MIB01_MIB_year,
                     T~0)) %>% 
  ## 2.5. Seleccionar variables relevantes
  select(ID_hex, starts_with("TP"))
  
  

# 4. Matriz final y exportación
Matriz_final <- merge(a1_legalización,a2_MIB, by = "ID_hex", all = T) %>% 
  as.data.frame() %>% filter(ID_hex %in% a0_Matriz_inicial$ID_hex_imp)

saveRDS(Matriz_final, "00_data/Matrices_listas_por_componente/C05_placebos_legalizacion_y_MIB.RDS")
  
