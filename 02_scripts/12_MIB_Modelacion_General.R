library(stargazer)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(did)
library(kableExtra)
library(stringr)
`%!in%` = Negate(`%in%`)



# Notas metdológicas ----

## Nota 1: Iteraciones ----
# Como es necesario correr varios modelos (por cada outcome y grupo de vecindad)
# se construyen bases de "iteraciones" que contienen los hiper-parámetros a tener
# en cuenta en cada modelo. Esta base de iteraciones se integra en un loop para 
# corre todos los modelos en un solo proceso.

## Nota 2: Tipos de resultados ----
# El paquete "did" arroja tres tips de resultados: 1) simples, 2) por grupo o  
# 3) por tiempo de tratamiento (event-study). Estos resultados son guardados en 
# objetos diferentes en cada loop.


# 0. Cargar y definir bases ----

## 0.1. Base inicial ----
a0_Matriz_inicial <- readRDS("00_data/M03_Matriz_Inicial_Tesis.RDS")


## 0.2. Sub bases

### 0.2.1. Solo moniotoreados permanentemente
a1_always_monitored <- a0_Matriz_inicial %>% 
  filter(M06_monitoring_group == "I. Always")


a2_all_units <- a0_Matriz_inicial 



## 0.3. Crear base de iteaciones ----
### 0.3.1. Extraer variables outcomes ----
outcomes <- names(a0_Matriz_inicial) %>% as.data.frame() %>% 
  mutate(outcomes = str_detect(.,"^O")) %>% filter(outcomes ==T) %>% 
  select(1) %>% as.vector() %>% unlist()





### 0.3.2. Expandir grilla de combinaciones de modelos generales ----
iterations <-
  expand.grid(
    #### A) Seleccioanar outcomes para el loop ----
    outcomes,
    
    #### B) Seleccioanar combianciones de grupos de vecindad ----
    c("Treated,1st neighborhood,2nd neighborhood,3rd neighborhood,4th neighborhood,5th neighborhood",
      "Treated,1st neighborhood","Treated,2nd neighborhood","Treated,3rd neighborhood",
      "Treated,4th neighborhood","Treated,5th neighborhood")) %>% 
  #### C) Crear etiquetas de grupos de control (para la base de salida) ----
mutate(across(1:2,~as.character(.)),
       control = case_when(
         str_detect(Var2,"^Treated,1st neighborhood,2")==T~"All controls",
         str_detect(Var2,"^Treated,1st neighborhood$")==T~"1st. neighbors",
         str_detect(Var2,"^Treated,2")==T~"2nd. neighbors",
         str_detect(Var2,"^Treated,3")==T~"3rd. neighbors",
         str_detect(Var2,"^Treated,4")==T~"4th. neighbors", 
         T~"5th. neighbors")) %>% 
  arrange(Var1)


### 0.3.3. Expandir grilla de combinaciones de placebos ----
iterations_placebos <-
  expand.grid(
    #### A) Seleccioanar outcomes para el loop ----
    outcomes[c(1,2,3,5,6,8,9,17,18,26,27,35,36)], # Con discriminación por uso residencial
    #outcomes[c(1,2,3,5,6,8,17,26,35,36)],
    
    #### B) Seleccioanar Placebos ----
    c("T2_MIB01_MIB_year","TP2_MIB_1st_neighborhood","TP2_MIB_2nd_neighborhood",
      "TP2_MIB_3rd_neighborhood","TP2_MIB_4th_neighborhood")) %>% 
  
  mutate(placebo = case_when(
    str_detect(Var2,"^TP2_MIB_1")==T~"1st Placebo",
    str_detect(Var2,"^TP2_MIB_2")==T~"2nd Placebo",
    str_detect(Var2,"^TP2_MIB_3")==T~"3rd Placebo",
    str_detect(Var2,"^TP2_MIB_4")==T~"4th Placebo",
    T~"Standard model")) %>% 
  mutate(across(1:ncol(.),~as.character(.))) %>% 
  as.data.frame()

#### C) Crear etiquetas de grupos de control (para la base de salida) ----








# 1. Modelos "Always monitored"-"No anticipation" ----

## 1.2. Bases por tipo de resultado ----
R1_simple <- data.frame()
R2_group <- data.frame()
R3_event_sudy <- data.frame()

## 1.3. Loop de modelos ----
for (i in 1:nrow(iterations)) {
  ### 1.3.1. INCONDICIONALES ----
  t1 <- Sys.time()
  #### I. Regresion ----
  m1 <- att_gt(
    ##### I.a. Outcome:
    yname = iterations[i,1],
    ##### I.b. DATA y Grupos de controles:
    data = a1_always_monitored %>% 
      filter(T1_LG04_neighborhood_order %in%
               c(str_split(iterations[i,2],pattern = "\\,") %>% unlist())),
    ##### I.c. Other parameters:
    tname = "year", idname = "ID_hex", gname = "T2_MIB01_MIB_year", 
    xformla = NULL, est_method = "dr", control_group = "nevertreated",
    bstrap = TRUE, biters = 10000, print_details = FALSE, 
    clustervars = "ID_hex", panel = TRUE, cores = 8)
  
  #### II. Agregación de coficientes por GRUPO ----
  message(paste0("Listo parte A:",i," de ", nrow(iterations)," modelos:",
                 iterations[i,1]," || ", iterations[i,3], "  ||  INCONDICIONAL"))
  
  #### III. Conversión en tabla de resultados ----
  ## III.a. Simples
  tryCatch({
    R1_simple <- rbind(
      R1_simple,did::tidy(aggte(m1, type = "simple", na.rm = T)) %>% 
        mutate(outcome = iterations[i,1],
               control_group = iterations[i,3],
               covariates = "inconditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base simple :(")}
  )
  ## III.b. Grupos
  tryCatch({
    R2_group <- rbind(
      R2_group,did::tidy(aggte(m1, type = "group", na.rm = T)) %>% 
        mutate(outcome = iterations[i,1],
               control_group = iterations[i,3],
               covariates = "inconditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por grupos :(")}
  )
  #### III. Event Study
  tryCatch({
    R3_event_sudy <- rbind(
      R3_event_sudy,did::tidy(aggte(m1, type = "dynamic", na.rm = T)) %>% 
        mutate(outcome = iterations[i,1],
               control_group = iterations[i,3],
               covariates = "inconditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por eventos :(")}
  )
  
  message(paste0("********* Listos los modelos INCONDICIONALES en: ", 
                 round(as.numeric(Sys.time()-t1),1), " Segundos !!! (",
                 iterations[i,1],")*********"))
  rm(m1,t1)
  
  ### 1.3.2. CONDICIONALES ----
  t1 <- Sys.time()
  #### I. Regresion ----
  m1 <- att_gt(
    #### I.a. Outcome:
    yname = iterations[i,1],
    #### I.b. DATA y Grupos de controles:
    data = a1_always_monitored %>% 
      filter(T1_LG04_neighborhood_order %in%
               c(str_split(iterations[i,2],pattern = "\\,") %>% unlist())),
    #### I.c. Other parameters:
    tname = "year", idname = "ID_hex", gname = "T2_MIB01_MIB_year", 
    xformla = ~PC1+PC2+PC3+PC4+PC5,
    est_method = "dr", control_group = "nevertreated",
    bstrap = TRUE, biters = 10000, print_details = FALSE, 
    clustervars = "ID_hex", panel = TRUE, cores = 8)
  
  #### II. Agregación de coficientes por GRUPO ----
  message(paste0("Listo parte B:",i," de ", nrow(iterations)," modelos:",
                 iterations[i,1]," || ", iterations[i,3], "  ||  CONDITIONAL"))
  
  #### III. Conversión en tabla de resultados ----
  ## III.a. Simples
  tryCatch({
    R1_simple <- rbind(
      R1_simple,did::tidy(aggte(m1, type = "simple", na.rm = T)) %>% 
        mutate(outcome = iterations[i,1],
               control_group = iterations[i,3],
               covariates = "conditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base simple :(")}
  )
  ## III.b. Grupos
  tryCatch({
    R2_group <- rbind(
      R2_group,did::tidy(aggte(m1, type = "group", na.rm = T)) %>% 
        mutate(outcome = iterations[i,1],
               control_group = iterations[i,3],
               covariates = "conditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por grupos :(")}
  )
  #### III. Event Study
  tryCatch({
    R3_event_sudy <- rbind(
      R3_event_sudy,did::tidy(aggte(m1, type = "dynamic", na.rm = T)) %>% 
        mutate(outcome = iterations[i,1],
               control_group = iterations[i,3],
               covariates = "conditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por eventos :(")}
  )
  message(paste0("********* Listos los modelos CONDICIONALES en: ", 
                 round(as.numeric(Sys.time()-t1),1), " Segundos !!! (",
                 iterations[i,1],")*********"))
  print(paste0("Vamos el ",round(((i+1)/nrow(iterations))*100,2), " % del proceso!"))
  rm(m1,t1)
  
}


## 1.4. Agrupa en una lista única ----
M1_always_monitored <- 
  list("Models group name" = "MIB: Always monitored and no anticipation",
       "Simple"=R1_simple, "Gruop" = R2_group,"Events"= R3_event_sudy)
saveRDS(M1_always_monitored, "03_results/R02MIB_Always_monitored_no_anticipation.RDS")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# 2. PLACEBOS: "Always monitored"-"No anticipation" ----

## 2.2. Bases por tipo de resultado ----
R1_simple <- data.frame()
R2_group <- data.frame()
R3_event_sudy <- data.frame()

## 2.3. Loop de modelos ----
for (i in 1:nrow(iterations_placebos)) {
  ### 1.3.1. INCONDICIONALES ----
  t1 <- Sys.time()
  #### I. Regresion ----
  m1 <- att_gt(
    ##### I.a. Outcome:
    yname = iterations_placebos[i,1],
    ##### I.b. DATA y Grupos de controles:
    data = a1_always_monitored,
    ##### I.c. Other parameters:
    tname = "year", idname = "ID_hex", gname = iterations_placebos[i,2], 
    xformla = NULL, est_method = "dr", control_group = "nevertreated",
    bstrap = TRUE, biters = 10000, print_details = FALSE, 
    clustervars = "ID_hex", panel = TRUE, cores = 8)
  
  #### II. Agregación de coficientes por GRUPO ----
  message(paste0("Listo parte A:",i," de ", nrow(iterations_placebos)," modelos:",
                 iterations_placebos[i,1]," || ", iterations_placebos[i,3], 
                 "  ||  INCONDICIONAL"))
  
  #### III. Conversión en tabla de resultados ----
  ## III.a. Simples
  tryCatch({
    R1_simple <- rbind(
      R1_simple,did::tidy(aggte(m1, type = "simple", na.rm = T)) %>% 
        mutate(outcome = iterations_placebos[i,1],
               placebo = iterations_placebos[i,3],
               covariates = "inconditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base simple :(")}
  )
  ## III.b. Grupos
  tryCatch({
    R2_group <- rbind(
      R2_group,did::tidy(aggte(m1, type = "group", na.rm = T)) %>% 
        mutate(outcome = iterations_placebos[i,1],
               placebo = iterations_placebos[i,3],
               covariates = "inconditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por grupos :(")}
  )
  #### III. Event Study
  tryCatch({
    R3_event_sudy <- rbind(
      R3_event_sudy,did::tidy(aggte(m1, type = "dynamic", na.rm = T)) %>% 
        mutate(outcome = iterations_placebos[i,1],
               placebo = iterations_placebos[i,3],
               covariates = "inconditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por eventos :(")}
  )
  
  message(paste0("********* Listos los modelos INCONDICIONALES en: ", 
                 round(as.numeric(Sys.time()-t1),1), " Segundos !!! (",
                 iterations_placebos[i,1],")*********"))
  rm(m1,t1)
  
  ### 2.3.2. CONDICIONALES ----
  t1 <- Sys.time()
  #### I. Regresion ----
  m1 <- att_gt(
    #### I.a. Outcome:
    yname = iterations_placebos[i,1],
    #### I.b. DATA y Grupos de controles:
    data = a1_always_monitored,
    #### I.c. Other parameters:
    tname = "year", idname = "ID_hex", gname = iterations_placebos[i,2], 
    xformla = ~PC1+PC2+PC3+PC4+PC5,
    est_method = "dr", control_group = "nevertreated",
    bstrap = TRUE, biters = 10000, print_details = FALSE, 
    clustervars = "ID_hex", panel = TRUE, cores = 8)
  
  #### II. Agregación de coficientes por GRUPO ----
  message(paste0("Listo parte B:",i," de ", nrow(iterations_placebos)," modelos:",
                 iterations_placebos[i,1]," || ", iterations_placebos[i,3], "  ||  CONDITIONAL"))
  
  #### III. Conversión en tabla de resultados ----
  ## III.a. Simples
  tryCatch({
    R1_simple <- rbind(
      R1_simple,did::tidy(aggte(m1, type = "simple", na.rm = T)) %>% 
        mutate(outcome = iterations_placebos[i,1],
               placebo = iterations_placebos[i,3],
               covariates = "conditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base simple :(")}
  )
  ## III.b. Grupos
  tryCatch({
    R2_group <- rbind(
      R2_group,did::tidy(aggte(m1, type = "group", na.rm = T)) %>% 
        mutate(outcome = iterations_placebos[i,1],
               placebo = iterations_placebos[i,3],
               covariates = "conditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por grupos :(")}
  )
  #### III. Event Study
  tryCatch({
    R3_event_sudy <- rbind(
      R3_event_sudy,did::tidy(aggte(m1, type = "dynamic", na.rm = T)) %>% 
        mutate(outcome = iterations_placebos[i,1],
               placebo = iterations_placebos[i,3],
               covariates = "conditional",sample = "always monitored",
               Observations = m1[[8]]))},
    error = function(e){print("OJO! no se imputo base por eventos :(")}
  )
  message(paste0("********* Listos los modelos CONDICIONALES en: ", 
                 round(as.numeric(Sys.time()-t1),1), " Segundos !!! (",
                 iterations_placebos[i,1],")*********"))
  print(paste0("Vamos el ",round(((i+1)/nrow(iterations_placebos))*100,2), " % del proceso!"))
  rm(m1,t1)
  
}


## 2.4. Agrupa en una lista única ----
M2_always_monitored_placebos <- 
  list("Models group name" = "MIB: Always monitored and no anticipation",
       "Simple"=R1_simple, "Gruop" = R2_group,"Events"= R3_event_sudy)
saveRDS(M2_always_monitored_placebos, "03_results/R02MIB_Always_monitored_no_anticipation_placebos.RDS")
