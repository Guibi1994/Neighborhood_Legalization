# Regresiones final
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
  filter(M06_monitoring_group == "I. Always", 
         T1_LG03_general_group != "Treated before 2005")


a2_all_units <- a0_Matriz_inicial %>% 
  filter(T1_LG03_general_group != "Treated before 2005")



## 0.3. Crear base de iteaciones ----
### 0.3.1. Extraer variables outcomes ----
outcomes <- names(a0_Matriz_inicial) %>% as.data.frame() %>% 
  mutate(outcomes = str_detect(.,"^O")) %>% filter(outcomes ==T) %>% 
  select(1) %>% as.vector() %>% unlist()

write.csv(outcomes, "00_data/004_ouctome_names.csv", row.names = F)



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
    c("T1_LG01_legalization_year","TP1_leg_1st_neighborhood","TP1_leg_2nd_neighborhood",
      "TP1_leg_3rd_neighborhood","TP1_leg_4th_neighborhood")) %>% 
  
  mutate(placebo = case_when(
    str_detect(Var2,"^TP1_leg_1")==T~"1st Placebo",
    str_detect(Var2,"^TP1_leg_2")==T~"2nd Placebo",
    str_detect(Var2,"^TP1_leg_3")==T~"3rd Placebo",
    str_detect(Var2,"^TP1_leg_4")==T~"4th Placebo",
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
    tname = "year", idname = "ID_hex", gname = "T1_LG01_legalization_year", 
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
    tname = "year", idname = "ID_hex", gname = "T1_LG01_legalization_year", 
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
  list("Models group name" = "legalization: Always monitored and no anticipation",
       "Simple"=R1_simple, "Gruop" = R2_group,"Events"= R3_event_sudy)
saveRDS(M1_always_monitored, "03_results/R01leg_Always_monitored_no_anticipation.RDS")


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
  list("Models group name" = "legalization: Always monitored and no anticipation",
       "Simple"=R1_simple, "Gruop" = R2_group,"Events"= R3_event_sudy)
saveRDS(M2_always_monitored_placebos, "03_results/R01leg_Always_monitored_no_anticipation_placebos.RDS")







# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -





# ZONA DE PRUBAS ----

## Ferreteria de loops ----
R1_simple <- data.frame()
R2_group <- data.frame()
R3_event_sudy <- data.frame()

{
  R1_simple <- data.frame()
  R2_group <- data.frame()
  R3_event_sudy <- data.frame()
  
i <- 5
m1 <- att_gt(
  #### I.a. Outcome:
  yname = iterations[i,1],
  #### I.b. DATA y Grupos de controles:
  data = M2_prueba %>% 
    filter(T1_LG04_neighborhood_order %in%
             c(str_split(iterations[i,2],pattern = "\\,") %>% unlist())),
  #### I.c. Other parameters:
  tname = "year", idname = "ID_hex", gname = "T1_LG01_legalization_year", 
  xformla = ~PC1+PC2+PC3+PC4+PC5,
  est_method = "dr", control_group = "nevertreated",
  bstrap = TRUE, biters = 10000, print_details = FALSE, 
  clustervars = "ID_hex", panel = TRUE, cores = 8)


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
}





## Alforja de gráficas ----






{
pr <- sample(outcomes,1,replace = T)
#pr <- outcomes[1]
M1_monitored_unconditional[[1]] %>% filter(outcome == pr) 
ggpubr::ggarrange(
  M1_monitored_unconditional[[1]] %>% 
  filter(outcome == pr) %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96)))+
  labs(title = "General effects",x="Neighborhhod order",y="")+
  geom_errorbar(width = 0, color = "grey60")+
  geom_point(color = "brown2")+
  geom_hline(yintercept = 0, lty =2, color = "red")+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5)),



  M1_monitored_unconditional[[3]] %>% 
  filter(outcome == pr) %>% 
  mutate(event.time = 
           case_when(
             control_group == "1st. neighbors"~event.time-0.35,
             control_group == "2nd. neighbors"~event.time-0.25,
             control_group == "3rd. neighbors"~event.time-0.15,
             control_group == "4th. neighbors"~event.time-.05,
             control_group == "5th. neighbors"~event.time+0.1,
             T~event.time+0.2)) %>% 
  ggplot(aes(event.time, estimate, color = control_group, group = control_group,fill =control_group,
             ymin = estimate-(std.error*1.96), ymax = estimate+(std.error*1.96)))+
  coord_cartesian(xlim = c(-4.8,10.8))+
  geom_point(size = 1)+geom_errorbar(width = 0)+
  geom_hline(yintercept = 0,lty = 2)+
  labs(title = "Event study effects")+
  scale_color_manual(values = c("grey80","grey60","grey40","black","cyan4","brown2"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_minimal()+
  labs(y = "", x = "Event time")+
  theme(text = element_text(family = "serif", size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.title = element_blank(),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())) %>% 
  annotate_figure(top =  text_grob(
    paste0(pr,"\n"), face = "bold",family = "serif", size = 14))
}








pr <- sample(outcomes,1,replace = T)
pr <- outcomes[27]
#pr <- outcomes[1]
R1_simple %>% filter(outcome == pr) 
R1_simple %>% 
  filter(outcome == pr) %>% 
  ggplot(aes(placebo, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = pr,x="Neighborhhod order",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.3))+
  geom_point(position=position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","cyan4"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")






data.frame(
  group_by()
)








