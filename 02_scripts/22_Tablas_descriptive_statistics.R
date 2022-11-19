# Descriptive statistics ----
library(dplyr)
library(ggplot2)
library(kableExtra)




# 0. Cargar bases de datos -----
## 0.1. Matriz inicial ----
a0_Matriz_inicial <- readRDS("00_data/M03_Matriz_Inicial_Tesis.RDS")

## 0.2. Matriz condicionada ----
a1_always_monitored <- a0_Matriz_inicial %>% 
  filter(M06_monitoring_group == "I. Always")

## 0.3. Base SIMPLIFICADA de Iterations ----
neighbors <- data.frame(
  "var2"= c("Treated,1st neighborhood,2nd neighborhood,3rd neighborhood,4th neighborhood,5th neighborhood",
           "Treated,1st neighborhood","Treated,2nd neighborhood","Treated,3rd neighborhood",
           "Treated,4th neighborhood","Treated,5th neighborhood"),
  "control" = c("All neighbors","1st. neighbors","2nd. neighbors",
                "3rd. neighbors","4th. neighbors","5th. neighbors"))

## 0.4. Base de outcomes etiquetados ----
outomes <- read.csv("00_data/005_ouctome_names_corregidos.csv") %>% 
  mutate(varibale.ordered = paste0("(",letters[orden],"). ", Variable))




# 1. Complementos a las tabla de modelos ----

## 1.1. Base de loop ----
R1_model_complements <- 
  data.frame(Program= character(),control_group=character(),Variable=n)


## 1.2. Construccion de adiciones a las tablas de regresion ----
for (i in 1:nrow(neighbors)) {
  # Always Monitored
  ## Legalization
  R1_model_complements <- R1_model_complements %>% rbind(
    a1_always_monitored %>% 
      filter(T1_LG04_neighborhood_order %in%
               c(str_split(neighbors[i,1],pattern = "\\,") %>% unlist())) %>% 
      group_by(ID_hex) %>%
      summarise(T1_LG00_was_treated = first(T1_LG00_was_treated)) %>% as.data.frame() %>% 
      group_by(Variable = T1_LG00_was_treated) %>% 
      summarise(n = n()) %>% as.data.frame() %>% 
      group_by() %>% 
      mutate(Variable = ifelse(Variable==1,"Treated units","Control units"),
             n = paste0(round((n/sum(n)*100),1)," %"),
             control_group = neighbors[i,2],
             Program = "legalization") %>% 
      select(4,3,1,2)) %>% 
    ## MIB
    rbind(a1_always_monitored %>% 
            filter(T2_MIB04_neighborhood_order %in%
                     c(str_split(neighbors[i,1],pattern = "\\,") %>% unlist())) %>% 
            group_by(ID_hex) %>%
            summarise(T2_MIB00_was_treated = first(T2_MIB00_was_treated)) %>% as.data.frame() %>% 
            group_by(Variable = T2_MIB00_was_treated) %>% 
            summarise(n = n()) %>% as.data.frame() %>% 
            group_by() %>% 
            mutate(Variable = ifelse(Variable==1,"Treated units","Control units"),
                   n = paste0(round((n/sum(n)*100),1)," %"),
                   control_group = neighbors[i,2],
                   Program = "MIB") %>% 
            select(4,3,1,2))
}

write.csv(R1_model_complements, "00_data/00_6_Additional_model_info.csv")



# 2. Tablas de estadísticas descriptivas ----

## 2.1. Base de loop ----
R2_descriptives <- 
  data.frame(Program = character(),Variable= character(),Avg=numeric(),Std=character())

## 2.2. Ejecucion del loop ----
for (i in 1:nrow(neighbors)) {
  
  ### 2.2.1. Legalization - - - - - ----
  #### 2.2.1.1. Condicionar las bases ----
  pr <- a1_always_monitored %>% 
    filter(T1_LG04_neighborhood_order %in%
             c(str_split(neighbors[i,1],pattern = "\\,") %>% unlist()))
  
  
  R2_descriptives <- R2_descriptives %>% rbind(
    pr %>% 
      #### 2.2.1.2. Calcular PROMEDIOS ----
    group_by() %>% 
      summarise(across(starts_with("O"),~mean(.,na.rm = T))) %>% 
      as.data.frame() %>% mutate(var = "i") %>% 
      reshape2::melt(id.vars = "var", variable.name = "outcome",
                     value.name = "Avg") %>% 
      as.data.frame() %>% 
      mutate(Avg = round(Avg,2)) %>% 
      merge(outomes, by = "outcome", all.x = T) %>% 
      select(orden, Variable, Avg) %>% 
      #### 2.2.1.2. Calcular STD ----
    merge(
      pr %>% 
        group_by() %>% 
        summarise(across(starts_with("O"),~sd(.,na.rm = T))) %>% 
        as.data.frame() %>% mutate(var = "i") %>% 
        reshape2::melt(id.vars = "var", variable.name = "outcome",
                       value.name = "Std") %>% 
        as.data.frame() %>% 
        mutate(Std = paste0("(",round(Std,2),")")) %>% 
        merge(outomes, by = "outcome", all.x = T) %>% 
        select(Variable, Std), by = "Variable") %>% 
      arrange(orden) %>% filter(orden <35) %>%
      mutate(control_group = neighbors[i,2]) %>% 
      mutate(Program = "Legalization") %>% 
      select(6,5,2,1,3,4)) 
  rm(pr)
  
  ### 2.2.2. MIB - - - - - ----
  #### 2.2.2.1. Condicionar las bases ----
  pr <- a1_always_monitored %>% 
    filter(T2_MIB04_neighborhood_order %in%
             c(str_split(neighbors[i,1],pattern = "\\,") %>% unlist()))
  
  
  R2_descriptives <- R2_descriptives %>% rbind(
    pr %>% 
      #### 2.2.1.2. Calcular PROMEDIOS ----
    group_by() %>% 
      summarise(across(starts_with("O"),~mean(.,na.rm = T))) %>% 
      as.data.frame() %>% mutate(var = "i") %>% 
      reshape2::melt(id.vars = "var", variable.name = "outcome",
                     value.name = "Avg") %>% 
      as.data.frame() %>% 
      mutate(Avg = round(Avg,2)) %>% 
      merge(outomes, by = "outcome", all.x = T) %>% 
      select(orden, Variable, Avg) %>% 
      #### 2.2.1.2. Calcular STD ----
    merge(
      pr %>% 
        group_by() %>% 
        summarise(across(starts_with("O"),~sd(.,na.rm = T))) %>% 
        as.data.frame() %>% mutate(var = "i") %>% 
        reshape2::melt(id.vars = "var", variable.name = "outcome",
                       value.name = "Std") %>% 
        as.data.frame() %>% 
        mutate(Std = paste0("(",round(Std,2),")")) %>% 
        merge(outomes, by = "outcome", all.x = T) %>% 
        select(Variable, Std), by = "Variable") %>% 
      arrange(orden) %>% filter(orden <35) %>%
      mutate(control_group = neighbors[i,2]) %>% 
      mutate(Program = "MIB") %>% 
      select(6,5,2,1,3,4)) 
  rm(pr)
  
}


## 3.1. Main outcomes ----
R2_descriptives %>%
  ### 3.1.1. Pegar transpociciones ----
  #### 3.1.1.1. Transponer Promedios ----
  reshape2::dcast(Program+orden+Variable~control_group, value.var = "Avg") %>% 
  #### 3.1.1.2. Transponer STD ----
  rbind(R2_descriptives %>% 
          reshape2::dcast(Program+orden+Variable~control_group, value.var = "Std")) %>% 
  arrange(Program, orden) %>% filter(orden<=10) %>% select(-orden,-Program) %>% 
  
  ### 3.1.2. LaTeX ----
  kbl(caption = "Main outcomes' descriptive statistics", booktabs = T, 
      format.args = list(big.mark = ","), format = "latex",
      align = c("l",rep("c",6))) %>% 
  kable_styling(font_size =  6) %>% 
  add_header_above(c("","Neighborhood orders" =5), italic = T) %>% 
  collapse_rows(1, latex_hline = "none") %>% 
  #### 3.1.2.1. Legalization ----
  pack_rows("By legalization program's neighborhood orders",1,1) %>%
  pack_rows("Ilegal market: ",1,10,bold = F, italic = T, underline = T) %>% 
  pack_rows("Legal market: ",11,20,bold = F, italic = T, underline = T) %>% 
  #### 3.1.2.2. MIB ----
  pack_rows("By MIB program's neighborhood orders",21,21) %>%
  pack_rows("Ilegal market: ",21,30,bold = F, italic = T, underline = T) %>% 
  pack_rows("Legal market: ",31,40,bold = F, italic = T, underline = T) %>%
  # Fotnotes
  footnote(general = "Average values above and standard deviations in parentheses. It is essential to consider that the average area of legalization has historically been 4.5 Ha, and in legalizations, after 2005, the average area is 1.8 Ha. On the other hand, the polygons of the MIB program have an average area of 36.9 Ha. The values came from the pre-filtered sample by the monitoring group.", 
           general_title = "Notes: ", title_format = "bold",
           footnote_as_chunk = T, threeparttable = T)



a1_always_monitored %>% 
  group














