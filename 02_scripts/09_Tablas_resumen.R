# Tablas resumen finales
library(dplyr)
library(stringr)
library(kableExtra)
library(ggplot2)
library(did)

a0_Matriz_inicial <- readRDS("00_data/M03_Matriz_Inicial_Tesis.RDS")





# 1. Tratados por grupo de monitoreo ----

## 1.1. Legalización ----
pr1 <- a0_Matriz_inicial %>%
  group_by(ID_hex) %>% 
  summarise(across(c(T1_LG03_general_group,M06_monitoring_group),~first(.))) %>% 
  group_by(T1_LG03_general_group, M06_monitoring_group) %>% 
  summarise(n = n()) %>%   as.data.frame() %>% 
  reshape2::dcast(T1_LG03_general_group~M06_monitoring_group,value.var = "n") %>%
  rowwise() %>% 
  mutate(Total = sum(c(`I. Always`,`II. Not always`,`III. Never`), na.rm = T)) %>% 
  as.data.frame() %>%  
  rbind(.[] %>% mutate(across(2:4,~paste0("(",round(./Total*100,1),"%)" )))) %>% 
  mutate(across(1:ncol(.),~ifelse(is.na(.)|.==0|.=="(NA%)","-",.))) %>% 
  arrange(T1_LG03_general_group) %>% 
  rename(`Tratment group`=1)
  
    

## 1.2. MIB ----
pr2 <- a0_Matriz_inicial %>%
  group_by(ID_hex) %>% 
  summarise(across(c(T2_MIB03_general_group,M06_monitoring_group),~first(.))) %>% 
  group_by(T2_MIB03_general_group, M06_monitoring_group) %>% 
  summarise(n = n()) %>%   as.data.frame() %>% 
  reshape2::dcast(T2_MIB03_general_group~M06_monitoring_group,value.var = "n") %>%
  rowwise() %>% 
  mutate(Total = sum(c(`I. Always`,`II. Not always`,`III. Never`), na.rm = T)) %>% 
  as.data.frame() %>%  
  rbind(.[] %>% mutate(across(2:4,~paste0("(",round(./Total*100,1),"%)" )))) %>% 
  mutate(across(1:ncol(.),~ifelse(is.na(.)|.==0|.=="(NA%)","-",.))) %>% 
  arrange(T2_MIB03_general_group) %>% 
  rename(`Tratment group`=1) 

  
## 1.3. Unir bases ----
pr3 <- data.frame(
  A  = rep(2005:2019,2),
  C = 1:2) %>% rename(`Tratment group`= 1) %>% 
  merge(pr1 %>% mutate(C = rep(1:2, nrow(.)/2)),
        by  = c("Tratment group","C"), all = T) %>% 
  merge(pr2 %>% mutate(C = rep(1:2, nrow(.)/2)),
        by = c("Tratment group","C"), all = T) %>% 
  mutate(across(1:ncol(.),~ifelse(is.na(.),"-",.))) %>% 
  select(-2)

names(pr3) <- str_remove_all(names(pr3),pattern = ".x$|.y$")


## 1.4. Pasar a LaTeX ----
pr3 %>% 
  kbl(caption = "Treatment groups by monitoring group",
      booktabs = T, align = "c", format = "latex")  %>% 
  kable_styling(font_size = 5) %>% 
  add_header_above(c("","Monitoring frequency group" =3, " "= 1,
                     "Monitoring frequency group"= 3, " "  =1), italic = T) %>% 
  add_header_above(c("", "Legalization program" = 4, "MIB Program"=4)) %>% 
  collapse_rows(columns = c(1,5,9), latex_hline = "none") %>% 
  pack_rows("Pre-court intervention",1,8) %>%
  pack_rows("Post-court intervention",9,30, hline_before =  T) %>% 
  pack_rows(" ",31,34, hline_before =  T)







# 2. Ordenes vecinales por grupo de monitoreo ----

## 2.1. Legalización ----
a0_Matriz_inicial %>%
  group_by(ID_hex) %>% 
  summarise(across(c(T1_LG04_neighborhood_order,M06_monitoring_group),~first(.))) %>% 
  group_by(T1_LG04_neighborhood_order, M06_monitoring_group) %>% 
  summarise(n = n()) %>%   as.data.frame() %>% 
  reshape2::dcast(T1_LG04_neighborhood_order~M06_monitoring_group,value.var = "n") %>%
  rowwise() %>% 
  mutate(Total = sum(c(`I. Always`,`II. Not always`,`III. Never`), na.rm = T)) %>% 
  as.data.frame() %>% 
  rbind(.[] %>% mutate(across(2:4,~paste0("(",round(./Total*100,1),"%)" )))) %>%
  mutate(across(1:ncol(.),~ifelse(is.na(.)|.==0,"-",.))) %>% 
  arrange(T1_LG04_neighborhood_order) %>% 
  rename(`Neighborhood order`=1) %>% 
  #LaTeX
  kbl(caption = "Legalization treatment units by neighborhood order and monitoring group",
      booktabs = T,format = "latex", align = "c") %>% 
  kable_styling(font_size = 8) %>% 
  add_header_above(c("","Monitoring frequency group" =3), italic = T) %>% 
  collapse_rows(columns = c(1,5), latex_hline = "none")

## 2.2. MIB ----
a0_Matriz_inicial %>%
  group_by(ID_hex) %>% 
  summarise(across(c(T2_MIB04_neighborhood_order,M06_monitoring_group),~first(.))) %>% 
  group_by(T2_MIB04_neighborhood_order, M06_monitoring_group) %>% 
  summarise(n = n()) %>%   as.data.frame() %>% 
  reshape2::dcast(T2_MIB04_neighborhood_order~M06_monitoring_group,value.var = "n") %>%
  rowwise() %>% 
  mutate(Total = sum(c(`I. Always`,`II. Not always`,`III. Never`), na.rm = T)) %>% 
  as.data.frame() %>% 
  rbind(.[] %>% mutate(across(2:4,~paste0("(",round(./Total*100,1),"%)" )))) %>%
  mutate(across(1:ncol(.),~ifelse(is.na(.)|.==0,"-",.))) %>% 
  arrange(T2_MIB04_neighborhood_order) %>% 
  rename(`Neighborhood order`=1) %>% 
  #LaTeX
  kbl(caption = "MIB treatment units by neighborhood order and monitoring group",
      booktabs = T,format = "latex", align = "c") %>% 
  kable_styling(font_size = 8) %>% 
  add_header_above(c("","Monitoring frequency group" =3), italic = T) %>% 
  collapse_rows(columns = c(1,5), latex_hline = "none")








