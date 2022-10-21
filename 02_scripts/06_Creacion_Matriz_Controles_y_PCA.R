# Matriz de time-invariant controls controles

library(dplyr)
library(ggplot2)
library(stringr)


# 0. Cargar información ----
a0.raw.espacial <- readRDS("00_data/M02_Hexonos_puntos_MIB_controles_y_adicionales.RDS")



# 1. Ediciones generales ---- 

## 1.1. Selaccionar variables de control ----
a1.controls <- a0.raw.espacial %>% 
  select(ID_hex, DANE_Pop_den2005, Dis_estrato_1:Dis_estrato_6, 
         starts_with("POT"),starts_with("PrePOT"))

## 1.2. Codificar variables ----
names(a1.controls)[2:ncol(a1.controls)] <-
  paste0("C",str_pad(1:ncol(a1.controls)-1,2,pad = 0),"_",
       names(a1.controls)[2:ncol(a1.controls)])


# 2. Definición de la matiz final ----


## 2.1. Agregar información básica de tratamiento ----
M1.control <- readRDS("00_data/M01_hexagonal_matrix_simple.RDS") %>% 
  select(ID_hex, general_group, neighborhood_order) %>% 
  group_by(ID_hex) %>% 
  summarise(across(general_group:neighborhood_order,~first(.))) %>% 
  as.data.frame() %>% 
  mutate(treated = ifelse(general_group == "no tratados",F,T)) %>% 
  merge(a1.controls, cy = "ID_hex")

## 2.2. Matriz logarítmica ----
# M2.control <- M1.control %>% 
#   mutate(across(5:ncol(M1.control),~log(1+.))) %>% 
#   as.data.frame()



# 3. Análilsis de componentes principales (PCA) ----
M1.pca <- M1.control %>% filter(!is.na(C00_DANE_Pop_den2005))

## 3.1. PCA por dimension

### 3.1.1. Información socioeconómica y demográfica ----
pca_1 <- prcomp((M1.pca %>% select(C00_DANE_Pop_den2005:C06_Dis_estrato_6)), 
        center = T, scale. = T)

### 3.1.2. Planeación Urbana ----
pca_2 <- prcomp((M1.pca %>% select(C07_POT1_expansion_Dis:C16_POT4_ambietal_prot)), 
                center = T, scale. = T)

### 3.1.3. Historia de la planeación ----
pca_3 <- prcomp((M1.pca %>% select(C17_PrePOT1_PIDUZOB1_Dis:C20_PrePOT3_Surbogota_Dis)), 
                center = T, scale. = T)

### 3.1.4. Planeación e Hisotria (Combinación) ----
pca_4 <- prcomp((M1.pca %>% select(C07_POT1_expansion_Dis:C20_PrePOT3_Surbogota_Dis)), 
                center = T, scale. = T)

### 3.1.5. TODO (Combinación) ----
pca_5 <- prcomp((M1.pca %>% select(C00_DANE_Pop_den2005:C20_PrePOT3_Surbogota_Dis)), 
                center = T, scale. = T)




## 3.2. Agrupación de resumenes

pr <- rbind(
  summary(pca_1)[[6]] %>% as.data.frame() %>% select(1:4) %>% 
    mutate(Dimension = "Socioeconomic (k = 7)"),
  summary(pca_2)[[6]] %>% as.data.frame() %>% select(1:4) %>% 
    mutate(Dimension = "POT (k = 10)"),
  summary(pca_3)[[6]] %>% as.data.frame() %>% select(1:4) %>% 
    mutate(Dimension = "Pre-POT (k = 4)"),
  summary(pca_4)[[6]] %>% as.data.frame() %>% select(1:4) %>% 
    mutate(Dimension = "POT and Pre-POT (k = 14)"),
  summary(pca_5)[[6]] %>% as.data.frame() %>% select(1:4) %>% 
    mutate(Dimension = "All togather (k = 21)")) %>% 
  add_rownames(var = "Metric") %>% 
  select(Dimension, Metric, PC1:PC4) %>% 
  mutate(across(PC1:PC4, ~round(.,2)),
         Metric = str_remove_all(Metric,"\\d")) %>% 
  filter(Metric == "Cumulative Proportion") %>% 
  select(-Metric)

pr


stargazer::stargazer(pr, summary = F, rownames = F,
                     title = "PCA cumulative variance over control varaibles' dimensions")










# Regresiones exploratoio

######### Exploración
x <- paste("treated~",(paste(names(M1.control)[5:ncol(M1.control)], 
                             collapse = " + ")))


m1.lm <- lm(as.formula(x),
            M1.control)
m1.glm <- glm(as.formula(x),
              M2.control, family = "binomial")

m2.lm <- lm(as.formula(x),
            M2.control)
m2.glm <- glm(as.formula(x),
              M2.control, family = "binomial")



stargazer::stargazer(m1.lm,m1.glm,m2.lm,m2.glm, type = "text")


