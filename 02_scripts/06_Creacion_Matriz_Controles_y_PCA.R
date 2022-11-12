# Matriz de time-invariant controls controles

library(dplyr)
library(ggplot2)
library(stringr)
library(kableExtra)

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
  merge(a1.controls, cy = "ID_hex") %>% 
  mutate(nall = treated,
         n1st = ifelse(neighborhood_order %in% c("1st neighborhood","treated"),treated,NA),
         n2nd = ifelse(neighborhood_order %in% c("2nd neighborhood","treated"),treated,NA),
         n3rd = ifelse(neighborhood_order %in% c("3rd neighborhood","treated"),treated,NA),
         n4th = ifelse(neighborhood_order %in% c("4th neighborhood","treated"),treated,NA),
         n5th = ifelse(neighborhood_order %in% c("outsider","treated"),treated,NA)) %>% 
  mutate(across(nall:n5th,~ifelse(general_group == "tratados antes de 2005",NA,.)))


## 2.2. Test de diferencia de medias ----
options(scipen = 100)

M2.ttest <- 
  data.frame(
    group = character(),
    var = character(),
    dif = numeric(),
    pval = numeric(),
    stars = character())

### 2.2.1. Loop general de T-test's -----
for (j in 26:31) {
for (i in 5:25) {
  print(paste0(names(M1.control[j]),"----",names(M1.control[i])))
  
  ttests <- t.test(M1.control[,i]~M1.control[,j])
  pr <- 
  data.frame(
    group = names(M1.control[j]),
    var = names(M1.control[i]),
    dif = ttests[[5]][[2]]-ttests[[5]][[1]],
    pval = ttests[[3]]) %>% 
    mutate(stars = case_when(pval <=0.01~"***",pval<=0.05~"**",pval<=0.1~"*",T~""))
  
  M2.ttest <- rbind(M2.ttest, pr)
}
}

### 2.2.2. Table en Latex de resultados ----

# Arreglo de matriz
M2.ttest %>% 
  mutate(valor = paste0(round(dif,1)," ",stars)) %>% 
  select(group, var, valor) %>% 
  reshape2::acast(var~group, value.var = "valor") %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("variable") %>% 
  mutate(varr = c(
    "Population density (hab/km2)","Dis. to stratum 1","Dis. to stratum 2",
    "Dis. to stratum 3","Dis. to stratum 4","Dis. to stratum 5",
    "Dis. to stratum 6","Dis. to expansion areas","Dis. to rural areas",
    "Dis. to urban area","Dis. to protected areas","Dis. to consolidation areas-TU",
    "Dis. to development areas-TU","Dis. to CI areas-TU","Dis. to renewal areas-TU",
    "Dis. to high risk zones","Dis. to environmental p. areas","Dis. to PIDUZOM I",
    "Dis. to PIDUZOM II","Dis. to Desmarginalization","Dis. to Sur-Bogotá project")) %>% 
  select(8,1, everything()) %>% 
  select(-2,Variable = varr, `1st`=3,`2nd`=4,`3rd`=5,
         `4th`=6,`5th`=7,`All neighbors`=8) %>% 
  
  # Poyecione en LaTex
  kbl(caption = "T-test by neighborhood orders",
      booktabs = T, format = "latex", align =  c("l","c","c","c","c","c","c")) %>% 
  kable_styling(font_size = 6) %>% 
  add_header_above(c("","Comparation bewteen specific neighborhood orders"=5), italic = T) %>% 
  pack_rows("Socio-economic covariates",1,7) %>% 
  pack_rows("POT covarietes",8,17) %>% 
  pack_rows("Pre-POT covarietes",18,21)




# 3. Análilsis de componentes principales (PCA) ----
M1.pca <- M1.control %>% filter(!is.na(C00_DANE_Pop_den2005))

## 3.1. PCA por dimension ----

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




## 3.2. Agrupación de resumenes ----

PCA_resumen <- bind_rows(
  summary(pca_1)[[6]] %>% as.data.frame() %>% select(1:7) %>% 
    mutate(Dimension = "Socioeconomic (k = 7)"),
  summary(pca_2)[[6]] %>% as.data.frame() %>% select(1:10) %>% 
    mutate(Dimension = "POT (k = 10)"),
  summary(pca_3)[[6]] %>% as.data.frame() %>% select(1:4) %>% 
    mutate(Dimension = "Pre-POT (k = 4)"),
  summary(pca_4)[[6]] %>% as.data.frame() %>% select(1:10) %>% 
    mutate(Dimension = "POT and Pre-POT (k = 14)"),
  summary(pca_5)[[6]] %>% as.data.frame() %>% select(1:10) %>% 
    mutate(Dimension = "All togather (k = 21)")) %>% 
  add_rownames(var = "Metric") %>% 
  select(Dimension, Metric,starts_with("PC")) %>% 
  mutate(across(PC1:PC10, ~round(.,3)),
         Metric = str_remove_all(Metric,"\\d")) %>% 
  filter(str_detect(Metric,"Cumulative") ==T) %>% 
  select(-Metric) %>% 
  mutate(across(1:ncol(.),~ifelse(is.na(.),"-",as.character(.))))

### 3.2.1. Tabla Latex de resumen de PCA ----
PCA_resumen %>% 
  kbl(caption = "PCA cumulative variance over control varaibles' dimensions",
      booktabs = T,format = "latex") %>% 
  kable_styling(font_size = 8) %>% 
  pack_rows("Conbinations",4,5) 
PCA_resumen


### 3.2.2. TEst de diferencia de medias 


pr <- pca_5$x %>% as.data.frame()
M2.pca <- cbind(M1.pca,pr)

M2.ttest <- 
  data.frame(
    group = character(),
    var = character(),
    dif = numeric(),
    pval = numeric(),
    stars = character())

### 3.2.2. Loop general de T-test's -----
for (j in 26:31) {
  for (i in 32:38) {
    print(paste0(names(M2.pca[j]),"----",names(M2.pca[i])))
    
    ttests <- t.test(M2.pca[,i]~M2.pca[,j])
    pr <- 
      data.frame(
        group = names(M2.pca[j]),
        var = names(M2.pca[i]),
        dif = ttests[[5]][[2]]-ttests[[5]][[1]],
        pval = ttests[[3]]) %>% 
      mutate(stars = case_when(pval <=0.01~"***",pval<=0.05~"**",pval<=0.1~"*",T~""))
    
    M2.ttest <- rbind(M2.ttest, pr)
  }
}

M2.ttest %>% 
  mutate(valor = paste0(round(dif,2)," ",stars)) %>% 
  select(group, var, valor) %>% 
  reshape2::acast(var~group, value.var = "valor") %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("Principal component") %>%
  select(1, `1st`=2,`2nd`=3,`3rd`=4,
         `4th`=5,`5th`=6,`All neighbors`=7) %>% 
  kbl(caption = "T-test results over PCA componentes by neighborhood orders", 
      booktabs = T, align = c("c","c","c","c","c","c","c"), format = "latex") %>% 
  add_header_above(c("","Comparation bewteen specific neighborhood orders"=5), italic = T) %>% 
  kable_styling(font_size = 7)


# 4. Matriz Final y exportación ----
Matriz_Final <- 
  M2.pca %>% select(ID_hex, starts_with("C"),PC1:PC10)

saveRDS(Matriz_Final,"00_data/Matrices_listas_por_componente/C04_Controles_y_PCA.RDS")




