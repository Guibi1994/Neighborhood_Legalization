## Análisis inicial
library(stargazer)
library(dplyr)
library(ggplot2)
library(did)
library(kableExtra)
library(stringr)
`%!in%` = Negate(`%in%`)

# 0. Cargar matriz inicial ----
M0_raw <- readRDS("00_data/M01_hexagonal_matrix_simple.RDS") %>% 
  merge(readRDS("00_data/Matrices_listas_por_componente/C02_Reasentamientos.RDS"),
        by = c("ID_hex", "year"))



# 1. Adicion de varaible ----
M1_incial <- M0_raw %>% 
  mutate(
    # Tratamientos simples ----
    ## 1.1. Dummy (time-invariant) si pertenece a grupo de control (legalizacion)
    T0_treated_group = ifelse(is.na(legalization_year),0,1),
    
    ## 1.2. Dummy (time-invariant) si pertenece a grupo de control (MIB)  ----
    
    
    # Arreglos años de tratamientos
    ## 1.3. Correción de año de legalización ----
    legalization_year = ifelse(is.na(legalization_year),0,legalization_year),
    
    ## 1.4. Corección de año de MIB ----
    )



# 2. Tablas de muestras ----

## 2.1. Distribución de observaciones por monitorio ----

mr1 <- M1_incial %>% 
  mutate(
    # Grupos experimetnales
    general_group = case_when(
      general_group =="tratados despues de 2005"~as.character(legalization_year),
      general_group == "no tratados"~"Never trated",
      T~"Treated before 2005"),
    
    # Grupos de monitoreo
    monitoring_group = 
      case_when(total_monitoring == 15~"I. Always",
                total_monitoring >0~"II. Not always",
                T~"III. Never"))

pr <-mr1 %>% group_by(ID_hex) %>% 
  summarise(across(general_group:monitoring_group,~first(.))) %>% 
  as.data.frame() %>% 
  group_by(general_group, monitoring_group) %>% 
  summarise(n = n()) %>% 
  as.data.frame() %>% 
  reshape2::dcast(general_group~monitoring_group, value.var = "n") %>% 
  rename(`Group` = general_group)  %>% 
  rowwise() %>% 
  mutate(Total = sum(c(`I. Always`,`II. Not always`,`III. Never`), na.rm = T))
  

rbind(
  pr %>% mutate(across(2:4,~ifelse(is.na(.),"-",as.character(.)))),
  pr %>% 
    mutate(across(2:4,~ifelse(is.na(.),"-",
                              paste0("(",round((100*./Total),1)," %)"))))) %>% 
  as.data.frame() %>% 
  arrange(Group) %>% 
  kbl(format = "latex", booktabs = T, align = "c", 
    caption = "Distribution observations of tratment status by monitoring group") %>% 
  add_header_above(c("", "Monitoring frequency group"=3),italic = T, 
                   align = "c") %>% 
  pack_rows("Pre-court intervention",1,8) %>%
  pack_rows("Post-court intervention",9,28, hline_before =  T) %>%
  pack_rows(" ",29,32, hline_before =  T) %>%
  collapse_rows(columns = c(1,5), latex_hline = "none")
    
  

## 2.2. Distribucuón de Never-treteted por monitoreo

pr <- mr1 %>% filter(neighborhood_order != "treated") %>% 
  select(ID_hex,monitoring_group, neighborhood_order) %>% 
  group_by(ID_hex) %>% 
  summarise(across(1:2,~first(.))) %>% 
  as.data.frame() %>% 
  group_by(monitoring_group, neighborhood_order) %>% 
  summarise(n = n()) %>% 
  reshape2::dcast(neighborhood_order~monitoring_group, value.var = "n") %>% 
  rename(`Control group` = neighborhood_order) %>% 
  rowwise() %>% 
  mutate(Total = sum(c(`I. Always`,`II. Not always`,`III. Never`), na.rm = T))


rbind(
  pr %>% mutate(across(2:4,~ifelse(is.na(.),"-",as.character(.)))),
  pr %>% 
    mutate(across(2:4,~ifelse(is.na(.),"-",
                              paste0("(",round((100*./Total),1)," %)"))))) %>% 
  as.data.frame() %>% arrange(`Control group`) %>% 
  kbl(format = "latex", booktabs = T, align = "c", 
      caption = "Never trated units' distribution by\n monitoring group") %>% 
  add_header_above(c("", "Monitoring frequency group"=3),italic = T, 
                   align = "c") %>% 
  collapse_rows(columns = c(1,5), latex_hline = "none")
  


##################




# 1. 
M2_prueba <- M1_incial %>% 
  filter(continus_monitoring == T, total_monitoring >= 15, general_group != "tratados antes de 2005")


  filter(neighborhood_order %in% c("treated","outsider")) 


m1.ili_ocu.nt <- att_gt(yname = "RES01_identificaion_m2", tname = "year",
               idname = "ID_hex", gname = "legalization_year", 
               ## DATA
               data = M2_prueba %>% filter(neighborhood_order %in% c("treated","outsider")),
               ###
               xformla = NULL, est_method = "dr", control_group = "nevertreated",
               bstrap = TRUE, biters = 1000, print_details = FALSE, 
               clustervars = "ID_hex", panel = TRUE)

m1.ili_ocu.nt <- att_gt(yname = "RES01_identificaion_m2", tname = "year",
                        idname = "ID_hex", gname = "legalization_year", 
                        ## DATA
                        data = M2_prueba %>% filter(neighborhood_order %in% c("treated","outsider")),
                        ###
                        xformla = NULL, est_method = "dr", control_group = "nevertreated",
                        bstrap = TRUE, biters = 1000, print_details = FALSE, 
                        clustervars = "ID_hex", panel = TRUE)






# 2. Agregations

## General ATT
ag00_general <- aggte(atts, type = "simple")
summary(ag00_general)


## Group ATT
ag01_group <- aggte(atts, type = "group")
summary(ag01_group)
pr <- did::tidy(ag01_group) %>% 
  mutate(pval = round(exp(-0.717*(estimate/std.error)-0.416*(estimate/std.error)^2),4),
         pmark = case_when(
           pval <=.01~"***",
           between(pval,.01,.05)~"**",
           between(pval,.05,.1)~"*",
           T~"")) %>% 
  mutate(coef = paste0(round(estimate,3),pmark),
         se = paste0("(",round(std.error,3),")")) %>% 
  select(Group = group, coef,se) %>% 
  reshape2::melt(id.vars = "Group",
                 value.name = "Iligal ocupations") %>% 
  arrange(Group) %>% mutate(Group = ifelse(variable =="se","",Group)) %>% 
  select(-variable) %>% as.data.frame()

pr

stargazer::stargazer(pr, summary = F, rownames = F,colnames = T,
                     title = "Group unconditional effects using only 5th order neighbours")



# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")


ggdid(agg_effects_es) +
  theme_minimal()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_color_manual(values = c("brown2","grey45"))+
  labs(title = "(A)")+
  theme(text = element_text(family = "serif"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")


ggsave("04_figures/plots/06_Event_study_plot.png")









######
# atts <- att_gt(yname = "RES01_identificaion_m2", # LHS variable
#                tname = "year", # time variable
#                idname = "ID_hex", # id variable
#                gname = "legalization_year", # first treatment period variable
#                data = M2_prueba, # data
#                xformla = NULL, # no covariates
#                #xformla = ~ l_police, # with covariates
#                est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
#                control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
#                bstrap = TRUE, # if TRUE compute bootstrapped SE
#                biters = 1000, # number of bootstrap iterations
#                print_details = FALSE, # if TRUE, print detailed results
#                clustervars = "ID_hex", # cluster level
#                panel = TRUE) # whether the data is panel or repeated cross-sectional
