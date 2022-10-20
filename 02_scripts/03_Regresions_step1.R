## Análisis inicial
library(stargazer)
library(dplyr)
library(ggplot2)
library(did)
`%!in%` = Negate(`%in%`)

# Cargar matriz inicial
M0_raw <- readRDS("00_data/M01_hexagonal_matrix_simple.RDS") %>% 
  merge(readRDS("00_data/Matrices_listas_por_componente/C02_Reasentamientos.RDS"),
        by = c("ID_hex", "year"))



# Variables adicoinmales de legalizaicón
M1_incial <- M0_raw %>% 
  # Variables de tratamiento
  mutate(T0_treated_group = ifelse(is.na(legalization_year),0,1),
         
         T1_treated_period = ifelse(year>= legalization_year ,1,0),
         T1_treated_period = ifelse(is.na(T1_treated_period),0,
                                    T1_treated_period),
          
         T2_treatment_dynamic = year-legalization_year,
         T2_treatment_dynamic = ifelse(is.na(T2_treatment_dynamic),0,
                                       T2_treatment_dynamic),
         legalization_year = ifelse(is.na(legalization_year),0,legalization_year))

table(M1_incial$legalization_year[M1_incial$year == 2005]) # Tratados por año


##################

# Filtros para todos

filter(continus_monitoring == T, total_monitoring >= 15, 
       general_group != "tratados antes de 2005", 
       year != 2019)


# 1. Incondicionales | Controles = Outsiders (pure control)
M2_prueba <- M1_incial %>% filter(neighborhood_order %in% c("treated","outsider"))


atts <- att_gt(yname = "RES01_identificaion_m2", tname = "year",
               idname = "ID_hex", gname = "legalization_year", 
               data = M2_prueba, # data
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
