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
  mutate(
    # Tratamientos simples ----
    ## 1.1. Dummy (time-invariant) si pertenece a grupo de control (legalizacion)
    T0_treated_group = ifelse(is.na(legalization_year),0,1),
    legalization_year = ifelse(is.na(legalization_year),0,legalization_year))


names(M0_raw) %>% as.data.frame %>% write.csv("nombres.csv")
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
    legalization_year = ifelse(is.na(legalization_year),0,legalization_year))





## 2.3. Monitored firs in 2005 y monitoring time ----
table(pr$general_group[pr$first_monitored <2007], 
      pr$total_monitoring[pr$first_monitored <2007]) %>% 
  as.data.frame() %>% 
  reshape2::dcast(Var1~Var2,value.var = "Freq") %>% 
  rename(`Tratment group` = 1) %>% 
  mutate(across(2:15,~ifelse(.==0,"-",as.character(.)))) %>% 
  ## LaTeX
  kbl(caption = "Units first monitored in 2005 by number of times monitored",
      format = "latex", format.args = list(big.mark = ","),
      align = "c", booktabs = T) %>% 
  add_header_above(c("", "Years monitored"=14), italic = T) %>% 
  pack_rows("Pre-court intervention",1,4) %>%
  pack_rows("Post-court intervention",5,10, hline_before =  T) %>%
  pack_rows(" ",11,12, hline_before =  T)








# 3. Preparacion regresiones ----

## 3.1. Base de iteraciones ----
iterations <-
  expand.grid(
    # Outcomes
    c("iligal_ocupations"
      # "RES00_identificaion_count","RES01_identificaion_m2",
      # "RES03_ingresos_count","RES04_ingresos_m2"
      ),
    # Control groups
    c("Treated,1st neighborhood,2nd neighborhood,3rd neighborhood,4th neighborhood,5th neighborhood",
      "Treated,1st neighborhood","Treated,2nd neighborhood",
      "Treated,3rd neighborhood","Treated,4th neighborhood","Treated,5th neighborhood")) %>% 
  # Etiquetas de grupos de control
  mutate(across(1:2,~as.character(.)),
         control = case_when(
           str_detect(Var2,"^Treated,1st neighborhood,2")==T~"All controls",
           str_detect(Var2,"^Treated,1st neighborhood$")==T~"1st. neighbors",
           str_detect(Var2,"^Treated,2")==T~"2nd. neighbors",
           str_detect(Var2,"^Treated,3")==T~"3rd. neighbors",
           str_detect(Var2,"^Treated,4")==T~"4th. neighbors", 
           T~"5th. neighbors"))
  
# 3.2. Bases de resultados
R1_simple <- data.frame()
R2_group <- data.frame()
R3_event_sudy <- data.frame()
  
  
# 4. Regresiones ----
## 3.1. Unconditional & "allways monitored sample"
## Base inicial
M2_prueba <- M1_incial %>% 
  filter(continus_monitoring == T, 
         total_monitoring >= 15, 
         general_group != "tratados antes de 2005")

for (i in 1:nrow(iterations)) {
  
  #I. Regresion
  m1 <- att_gt(
    ## I.a. Outcome:
    yname = iterations[i,1],
    ## I.b. DATA y Grupos de controles:
    data = M2_prueba %>% 
      filter(neighborhood_order %in%
               c(str_split(iterations[i,2],pattern = "\\,") %>% unlist())),
    ## I.c. Other parameters:
    tname = "year", idname = "ID_hex", gname = "legalization_year", 
    xformla = NULL, est_method = "dr", control_group = "nevertreated",
    bstrap = TRUE, biters = 10000, print_details = FALSE, 
    clustervars = "ID_hex", panel = TRUE)
  
  # II. Agregación de coficientes por GRUPO
  message(paste0("Listo ",i," de ", nrow(iterations)," modelos:",
                 iterations[i,1]," || ", iterations[i,3]))
  
  # III. Conversión en tabla de resultados
  ## III.a. Simples
  R1_simple <- rbind(
    R1_simple,did::tidy(aggte(m1, type = "simple")) %>% 
      mutate(outcome = iterations[i,1],
             control_group = iterations[i,3],
             covariates = "inconditional",sample = "always monitored"))
  ## III.b. Grupos
  R2_group <- rbind(
    R2_group,did::tidy(aggte(m1, type = "group")) %>% 
      mutate(outcome = iterations[i,1],
             control_group = iterations[i,3],
             covariates = "inconditional",sample = "always monitored"))
  ### III. Event Study
  R3_event_sudy <- rbind(
    R3_event_sudy,did::tidy(aggte(m1, type = "dynamic")) %>% 
      mutate(outcome = iterations[i,1],
             control_group = iterations[i,3],
             covariates = "inconditional",sample = "always monitored"))
  rm(m1)
 
}






R3_event_sudy %>% 
  filter(outcome == "O01_ILO00_iligal_ocupations") %>% 
  mutate(event.time = 
                           case_when(
                             control_group == "1st. neighbors"~event.time-0.35,
                             control_group == "2nd. neighbors"~event.time-0.25,
                             control_group == "3rd. neighbors"~event.time-0.15,
                             control_group == "4th. neighbors"~event.time-.05,
                             control_group == "5th. neighbors"~event.time+0.1,
                             T~event.time+0.2)) %>% 
  ggplot(aes(event.time, estimate, color = control_group, group = control_group,fill =control_group,
             ymin = point.conf.low, ymax = point.conf.high))+
  coord_cartesian(xlim = c(-5.8,10.8))+
  geom_point(size = 1)+geom_errorbar(width = 0)+
  geom_hline(yintercept = 0,lty = 2)+
  scale_color_manual(values = c("grey80","grey60","grey40","black","cyan4","brown2"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_minimal()+
  labs(y = "", x = "Event time")+
  theme(text = element_text(family = "serif", size = 12),
        legend.position = "bottom", legend.title = element_blank(),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())









### PVALUES
r1_incon_always_monitored <-
  rbind(r1_incon_always_monitored,
        did::tidy(g1) %>% 
          mutate(pval = round(exp(-0.717*(estimate/std.error)-0.416*(estimate/std.error)^2),4),
                 pmark = case_when(
                   pval <=.01~"***",
                   between(pval,.01,.05)~"**",
                   between(pval,.05,.1)~"*",
                   T~"")) %>% 
          mutate(coef = paste0(round(estimate,3),pmark),
                 se = paste0("(",round(std.error,3),")"),
                 outcome = i) %>% 
          select(outcome, Group = group, coef,se))
rm(m1,g1)




#### AYUDAS y PRUEBAS 
for (i in 1:6) {
  print(paste0("Iteración #",i))
  print(iterations[i,1])
  print(iterations[i,2])
  print(iterations[i,3])
  print("")
}
c(str_split(iterations[1,2],pattern = "\\,") %>% unlist())










R1_simple <- rbind(
  R1_simple,did::tidy(aggte(m1, type = "simple")) %>% 
    mutate(outcome = iterations[i,1],
           control_group = iterations[i,3],
           covariates = "inconditional",sample = "always monitored"))

R2_group <- rbind(
  R2_group,did::tidy(aggte(m1, type = "group")) %>% 
    mutate(outcome = iterations[i,1],
           control_group = iterations[i,3],
           covariates = "inconditional",sample = "always monitored"))

R3_event_sudy <- rbind(
  R3_event_sudy,did::tidy(aggte(m1, type = "dynamic")) %>% 
    mutate(outcome = iterations[i,1],
           control_group = iterations[i,3],
           covariates = "inconditional",sample = "always monitored"))





pr %>% mutate(grupo = ifelse(event.time <0,"pre-legalization","post-legalization")) %>% 
  ggplot(aes(event.time,estimate,
                  ymin = point.conf.low,
                  ymax =point.conf.high, color = grupo))+
  geom_point()+
  geom_errorbar(width =.1)+
  geom_hline(yintercept = 0, lty = 2)+
  scale_color_manual(values = c("brown3", "grey45"))+
  labs(x ="",y="")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank())
    
ggdid(aggte(m1, type = "dynamic")) +
  theme_minimal()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 16))



































r1_incon_always_monitored %>% 
  mutate(across(2:4,~ifelse(se == "(NA)","-",.))) %>% 
  reshape2::melt(id.vars = c("outcome", "Group")) %>% 
  arrange(outcome,Group) %>% filter(Group != "-") %>%    
  reshape2::dcast(Group+variable~outcome, value.var = "value") %>% 
  select(-2) %>% mutate(across(2:ncol(.),~ifelse(is.na(.),"-",.))) #%>%

  rename(`Iligal ocupations`=2,`Households`=3,m2 =4,`Households.`=5,m2. =6) %>% 
  kbl(caption = "Unconditional group effects over urban informal growth \n(always monitored units)",
      booktabs = T, align = "c", format = "latex") %>% 
  collapse_rows(columns = 1, latex_hline = "none") %>% 
  add_header_above(c(" "=2,"Identifications"=2,"Enrrollments"=2),italic =T) %>% 
  add_header_above(c(" "=2,"Resettlement program"=4))
    
  














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
