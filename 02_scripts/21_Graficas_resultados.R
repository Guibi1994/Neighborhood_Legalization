# Gráficas de resultados

library(dplyr)
library(ggplot2)
library(gridExtra)
library(egg)
library(stringr)
pval <- function(base) {
  base <- base %>% 
    mutate(pval = round(exp(-0.717*(estimate/std.error)-0.416*(estimate/std.error)^2),4),
           pmark = case_when(
             pval <=.01~"***",
             between(pval,.01,.05)~"**",
             between(pval,.05,.1)~"*",
             T~"")) %>% as.data.frame() %>% 
    mutate(std = ifelse(is.na(std.error),"-", paste0("(",round(std.error,2), ")")),
           estimate = ifelse(is.na(std.error),"-",paste0(round(estimate,2),pmark)))
  return(base)
}

# Cargar bases ----

## Outcomes re-etiquetados ----
outomes <- read.csv("00_data/005_ouctome_names_corregidos.csv") %>% 
  mutate(varibale.ordered = paste0("(",letters[orden],"). ", Variable))

# Resutados generales  ----
a1_leg_general <- readRDS("03_results/R01leg_Always_monitored_no_anticipation.RDS")
a2_MIB_general <- readRDS("03_results/R02MIB_Always_monitored_no_anticipation.RDS")


# 5. LEG: Overall slum management effects ----

## 5.1 Legalization and ILLIGAL construction ----

### A. Condicionar bases y cambiar nombres ----
base <- a1_leg_general[[2]] %>% 
  mutate(control_group = str_sub(control_group,1,3)) %>% 
  filter(outcome %in% outomes$outcome[1:5]) %>% 
  merge(outomes, by = "outcome", all.x = T)

### B. Crear la gráfica general ----
p <- base %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "Legalization's overall effects on the illegal construction market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.4), lwd = 0.3)+
  geom_point(size = 0.5,position=position_dodge(width = 0.4))+
  geom_hline(lwd = 0.5,yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","grey40"))+
  theme_minimal()+
  theme(text = element_text(size = 9, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")


### C. Segmentar la gráfica en partes ----
p1 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[1:3])+
  theme(plot.title = element_blank())
p2 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[4:5]) +
  theme(plot.title = element_blank(), legend.position = "none")+
  labs(x = "Neighborhood orders")

### D. Unir partes y exportar ----
p <- grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(5, "cm"),
  height = unit(4, "cm"))) 
p

ggsave("04_figures/plots/R01_legalization_iligal_effects.png",p,
      h = 5,w = 7)


## 5.2 Legalization and LEGAL construction ----

### A. Condicionar bases y cambiar nombres ----
base <- a1_leg_general[[2]] %>% 
  mutate(control_group = str_sub(control_group,1,3)) %>% 
  filter(outcome %in% outomes$outcome[6:10]) %>% 
  merge(outomes, by = "outcome", all.x = T)

### B. Crear la gráfica general ----
p <- base %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "Legalization's overall effects on the legal construction market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.5), lwd = 0.3)+
  geom_point(size = 0.5,position=position_dodge(width = 0.5))+
  geom_hline(lwd = 0.5,yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","grey40"))+
  theme_minimal()+
  theme(text = element_text(size = 9, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")

p
### C. Segmentar la gráfica en partes ----
p1 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[6:8])+
  theme(plot.title = element_blank())
p2 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[9:10]) +
  theme(plot.title = element_blank(), legend.position = "none")+
  labs(x = "Neighborhood orders")

### D. Unir partes y exportar ----
p <- grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(5, "cm"),
  height = unit(4, "cm"))) 
p

ggsave("04_figures/plots/R02_legalization_legal_effects.png",p,
       h = 5,w = 7)



# 5. MIB: Overall slum management effects ----

## 5.3. MIB and ILLIGAL construction ----

### A. Condicionar bases y cambiar nombres ----
base <- a2_MIB_general[[2]] %>% 
  mutate(control_group = str_sub(control_group,1,3)) %>% 
  filter(outcome %in% outomes$outcome[1:5]) %>% 
  merge(outomes, by = "outcome", all.x = T)

### B. Crear la gráfica general ----
p <- base %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "MIB's overall effects on the illegal construction market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.5), lwd = 0.3)+
  geom_point(size = 0.5,position=position_dodge(width = 0.5))+
  geom_hline(lwd = 0.5,yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","grey40"))+
  theme_minimal()+
  theme(text = element_text(size = 9, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")


### C. Segmentar la gráfica en partes ----
p1 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[1:3])+
  theme(plot.title = element_blank())
p2 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[4:5]) +
  theme(plot.title = element_blank(), legend.position = "none")+
  labs(x = "Neighborhood orders")

### D. Unir partes y exportar ----
p <- grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(5, "cm"),
  height = unit(4, "cm"))) 
p

ggsave("04_figures/plots/R03_MIB_iligal_effects.png",p,
       h = 5,w = 7)


## 5.3 MIB and LEGAL construction ----

### A. Condicionar bases y cambiar nombres ----
base <- a2_MIB_general[[2]] %>% 
  mutate(control_group = str_sub(control_group,1,3)) %>% 
  filter(outcome %in% outomes$outcome[6:10]) %>% 
  merge(outomes, by = "outcome", all.x = T)

### B. Crear la gráfica general ----
p <- base %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "MIB's overall effects on the legal construction market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.5), lwd = 0.3)+
  geom_point(size = 0.5,position=position_dodge(width = 0.5))+
  geom_hline(lwd = 0.5,yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","grey40"))+
  theme_minimal()+
  theme(text = element_text(size = 9, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")

p
### C. Segmentar la gráfica en partes ----
p1 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[6:8])+
  theme(plot.title = element_blank())
p2 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[9:10]) +
  theme(plot.title = element_blank(), legend.position = "none")+
  labs(x = "Neighborhood orders")

### D. Unir partes y exportar ----
p <- grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(5, "cm"),
  height = unit(4, "cm"))) 
p

ggsave("04_figures/plots/R04_MIB_legal_effects.png",p,
       h = 5,w = 7)






# 5.4. Resendential use GENERAL ----


## A. Legalization ----
base <- a1_leg_general[[2]] %>% 
  mutate(control_group = str_sub(control_group,1,3)) %>% 
  filter(outcome %in% outomes$outcome[11:13]) %>% 
  merge(outomes, by = "outcome", all.x = T)
p1 <- base %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "(A) Legalization policy",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.5), lwd = 0.3)+
  geom_point(size = 0.5,position=position_dodge(width = 0.5))+
  geom_hline(lwd = 0.5,yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","grey40"))+
  theme_minimal()+
  theme(text = element_text(size = 9, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(
          size = 7,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")


## B. MIB ----
base <- a2_MIB_general[[2]] %>% 
  mutate(control_group = str_sub(control_group,1,3)) %>% 
  filter(outcome %in% outomes$outcome[11:13]) %>% 
  merge(outomes, by = "outcome", all.x = T)
p2 <- base %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "(B) MIB program",x="Neighborhood orders",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.5), lwd = 0.3)+
  geom_point(size = 0.5,position=position_dodge(width = 0.5))+
  geom_hline(lwd = 0.5,yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","grey40"))+
  theme_minimal()+
  theme(text = element_text(size = 9, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(
          size = 7,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")

## C. Combinación ----
p3 <- ggarrange(p1,p2)
ggsave("04_figures/plots/R05_MIB_vs_LEG_residential.png",p3,
       h = 5.5,w = 7)




p
### C. Segmentar la gráfica en partes ----
p1 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[6:8])+
  theme(plot.title = element_blank())
p2 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[9:10]) +
  theme(plot.title = element_blank(), legend.position = "none")+
  labs(x = "Neighborhood orders")

### D. Unir partes y exportar ----
p <- grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(5, "cm"),
  height = unit(4, "cm"))) 
p

ggsave("04_figures/plots/R04_MIB_legal_effects.png",p,
       h = 5,w = 7)













# 6. Time Study effects ----

## 6.1. Princiaples ----
p_detail <- function(base, seleccionado) {
  base <- base[[4]]
  p <- base %>% 
    filter(outcome %in% outomes$outcome[seleccionado]) %>% 
    merge(outomes, by = "outcome", all.x = T) %>%
    filter(covariates == "conditional") %>% 
    ggplot(aes(event.time, estimate, ymin = estimate+(std.error*1.96), 
               ymax = estimate-(std.error*1.96),
               color = control_group, group = control_group))+
    coord_cartesian(xlim = c(-3.6,13))+
    labs(x="",y="", 
         #title = "Legalization's effects on the legal market (by traetment groups)",
         color = "")+
    geom_vline(xintercept = 0, lty = 4, color = "grey60", lwd = 0.4)+
    geom_errorbar(width = 0.35,position=position_dodge(width = 0.6),lwd = 0.5)+
    geom_point(size = 1, position=position_dodge(width = 0.6))+
    geom_hline(yintercept = 0, lty =2, color = "grey40", lwd = 0.2)+
    scale_color_manual(values = c("black","grey25","grey45","grey65","lightpink2","brown3"))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 15))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
    theme_minimal(base_line_size = 0, base_rect_size = 0)+
    theme(text = element_text(size = 12, family = "serif"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          axis.text.x = element_text(size = 10),
          axis.ticks.y=element_blank())
  print(paste0(seleccionado," - ",outomes$varibale.ordered[seleccionado]))
  return(p)
  
} 
Consulta_et <- function(base,seleccionado) {
  print(paste0(outomes$outcome[seleccionado]," : ",outomes$varibale.ordered[seleccionado]))
  print("  ")
  base[[4]] %>% pval() %>% 
    merge(outomes, by = "outcome", all.x = T)%>% 
    filter(covariates == "conditional") %>% 
    filter(outcome %in% outomes$outcome[seleccionado]) %>% 
    filter(pmark != "") %>% 
    select(Variable ,control_group,event.time,estimate ) %>% 
    reshape2::dcast(Variable+control_group~event.time, value.var = "estimate") %>% 
    mutate(across(2:ncol(.),~ifelse(is.na(.),"-",.)))
}

### 6.1.1. LEG: Illigal ocupations ----

p_detail(a1_leg_general,1)
ggsave("04_figures/plots/R06_LEG_events_illigal_ocupations.png", h = 4.5, w=11)
Consulta_et(a1_leg_general,1)


### 6.1.3. LEG: Avg Stories ----
p_detail(a1_leg_general,10)
ggsave("04_figures/plots/R07_LEG_events_Avg_Sotries.png", h = 4.5, w=11)
Consulta_et(a1_leg_general,10)

### 6.1.4. LEG: Residential area (m2) ----
p_detail(a1_leg_general,12)
ggsave("04_figures/plots/R08_LEG_events_Residential_m2.png", h = 4.5, w=11)
Consulta_et(a1_leg_general,12)



### 6.1.5. MIB Illigal ocupations ----
p_detail(a2_MIB_general,1) +
  coord_cartesian(xlim = c(-3.7,11))
ggsave("04_figures/plots/R10_MIB_events_illigal_ocupations.png", h = 4.5, w=11)
Consulta_et(a2_MIB_general,1)

### 6.1.3. MIB: Avg Stories ----
p_detail(a2_MIB_general,10)+
  coord_cartesian(xlim = c(-3.7,11))
ggsave("04_figures/plots/R11_MIB_events_Avg_Sotries.png", h = 4.5, w=11)
Consulta_et(a2_MIB_general,10)

### 6.1.4. MIB: Residential area (m2) ----
p_detail(a2_MIB_general,12)+
  coord_cartesian(xlim = c(-3.7,11))
ggsave("04_figures/plots/R12_MIB_events_Residential_m2.png", h = 4.5, w=11)
Consulta_et(a2_MIB_general,12)





## 6.2. Complementarios ----

### 6.2.1. Legalización ----
a1_leg_general[[4]] %>% 
  filter(outcome %in% outomes$outcome[c(2:9,11,13)]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(event.time, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_cartesian(xlim = c(-3.6,13))+
  
  labs(x="",y="", 
       #title = "Legalization's effects on the legal market (by traetment groups)",
       color = "")+
  geom_vline(xintercept = 0, lty = 4, color = "grey60")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.6),lwd = 0.3)+
  geom_point(size = .3, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("black","grey25","grey45","grey65","lightpink2","brown3"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_y_continuous(labels = scales::comma)+
  
  facet_wrap(varibale.ordered~ ., scales="free", nrow = 5)+
  
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 15, family = "serif"),
        strip.text = element_text(size = 18), 
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(size = 12),
        axis.ticks = element_line(size = 0.0001))

ggsave("04_figures/plots/R09_LEG_events_aditionals.png", h = 13, w= 15)


### 6.2.2. MIB ----
a2_MIB_general[[4]] %>% 
  filter(outcome %in% outomes$outcome[c(2:9,11,13)],
         event.time >-5) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(event.time, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_cartesian(xlim = c(-3.6,11))+
  
  labs(x="",y="", 
       #title = "Legalization's effects on the legal market (by traetment groups)",
       color = "")+
  geom_vline(xintercept = 0, lty = 4, color = "grey60")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.6),lwd = 0.3)+
  geom_point(size = .3, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("black","grey25","grey45","grey65","lightpink2","brown3"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_y_continuous(labels = scales::comma)+
  
  facet_wrap(varibale.ordered~ ., scales="free", ncol = 2)+
  
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 15, family = "serif"),
        strip.text = element_text(size = 18), 
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(size = 12),
        axis.ticks = element_line(size = 0.0001))


ggsave("04_figures/plots/R13_MIB_events_aditionals.png", h = 13, w= 15)



# 7. Efectos por GRUPOS ----

## 7.1. Princiaples ----

### 7.1.1. Legalization ----
a1_leg_general[[3]] %>% 
  filter(outcome %in% outomes$outcome[c(1,10,12)]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_flip()+
  #scale_y_discrete(limits=rev)+
  geom_errorbar(width = 0.4,position=position_dodge(width = 0.6),lwd = 0.3)+
  geom_point(size = 1, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  labs(x ="", y = "")+
  scale_color_manual(values = c("black","grey25","grey45","grey65","lightpink2","brown3"))+
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 14, family = "serif"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(Variable~ .,scales = "free_x")

ggsave("04_figures/plots/R14_LEG_MAIN_group_effects.png", h = 5, w= 10)

### 7.1.2.MIB ----
a2_MIB_general[[3]] %>% 
  filter(outcome %in% outomes$outcome[c(1,10,12)]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_flip()+
  #scale_y_discrete(limits=rev)+
  geom_errorbar(width = 0.4,position=position_dodge(width = 0.6),lwd = 0.3)+
  geom_point(size = 1, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  labs(x ="", y = "")+
  scale_color_manual(values = c("black","grey25","grey45","grey65","lightpink2","brown3"))+
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 14, family = "serif"),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(Variable~ .,scales = "free_x")

ggsave("04_figures/plots/R15_MIB_MAIN_group_effects.png", h = 5, w= 10)


## 7.2. Complementarios ----


### 7.1.1. Legalization ----
a1_leg_general[[3]] %>% 
  filter(outcome %in% outomes$outcome[c(2:9,11,13)]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_flip()+
  #scale_y_discrete(limits=rev)+
  geom_errorbar(width = 0.2,position=position_dodge(width = 0.6),lwd = 0.2)+
  geom_point(size = 0.5, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2,lwd = 0.16, color = "black")+
  labs(x ="", y = "")+
  scale_color_manual(values = c("black","grey25","grey45","grey65","lightpink2","brown3"))+
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 7, family = "serif"),
        plot.title = element_blank(),
        strip.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(Variable~ .,scales = "free_x", nrow = 2)

ggsave("04_figures/plots/R19_LEG_COMPLEMENTARY_group_effects.png", h = 6, w= 8)

### 7.1.1. Legalization ----
a2_MIB_general[[3]] %>% 
  filter(outcome %in% outomes$outcome[c(2:9,11,13)]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_flip()+
  #scale_y_discrete(limits=rev)+
  geom_errorbar(width = 0.2,position=position_dodge(width = 0.6),lwd = 0.2)+
  geom_point(size = 0.5, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2,lwd = 0.16, color = "black")+
  labs(x ="", y = "")+
  scale_color_manual(values = c("black","grey25","grey45","grey65","lightpink2","brown3"))+
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 7, family = "serif"),
        strip.text = element_text(size = 8),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(Variable~ .,scales = "free_x", nrow = 2)

ggsave("04_figures/plots/R20_MIB_COMPLEMENTARY_group_effects.png", h = 6, w= 8)





















# 8.  Placebos ----


## 8.1. Cargar bases ----
p1_leg_placebos_gen <- readRDS("03_results/R01leg_Always_monitored_no_anticipation_placebos.RDS")
p2_MIB_placebos_gen <- readRDS("03_results/R02MIB_Always_monitored_no_anticipation_placebos.RDS")

## 8.2. Unir bases ----
base <-rbind(p1_leg_placebos_gen[[2]] %>% mutate(Program = "Legalization policy  "),
             p2_MIB_placebos_gen[[2]] %>% mutate(Program = "MIB program")) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  mutate(placebo = ifelse(placebo == "Standard model", "None",
                          str_sub(placebo,1,3)))


## 8.3. Gráficas ----

### 8.3.1. Illigal market ----
p <- base %>% 
  filter(outcome %in% outomes$outcome[1:5]) %>%
  ggplot(aes(placebo, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = Program, group = Program))+
  labs(x="",y="", 
       #title = "MIB program's placebo effects",
       color = "")+
  geom_errorbar(width = 0.2,position=position_dodge(width = 0.5), lwd =0.4)+
  geom_point(position=position_dodge(width = 0.5))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("cyan4","brown3"))+
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 16, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top")+
  facet_wrap(Variable~ ., scales="free")

p1 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[1:3])+
  theme(plot.title = element_blank())
p2 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[4:5]) +
  theme(plot.title = element_blank(), legend.position = "none")+
  labs(x = "Placebos")

p <- grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(9, "cm"),
  height = unit(4, "cm"))) 
p

ggsave("04_figures/plots/R16_Placebos_Iligal_markets.png", p,h = 6, w= 12)

### 8.3.2. Legal market ----
p <- base %>% 
  filter(outcome %in% outomes$outcome[c(6:8,10,12)]) %>%
  ggplot(aes(placebo, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = Program, group = Program))+
  labs(x="",y="", 
       #title = "MIB program's placebo effects",
       color = "")+
  geom_errorbar(width = 0.2,position=position_dodge(width = 0.5), lwd =0.4)+
  geom_point(position=position_dodge(width = 0.5))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("cyan4","brown3"))+
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 16, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top")+
  facet_wrap(Variable~ ., scales="free")

p1 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[c(6:8)])+
  theme(plot.title = element_blank())
p2 <- p %+% subset(base,
                   varibale.ordered %in% outomes$varibale.ordered[c(10,12)]) +
  theme(plot.title = element_blank(), legend.position = "none")+
  labs(x = "Placebos")

p <- grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(9, "cm"),
  height = unit(4, "cm"))) 
p

ggsave("04_figures/plots/R17_Placebos_Legal_markets.png", p,h = 6, w= 12)






base %>% 
  #filter(outcome %in% outomes$outcome[c(6:8,10,12)]) %>%
  ggplot(aes(placebo, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = Program, group = Program))+
  labs(x="",y="", 
       #title = "MIB program's placebo effects",
       color = "")+
  geom_errorbar(width = 0.2,position=position_dodge(width = 0.5), lwd =0.4)+
  geom_point(position=position_dodge(width = 0.5))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("cyan4","brown3"))+
  theme_minimal(base_line_size = 0, base_rect_size = 0)+
  theme(text = element_text(size = 16, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top")+
  facet_wrap(Variable~ ., scales="free")





