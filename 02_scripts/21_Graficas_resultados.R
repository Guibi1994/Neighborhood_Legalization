# Gráficas de resultados

library(dplyr)
library(ggplot2)


# Cargar bases ----

## Outcomes re-etiquetados ----
outomes <- read.csv("00_data/005_ouctome_names_corregidos.csv") %>% 
  mutate(varibale.ordered = paste0("(",letters[orden],"). ", Variable))

# Resutados generales  ----
a1_leg_general <- readRDS("03_results/R01leg_Always_monitored_no_anticipation.RDS")
a2_MIB_general <- readRDS("03_results/R02MIB_Always_monitored_no_anticipation.RDS")



### Legalization ----

a1_leg_general[[2]] %>% 
  filter(outcome %in% outomes$outcome[1:5]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "Legalization's effects on the housing ilegal market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.3))+
  geom_point(position=position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","cyan4"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")



a1_leg_general[[2]] %>% 
  filter(outcome %in% outomes$outcome[6:10]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "Legalization's effects on the housing legal market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.3))+
  geom_point(position=position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","cyan4"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 6, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")



## MIB 
a2_MIB_general[[2]] %>% 
  filter(outcome %in% outomes$outcome[1:5]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "MIB's effects on the housing ilegal market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.3))+
  geom_point(position=position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","cyan4"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")



a2_MIB_general[[2]] %>% 
  filter(outcome %in% outomes$outcome[6:10]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>% 
  ggplot(aes(control_group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "MIB's effects on the housing legal market",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.3))+
  geom_point(position=position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","cyan4"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 6, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")







#### # ## # ## # ## #  # 

# Por grupos 

a1_leg_general[[3]] %>% 
  filter(outcome %in% outomes$outcome[1:5]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(group, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  labs(title = "Legalization's effects on the legal market (by traetment groups)",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.6))+
  geom_point(size = .5, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("grey60","grey45","black","cyan4","orange","brown3"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")



# Por eventos

a1_leg_general[[4]] %>% 
  filter(outcome %in% outomes$outcome[1:5]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(event.time, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_cartesian(xlim = c(-4,8))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(title = "Legalization's effects on the legal market (by traetment groups)",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.6))+
  geom_point(size = .5, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("grey60","grey45","black","cyan4","orange","brown3"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")


a1_leg_general[[4]] %>% 
  filter(outcome %in% outomes$outcome[6:10]) %>% 
  merge(outomes, by = "outcome", all.x = T) %>%
  filter(covariates == "conditional") %>% 
  ggplot(aes(event.time, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = control_group, group = control_group))+
  coord_cartesian(xlim = c(-4,8))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(title = "Legalization's effects on the legal market (by traetment groups)",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.6))+
  geom_point(size = .5, position=position_dodge(width = 0.6))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("grey60","grey45","black","cyan4","orange","brown3"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 7, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")


### MIB ----

## Placebos ----

### Legalization ----
p1_leg_placebos_gen <- readRDS("03_results/R01leg_Always_monitored_no_anticipation_placebos.RDS")


### MIB ----
p2_MIB_placebos_gen <- readRDS("03_results/R02MIB_Always_monitored_no_anticipation_placebos.RDS")















# Placebos: generl effects ----

## Legalization ----
p1_leg_placebos_gen[[2]] %>% 
  merge(outomes, by = "outcome", all.x = T)%>% 
  filter(outcome %in% outomes$outcome[6:10]) %>%
  ggplot(aes(placebo, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "Legalization's placebo effects",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.3))+
  geom_point(position=position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","cyan4"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 6, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")



## MIB ----
p2_MIB_placebos_gen[[2]] %>% 
  merge(outomes, by = "outcome", all.x = T)%>% 
  filter(outcome %in% outomes$outcome[1:5]) %>%
  ggplot(aes(placebo, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = covariates, group = covariates))+
  labs(title = "MIB program's placebo effects",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.3))+
  geom_point(position=position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("brown3","cyan4"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 6, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")








p2_MIB_placebos_gen[[4]] %>% 
  merge(outomes, by = "outcome", all.x = T) %>% 
  filter(covariates == "conditional") %>% 
  ggplot(aes(event.time, estimate, ymin = estimate+(std.error*1.96), 
             ymax = estimate-(std.error*1.96),
             color = placebo, group = placebo))+
  labs(title = "MIB program's placebo effects",x="",y="", color = "")+
  geom_errorbar(width = 0,position=position_dodge(width = 0.6))+
  geom_point(size = .1,position=position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, lty =2, color = "black")+
  scale_color_manual(values = c("grey50","grey50","black","red"))+
  theme_minimal()+
  theme(text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(
          size = 6, angle = 45,hjust = 1))+
  facet_wrap(varibale.ordered~ ., scales="free")




