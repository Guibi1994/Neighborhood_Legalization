# Graficas y tablas de resultados:


# 0. Pre-streps ----
## 0.1. Libraries ----
library(dplyr)
library(ggplot2)
library(kableExtra)
options(scipen = 100)

## 0.2. Function for pvalues ----
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

## 0.3. Lists of outcomes and main outcomes ----
outcomes <- read.csv("00_data/005_ouctome_names_corregidos.csv")
outcomes_main <- outcomes %>% filter(grupo != "Detalle")

## 0.4. Nota de pvalues y errores standar ----
pnotes <- "Standard errors in parentheses. '*' is significant at the 10% level, '**' is significant at the 5% level, '***' is significant at the 1% level"


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||






# 1. REsultados generales ----

## 1.1. Legalización ----
a1_leg_general <- readRDS("03_results/R01leg_Always_monitored_no_anticipation.RDS")


R1_leg <- a1_leg_general[[2]] %>% pval() %>% 
  select(2,15,8,9,10) %>% 
  reshape2::melt(id.vars = c("outcome","control_group","covariates")) %>%
  mutate(covariates = ifelse(covariates=="inconditional","(A)","(B)")) %>% 
  reshape2::dcast(outcome+variable~control_group+covariates, value.var = "value") %>% 
  filter(str_detect(outcome,"dummy")==F) %>% 
  merge(outcomes,by = "outcome",all.x=T) %>% 
  arrange(orden) %>% 
  select(15,everything()) %>% select(-2,-3,-16,-17)


names(R1_leg) <- c("Variable",str_extract(
  names(R1_leg)[2:ncol(R1_leg)],pattern = "\\_\\D{3}") %>% 
    str_remove(.,"_"))

R1_leg <- 
  rbind(R1_leg,
  data.frame(
  var1 = c("Variable",rep(c("(A)","(B)"),6)),
  var2  =  c("PCA controls",rep(c("No","Yes"),6))) %>% 
  t() %>% as.data.frame() %>% 
  janitor::row_to_names(1) %>%
  as.data.frame())

rownames(R1_leg) <-NULL

R1_leg %>% 
  kbl(caption = "Legalization general effects -always monitored sample",
      booktabs = T, align = c("l",sample("c",12,T)),
      format = "latex") %>% 
  kable_styling(font_size = 4) %>% 
  collapse_rows(1, latex_hline = "none") %>% 
  add_header_above(c("","1st neighbours"=2,"2nd neighbours"=2,"3rd neighbours"=2,
                     "4th neighbours"=2,"5th neighbours"=2,"All neighbours"=2),
                   italic = T) %>% 
  pack_rows("Informal Market",1,10) %>% 
  pack_rows("Formal Market - Overall",11,20,hline_before = T) %>%
  pack_rows("Formal Market - By land uses",21,68,hline_before = T) %>% 
  pack_rows(" ",69,69,hline_before = T) %>% 
  kableExtra::landscape() %>% 
  footnote(general = pnotes, general_title = "Notes: ", title_format = "bold",
           footnote_as_chunk = T)
  

#/////////////////////////////////////////////////////////////////////////////

## 1.2. MIB ----
am_2_resultados <- readRDS("03_results/R02MIB_Always_monitored_no_anticipation.RDS")
pr <- am_2_resultados[[2]] %>% pval() %>% 
  select(2,15,8,9,10) %>% 
  reshape2::melt(id.vars = c("outcome","control_group","covariates")) %>%
  mutate(covariates = ifelse(covariates=="inconditional","(A)","(B)")) %>% 
  reshape2::dcast(outcome+variable~control_group+covariates, value.var = "value") %>% 
  filter(str_detect(outcome,"dummy")==F) %>% 
  merge(read.csv("00_data/005_ouctome_names_corregidos.csv"),by = "outcome",all.x=T) %>% 
  arrange(orden) %>% 
  select(15,everything()) %>% select(-2,-3,-16,-17)


names(pr) <- c("Variable",str_extract(names(pr)[2:ncol(pr)],pattern = "\\_\\D{3}") %>% 
                 str_remove(.,"_"))

pr <- 
  rbind(pr,
        data.frame(
          pr = c("Variable",rep(c("(A)","(B)"),6)),
          var  =  c("PCA controls",rep(c("No","Yes"),6))) %>% 
          t() %>% as.data.frame() %>% 
          janitor::row_to_names(1) %>%
          as.data.frame())
rownames(pr) <-NULL

pr %>% 
  kbl(caption = "MIB general effects -always monitored sample",
      booktabs = T, align = c("l",sample("c",12,T)),
      format = "latex") %>% 
  kable_styling(font_size = 4) %>% 
  collapse_rows(1, latex_hline = "none") %>% 
  add_header_above(c("","1st neighbours"=2,"2nd neighbours"=2,"3rd neighbours"=2,
                     "4th neighbours"=2,"5th neighbours"=2,"All neighbours"=2),
                   italic = T) %>% 
  pack_rows("Informal Market",1,10) %>% 
  pack_rows("Formal Market - Overall",11,20,hline_before = T) %>%
  pack_rows("Formal Market - By land uses",21,68,hline_before = T) %>% 
  pack_rows(" ",69,69,hline_before = T) %>% 
  kableExtra::landscape()





  

