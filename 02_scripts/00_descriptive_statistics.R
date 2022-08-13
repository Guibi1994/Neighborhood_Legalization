# Estadísticas descriptivas generales
library(rgdal)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)
library(grid)


options(scipen = 100)

# 1.1. Cargar información geográfica ----

my_GBD = "00_geographic_data\\01_espacial_WGS1984.gdb"


## 1.1. Asentamientos inforamles ----
a00_informales <- data.frame(
  year = numeric(),
  coords.x1 = numeric(),
  coords.x2 = numeric())

for (i in seq(2005,2019)) {
  pr <- readOGR(my_GBD, 
                layer = paste0("Ocu_", i)) %>% 
    as.data.frame() %>% mutate(year = i) %>% 
    select(year, lon = coords.x1, lat = coords.x2)
  
  a00_informales <- rbind(a00_informales, pr)
  rm(pr)
}

g00_informales <- a00_informales %>% 
  mutate(ln = lon, lt = lat) %>% 
  st_as_sf(coords = c("ln","lt")) %>% 
  st_set_crs(4326)


## 1.2. Localidades ----
g01_localidades <- readOGR(my_GBD, "s01_localidades") %>% as("sf") %>% 
  st_set_crs(4326)



## 1.3. Barrios legalizados ----
g02_legalizacion <- readOGR(my_GBD, "s00_legelizaciones_prelegalizaciones") %>% 
  as("sf") %>% st_set_crs(4326) 

## 1.4. Buffers de legalización ----
g03_buffers <- readOGR(my_GBD, "s01_buffers_legalizaciones_simple") %>% 
  as("sf") %>% st_set_crs(4326)



## 1.5. Resentamientos IDGER ----
g04_reasentamientos <- readOGR(my_GBD, "s01_programa_resentamientos_IDGER") %>% 
  as("sf") %>% st_set_crs(4326)

a04_reasentamientos <- g04_reasentamientos %>% as.data.frame() %>% 
  mutate(id_year = substr(id,1,4),
         capture_year = substr(fecha_capt,1,4)) %>% 
  filter(estado_pro == "REASENTAMIENTO TERMINADO") %>% 
  select(estado_pro, localidad, amenaza,area, id,id_year,capture_year, soporte_ed, 
         fecha_ed, id_sire, year = aÃ.o, obs_reas, fecha_capt,fecha, 
         created_date, last_edited_date)
  





# 2. Transforamciones ----

## 2.1. Curse inforamles, bufffers, legalizaiones, localidades ----

b00_informales <- st_join(g00_informales,g01_localidades) %>% 
  st_join(g02_legalizacion) %>% 
  st_join(g03_buffers) %>% 
  as.data.frame() %>% 
  mutate(franjas = 
           case_when(!is.na(distance)~paste0(distance/1000, " km buffer"),
                     !is.na(Mi_codigo) & is.na(distance)~"Legalization polygones",
                     T ~ "Other areas")) %>% 
  mutate(franjas = factor(
    franjas,levels =  c("Other areas","Legalization polygones",
                        "0.25 km buffer","0.5 km buffer","0.75 km buffer",
                        "1 km buffer")),
    areas = ifelse(franjas == "Legalization polygones","Legalization polygones",
                   "Other areas"))



## 2.2. Resumen de legalizaciones y áreas legalizadas por año
b02_legalizacioens <- g02_legalizacion %>% 
  as.data.frame() %>% 
  group_by(year = Legalizacion) %>% 
  summarise(legalizaciones = n(),
            area_legalizda = sum(Ha_area, na.rm = T)) %>% 
  as.data.frame() %>% filter(!is.na(year)) %>% 
  # Acumulados 
  mutate(legalizaciones_cum = cumsum(legalizaciones),
         area_legalizda_cum = cumsum(area_legalizda)) %>% 
  filter(year >= 1975)



# 3. Reportes ----

## 3.1. PLOT: Iformality by area ----
b00_informales %>% 
  # Data
  filter(!is.na(Localidad)) %>% 
  group_by(year, areas) %>% 
  summarise(n = n()) %>% as.data.frame() %>% 
  group_by(areas) %>% 
  mutate(previus = lag(n, order_by = year),
         change = (n-previus)/previus
  ) %>% arrange(areas, year) %>% 
  
  # Plot
  ggplot(aes(year, n, group = areas)) +
  geom_path(aes(lty = areas, color = areas), lwd =0.5)+
  geom_point(aes(color = areas),size = 2) +
  
  scale_color_manual(values = c("cyan4","grey20"))+
  
  
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(y="Informal settlements", 
       title = "Bogota's informal settlements growth by areas")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))


ggsave("04_figures/plots/01_informal_settlements_by_area.png", h =6, w = 6)


## 3.2. PLOT: Iformality by buffer ----
b00_informales %>% 
  # Data
  filter(!is.na(Localidad)) %>% 
  group_by(year, franjas) %>% 
  summarise(n = n()) %>% as.data.frame() %>% 
  group_by(franjas) %>% 
  mutate(previus = lag(n, order_by = year),
         change = (n-previus)/previus
  ) %>% arrange(franjas, year) %>% 
  
  # Plot
  ggplot(aes(year, n, group = franjas)) +
  geom_path(aes(lty = franjas, color = franjas), lwd = .4) +
  geom_point(aes(color = franjas, shape = franjas), size = 1.5)+
  scale_shape_manual(values = c(16,16,0,1,2,4))+
  
  scale_linetype_manual(values = c(2,1,6,6,6,6))+
  scale_color_manual(values = c("black", "cyan4", "red4",
                                "red1","orangered","orange3"))+
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(y="Informal settlements", 
       title = "Bogota's informal settlements growth by buffers")+
  #geom_smooth()+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave("04_figures/plots/02_informal_settlements_by_buffer.png",h=6,w=6)


## 3.3. PLOT: Legalizations by year ----


c1 <- grobTree(
  textGrob("A--", x=0.425,  y=.97, hjust=0.5,rot = 0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 

c2 <- grobTree(
  textGrob("B--", x=0.614,  y=.97, hjust=0.5,rot =0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 

c3 <- grobTree(
  textGrob("--C", x=0.658,  y=.97, hjust=0.5,rot =0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 

c4 <- grobTree(
  textGrob("D--", x=0.718,  y=.97, hjust=0.5,rot =0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 


b02_legalizacioens %>% ggplot(aes(year, legalizaciones_cum))+
  geom_vline(xintercept = c(1994, 2003, 2004,2008, 2005,2012), 
             lty = c(2,2,2,2,3,3), 
             color = c("brown3","brown3","brown3","brown3","grey30","grey30"))+
  geom_path(color = "cyan4")+
  
  labs(title = "Cumulative number of neighborhood legalizations (1975-2019)",
       subtitle = "Bogotá, Colombia",
       y = "Number of closed processes")+
  
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n =6))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank())# +
  # annotation_custom(c1) +
  # annotation_custom(c2) +
  # annotation_custom(c3) +
  # annotation_custom(c4)


ggsave("04_figures/plots/03_Cumulative_legalizations_by_year.png",h=6,w=8)


### 3.4. 


c1 <- grobTree(
  textGrob("A--", x=0.425,  y=.97, hjust=0.5,rot = 0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 

c2 <- grobTree(
  textGrob("B--", x=0.614,  y=.97, hjust=0.5,rot =0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 

c3 <- grobTree(
  textGrob("--C", x=0.658,  y=.97, hjust=0.5,rot =0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 

c4 <- grobTree(
  textGrob("D--", x=0.718,  y=.97, hjust=0.5,rot =0,
           gp=gpar(col="red", fontsize=9, fontfamily = "serif"))) 


b02_legalizacioens %>% ggplot(aes(year, area_legalizda_cum))+
  geom_vline(xintercept = c(1994, 2003, 2004,2008, 2005,2012), 
             lty = c(2,2,2,2,3,3), 
             color = c("brown3","brown3","brown3","brown3","grey30","grey30"))+
  geom_path(color = "cyan4")+
  
  labs(title = "Cumulative number of neighborhood legalizations (1975-2019)",
       subtitle = "Bogotá, Colombia",
       y = "Number of closed processes")+
  
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n =6))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        legend.title = element_blank())# +
  # annotation_custom(c1) +
  # annotation_custom(c2) +
  # annotation_custom(c3) +
  # annotation_custom(c4)









