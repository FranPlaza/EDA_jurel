library(MASS) 
library(reshape2) 
library(reshape)
library(sf)
library(dplyr)
library(spData)
library(ggplot2) # tidyverse data visualization package
library(ggforce)
library(rgeos)
library(ggspatial)
library(tidyverse)
library(rgdal)
library(chilemapas)
library(patchwork)
library(colorspace)
library(RColorBrewer)
# library(gghighlight)
# library(akima)


pac<-c("readr","MASS","dplyr","lme4","Hmisc","gam")
lapply(pac, require, character.only = TRUE)
lapply(pac, library, character.only = TRUE)


df = db2020 %>% 
  mutate_at(colnames(db2020)[19:23], as.numeric) %>%
  filter(Especie == "Jurel") %>%
  dplyr::select(c("Longitud", "Latitud", colnames(db2020)[11:18])) %>%
  mutate(Sa_class = fct_rev(cut(Sa, breaks =c(0, 1, 500, 1000, 2000, Inf))), .after = Sa) %>% 
  mutate(Sa_class = fct_recode(Sa_class, 
                               ">2000" = "(2e+03,Inf]",
                               "1000-2000" = "(1e+03,2e+03]",
                               "500-1000" = "(500,1e+03]",
                               "1-500" = "(1,500]",
                               "0" = "(0,1]")) %>% 
  mutate_at(colnames(db2020)[12:18], cut_number, n=5) %>% 
  mutate_at(colnames(db2020)[12:18], fct_rev)

# Krigging of datapoints to set up interpolated maps - just as a test
# df_k = db2020 %>%
#   mutate_at(colnames(db2020)[19:23], as.numeric) %>%
#   # filter(Especie == "Jurel") %>%
#   dplyr::select(c("Longitud", "Latitud", colnames(db2020)[11:18]))
# fld = with(df_k, interp(x = Longitud, y = Latitud, z = Temperat, duplicate="median"))
# # prepare data in long format
# df_TSM <- melt(fld$z, na.rm = TRUE)
# names(df_TSM) <- c("x", "y", "TSM")
# df_TSM$Lon <- fld$x[df_TSM$x]
# df_TSM$Lat <- fld$y[df_TSM$y]

#From chilemapas package
cl <- dplyr::filter(chilemapas::mapa_comunas)
shp_chile = generar_regiones(cl)

#fortify prepares the data for ggplot
shp_chile <- fortify(shp_chile) # convert to data frame for ggplot

#Map for the study area adjacent countries
countries_x = c(-71, -68.1, -68.6)
countries_y = c(-16.5, -17.5, -31.5)
countries = c("Peru", "Bolivia", "Argentina")

# Map Plots ------------------------------------------------------------------------
p0 = 
  ggplot() +
  
  #World Map
  # geom_sf(data = world, colour = "grey") +
  labs( x = "Longitud", y = "Latitud") +
  
  #Add detailed map from Chile
  geom_sf(data = shp_chile, colour = "grey15", fill = "grey60", size = 0.7) + 
  coord_sf(xlim = c(-75.0, -69.00), ylim = c(-35.00, -17.00), expand = FALSE) +
  
  #Add adjacent countries and cities
  # annotate("text", x = countries_x, y = countries_y, label = countries, size = 3, alpha = 0.5, fontface = "bold") +
  geom_point(aes(x = -70.3975, y = -23.6509), shape = 21, colour="black", fill = "white", size = 2, stroke = 1) +
  annotate("text", x = -70.25, y = -23.6509, label = "Antofagasta", size = 3, alpha=1, hjust = 0) +
  geom_point(aes(x = -70.1357, y = -20.2307), shape = 21, colour="black", fill = "white", size = 2, stroke = 1) +
  annotate("text", x = -70.00, y = -20.2307, label = "Iquique", size = 3, alpha=1, hjust = 0) +
  geom_point(aes(x = -70.3126, y = -18.4783), shape = 21, colour="black", fill = "white", size = 2, stroke = 1) +
  annotate("text", x = -70.15, y = -18.4783, label = "Arica", size = 3, alpha=1, hjust = 0) +
  
  #Add north arrow and scale to map
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(height =unit(0.8, "cm"),
                         width = unit(0.8, "cm"),
                         location = "bl", which_north = "true", 
                         pad_x = unit(0.4, "cm"), pad_y = unit(0.6, "cm"),
                         style = north_arrow_fancy_orienteering) +
  scale_x_continuous(breaks = seq(-74, -68, 2)) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  theme_bw()
# p0

#Add aditional variables for creating plots ----


## Sa plot ----
p1 = p0 +
  geom_point(data = df, aes(x= Longitud, y= Latitud, colour = Sa_class), size = 3, alpha = 0.7) +
  scale_color_manual(values = c(">2000" = "red",
                                "1000-2000" = "orange",
                                "500-1000" = "gray80",
                                "1-500" = "gray90",
                                "0" = "gray100")) +
  theme(legend.position = c(0.01, 0.85), 
        legend.text=element_text(size=5),
        legend.title = element_text(size=5),
        legend.justification='left')
# p1


for (i in 2:8){
  aux = p0 + 
    geom_point(data = df, aes(x=Longitud, y=Latitud, colour = !! sym(colnames(df)[i+3])), size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "Spectral", #direction = -1,
                       name = colnames(df)[i+3], na.translate = F) +
    theme(legend.position = c(0.01, 0.85), 
          legend.text=element_text(size=5),
          legend.title = element_text(size=5),
          legend.justification='left',
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
    # scale_colour_gradient(low = "white", high = "red", na.value = NA, name = colnames(df)[i+3])
    # scale_fill_distiller(palette = "Spectral")
  assign(paste0("p", i), aux)
}

# plot_maps = (p1 | p2 | p3 | p4)/ (p5 | p6 | p7 | p8)
# plot = (p2 | p3 | p4 | p5 | p6 | p7 | p8)
# plot
# ggsave(plot, file = "spatial_maps_class.pdf", 
#        dpi = 300, width = 40, height = 40, units = "cm")