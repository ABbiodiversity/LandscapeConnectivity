#
# Title: Visualization of landscape connectivity
# Created: October 11th, 2022
# Last Updated: October 17th, 2022
# Author: Brandon Allen
# Objectives: Visualize the landscape connectivity indicator.
# Keywords: Notes, Visualization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
#
#################
# Visualization #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries and source functions
library(abmi.themes)
library(ggplot2)
library(MetBrewer)
library(sf)

# Load results
load("results/tables/connectivity_HFI2018_v7.RData")

# Aspatial visualization

# Save results
png(paste0("results/figures/connectivity-HFI2018-histogramv7.png"),
    width = 1800,
    height = 1800, 
    res = 300)

ggplot(data = results.connect, aes(x = Connect, col = "#004f63", fill = "#004f63")) + 
  geom_histogram(bins = 100, show.legend = FALSE) +
  scale_color_manual(values = "#004f63") +
  scale_fill_manual(values = "#004f63") +
  ggtitle(paste0("Landscape Connectivity (2018)")) + 
  xlab("Connectivity (%)") +
  ylab("Frequency") +
  theme_light() +
  theme_abmi(font = "Montserrat")

dev.off()

# Spatial visualization

shape.in <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
shape.in <- merge(shape.in, results.connect, by = "HUC_8")

png(paste0("results/figures/connectivity-HFI2018v7.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
  geom_sf(data = shape.in, aes(fill = Connect), show.legend = TRUE) +
  scale_fill_gradientn(name = paste0("Connectivity (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
  ggtitle("Landscape Connectivity (2018)") + 
  theme_light() +
  theme(axis.title = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        title = element_text(size=12), 
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.5, "cm"),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = c(0.19, 0.15)) 

dev.off()

#
# Resistance
#

resist.in <- read_sf("data/processed/huc-8/2018/movecost/huc-8-movecost_2018.shp")


# Need to create a resistance map for each habitat type and reference condition
habitat <- c("UpCur", "UpRef", "LowCur", "LowRef", "GrCur", "GrRef")
names(habitat) <- c("Upland Forest Current", "Upland Forest Reference", 
                    "Lowland Forest Current", "Lowland Forest Reference",
                    "Grassland Current", "Grassland Reference")

for (i in names(habitat)) {
  
  name.id <- as.character(habitat[i])
  
  png(paste0("results/figures/", i, " resistancev7.png"),
      width = 1800,
      height = 2400, 
      res = 300)
    
  print(ggplot() + 
    geom_sf(data = resist.in, aes_string(fill = name.id), show.legend = TRUE) +
    scale_fill_gradientn(name = paste0("Resistance"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,10), guide = "colourbar") +
    ggtitle(i) + 
    theme_light() +
    theme(axis.title = element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          title = element_text(size=12), 
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.19, 0.15)))
  
  dev.off()
  
}

