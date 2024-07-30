#
# Title: Visualization of landscape connectivity
# Created: October 11th, 2022
# Last Updated: July 30th, 2024
# Author: Brandon Allen
# Objectives: Visualize the landscape connectivity indicator.
# Keywords: Notes, Connectivity, Resistance, Forest recovery
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 
################
# Connectivity # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(sf)

# Source scripts
source("src/visualization_functions.R")

# Load the results
load("results/tables/connectivity_HFI2010.RData")
landscape.connecivity.2010 <- results.connect
load("results/tables/connectivity_HFI2018.RData")
landscape.connecivity.2018 <- results.connect
load("results/tables/connectivity_HFI2019.RData")
landscape.connecivity.2019 <- results.connect
load("results/tables/connectivity_HFI2020.RData")
landscape.connecivity.2020 <- results.connect
load("results/tables/connectivity_HFI2021.RData")
landscape.connecivity.2021 <- results.connect

rm(results.connect, results.current, results.reference)

# Load the blank shapefile
connectivity.results <- read_sf("data/processed/huc-8/2010/movecost/huc-8-movecost_2010.dbf")

# Calculate the dominant landcover type
connectivity.results$LandcoverDominant <- NA

for(huc in connectivity.results$HUC_8) {
  
  id <- names(which.max(landscape.connecivity.2021[landscape.connecivity.2021$HUC_8 == huc, 
                                                   c("UplandForestArea", "LowlandForestArea", "GrasslandArea")]))
  
  id <- gsub("Area", "", id)
  connectivity.results[connectivity.results$HUC_8 == huc, "LandcoverDominant"] <- id
  
}

# Align the rows
landscape.connecivity.2010 <- landscape.connecivity.2010[connectivity.results$HUC_8, ]
landscape.connecivity.2018 <- landscape.connecivity.2018[connectivity.results$HUC_8, ]
landscape.connecivity.2019 <- landscape.connecivity.2019[connectivity.results$HUC_8, ]
landscape.connecivity.2020 <- landscape.connecivity.2020[connectivity.results$HUC_8, ]
landscape.connecivity.2021 <- landscape.connecivity.2021[connectivity.results$HUC_8, ]

# Convert NA to 0
landscape.connecivity.2010[is.na(landscape.connecivity.2010)] <- 0

# Create the trend results
upland.trend <- data.frame(HUC_8 = landscape.connecivity.2010$HUC_8,
                           LC_2010 = (landscape.connecivity.2010$UplandForestCur / landscape.connecivity.2010$UplandForestRef) * 100,
                                LC_2021 = (landscape.connecivity.2021$UplandForestCur / landscape.connecivity.2021$UplandForestRef) * 100,
                           Area = (landscape.connecivity.2010$UplandForestArea / (landscape.connecivity.2010$UplandForestArea +
                                                                                    landscape.connecivity.2010$LowlandForestArea +
                                                                                    landscape.connecivity.2010$GrasslandArea)) * 100)
upland.trend$Difference <- upland.trend$LC_2021 - upland.trend$LC_2010

lowland.trend <- data.frame(HUC_8 = landscape.connecivity.2010$HUC_8,
                            LC_2010 = (landscape.connecivity.2010$LowlandForestCur / landscape.connecivity.2010$LowlandForestRef) * 100,
                            LC_2021 = (landscape.connecivity.2021$LowlandForestCur / landscape.connecivity.2021$LowlandForestRef) * 100,
                            Area = (landscape.connecivity.2010$LowlandForestArea / (landscape.connecivity.2010$UplandForestArea +
                                                                                     landscape.connecivity.2010$LowlandForestArea +
                                                                                     landscape.connecivity.2010$GrasslandArea)) * 100)
lowland.trend$Difference <- lowland.trend$LC_2021 - lowland.trend$LC_2010

grassland.trend <- data.frame(HUC_8 = landscape.connecivity.2010$HUC_8,
                              LC_2010 = (landscape.connecivity.2010$GrasslandCur / landscape.connecivity.2010$GrasslandRef) * 100,
                              LC_2021 = (landscape.connecivity.2021$GrasslandCur / landscape.connecivity.2021$GrasslandRef) * 100,
                              Area = (landscape.connecivity.2010$GrasslandArea / (landscape.connecivity.2010$UplandForestArea +
                                                                                       landscape.connecivity.2010$LowlandForestArea +
                                                                                       landscape.connecivity.2010$GrasslandArea)) * 100)
grassland.trend$Difference <- grassland.trend$LC_2021 - grassland.trend$LC_2010

total.trend <- data.frame(HUC_8 = landscape.connecivity.2010$HUC_8,
                          LC_2010 = landscape.connecivity.2010$Connect,
                          LC_2021 = landscape.connecivity.2021$Connect,
                          Difference = landscape.connecivity.2021$Connect - landscape.connecivity.2010$Connect)

# Append the differences
upland.trend <- merge(connectivity.results, upland.trend, by = "HUC_8")
lowland.trend <- merge(connectivity.results, lowland.trend, by = "HUC_8")
grassland.trend <- merge(connectivity.results, grassland.trend, by = "HUC_8")
total.trend <- merge(connectivity.results, total.trend, by = "HUC_8")

# If areas are 0 for the three habitat types, fix to NA
upland.trend$Area[upland.trend$Area == 0] <- NA
lowland.trend$Area[lowland.trend$Area == 0] <- NA
grassland.trend$Area[grassland.trend$Area == 0] <- NA

#########
# Total #
#########

# Append connectivity
connectivity.results$Connect2010 <- landscape.connecivity.2010$Connect 
connectivity.results$Connect2018 <- landscape.connecivity.2018$Connect 
connectivity.results$Connect2019 <- landscape.connecivity.2019$Connect 
connectivity.results$Connect2020 <- landscape.connecivity.2020$Connect 
connectivity.results$Connect2021 <- landscape.connecivity.2021$Connect 

connect.2010 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2010", 
                        title = "Connectivity 2010")

connect.2018 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2018", 
                        title = "Connectivity 2018")

connect.2019 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2019", 
                        title = "Connectivity 2019")

connect.2020 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2020", 
                        title = "Connectivity 2020")

connect.2021 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2021", 
                        title = "Connectivity 2021")
  
ggsave(filename = "results/figures/indicator/landscape-connectivity-appendix.png",
       plot = ggarrange(connect.2010, connect.2018,
                        connect.2019, connect.2020, connect.2021,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

connect.2010.2021 <- difference_plot(data.in = total.trend, 
                                     habitat = "Difference", 
                                     title = "")

ggsave(filename = "results/figures/indicator/landscape-connectivity-in-text.png",
       plot = ggarrange(connect.2010, connect.2021, connect.2010.2021,
                        ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = connectivity.results, 
                         x = "2010", 
                         y = "2021", 
                         title = "Landscape Connectivity")

ggsave(filename = "results/figures/support/landscape-connectivity-trend.png",
       plot = trend.plot,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

#################
# Upland Forest #
#################

# Append connectivity
connectivity.results$Connect2010 <- (landscape.connecivity.2010$UplandForestCur / landscape.connecivity.2010$UplandForestRef) * 100
connectivity.results$Connect2018 <- (landscape.connecivity.2018$UplandForestCur / landscape.connecivity.2018$UplandForestRef) * 100
connectivity.results$Connect2019 <- (landscape.connecivity.2019$UplandForestCur / landscape.connecivity.2019$UplandForestRef) * 100
connectivity.results$Connect2020 <- (landscape.connecivity.2020$UplandForestCur / landscape.connecivity.2020$UplandForestRef) * 100
connectivity.results$Connect2021 <- (landscape.connecivity.2021$UplandForestCur / landscape.connecivity.2021$UplandForestRef) * 100
connectivity.results$Area <- (landscape.connecivity.2010$UplandForestArea / (landscape.connecivity.2010$UplandForestArea +
                                                                                      landscape.connecivity.2010$LowlandForestArea +
                                                                                      landscape.connecivity.2010$GrasslandArea)) * 100

connect.2010 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2010", 
                        title = "Upland Forest Connectivity 2010")

connect.2018 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2018", 
                        title = "Upland Forest Connectivity 2018")

connect.2019 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2019", 
                        title = "Upland Forest Connectivity 2019")

connect.2020 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2020", 
                        title = "Upland Forest Connectivity 2020")

connect.2021 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2021", 
                        title = "Upland Forest Connectivity 2021")

ggsave(filename = "results/figures/indicator/upland-forest-connectivity-appendix.png",
       plot = ggarrange(connect.2010, connect.2018,
                        connect.2019, connect.2020, connect.2021,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

connect.2010.2021 <- difference_plot(data.in = upland.trend, 
                                     habitat = "Difference", 
                                     title = "")

upland.area <- area_plot(data.in = upland.trend, 
                          habitat = "Area", 
                          title = "")

# Define the outline of specific regions
subset <- connectivity.results[connectivity.results$HUC_8 %in% connectivity.results$HUC_8[connectivity.results$LandcoverDominant == "UplandForest"], ]
subset$Color <- "#000000"
subset$Fill <- "#000000"
connect.2010.2021 <- connect.2010.2021 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

connect.2010 <- connect.2010 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

connect.2021 <- connect.2021 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

upland.area <- upland.area + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

ggsave(filename = "results/figures/indicator/upland-forest-connectivity-in-text.png",
       plot = ggarrange(connect.2010, connect.2021, connect.2010.2021, ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = connectivity.results, 
                         x = "2010", 
                         y = "2021", 
                         title = "Upland Forest Connectivity")

ggsave(filename = "results/figures/support/upland-forest-connectivity-trend.png",
       plot = trend.plot,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

##################
# Lowland Forest #
##################

# Append connectivity
connectivity.results$Connect2010 <- (landscape.connecivity.2010$LowlandForestCur / landscape.connecivity.2010$LowlandForestRef) * 100
connectivity.results$Connect2018 <- (landscape.connecivity.2018$LowlandForestCur / landscape.connecivity.2018$LowlandForestRef) * 100
connectivity.results$Connect2019 <- (landscape.connecivity.2019$LowlandForestCur / landscape.connecivity.2019$LowlandForestRef) * 100
connectivity.results$Connect2020 <- (landscape.connecivity.2020$LowlandForestCur / landscape.connecivity.2020$LowlandForestRef) * 100
connectivity.results$Connect2021 <- (landscape.connecivity.2021$LowlandForestCur / landscape.connecivity.2021$LowlandForestRef) * 100
connectivity.results$Area <- (landscape.connecivity.2010$LowlandForestArea / (landscape.connecivity.2010$UplandForestArea +
                                                                               landscape.connecivity.2010$LowlandForestArea +
                                                                               landscape.connecivity.2010$GrasslandArea)) * 100

connect.2010 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2010", 
                        title = "Lowland Forest Connectivity 2010")

connect.2018 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2018", 
                        title = "Lowland Forest Connectivity 2018")

connect.2019 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2019", 
                        title = "Lowland Forest Connectivity 2019")

connect.2020 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2020", 
                        title = "Lowland Forest Connectivity 2020")

connect.2021 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2021", 
                        title = "Lowland Forest Connectivity 2021")

ggsave(filename = "results/figures/indicator/lowland-forest-connectivity-appendix.png",
       plot = ggarrange(connect.2010, connect.2018,
                        connect.2019, connect.2020, connect.2021,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

connect.2010.2021 <- difference_plot(data.in = lowland.trend, 
                                     habitat = "Difference", 
                                     title = "")

lowland.area <- area_plot(data.in = lowland.trend, 
                          habitat = "Area", 
                          title = "")


# Define the outline of specific regions
subset <- connectivity.results[connectivity.results$HUC_8 %in% connectivity.results$HUC_8[connectivity.results$LandcoverDominant == "LowlandForest"], ]
subset$Color <- "#000000"
subset$Fill <- "#000000"
connect.2010.2021 <- connect.2010.2021 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

connect.2021 <- connect.2021 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

connect.2010 <- connect.2010 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

lowland.area <- lowland.area + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

ggsave(filename = "results/figures/indicator//lowland-forest-connectivity-in-text.png",
       plot = ggarrange(connect.2010, connect.2021, connect.2010.2021, ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = connectivity.results, 
                         x = "2010", 
                         y = "2021", 
                         title = "Lowland Forest Connectivity")

ggsave(filename = "results/figures/support/lowland-forest-connectivity-trend.png",
       plot = trend.plot,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

##############
# Grasslands #
##############

# Append connectivity
connectivity.results$Connect2010 <- (landscape.connecivity.2010$GrasslandCur / landscape.connecivity.2010$GrasslandRef) * 100
connectivity.results$Connect2018 <- (landscape.connecivity.2018$GrasslandCur / landscape.connecivity.2018$GrasslandRef) * 100
connectivity.results$Connect2019 <- (landscape.connecivity.2019$GrasslandCur / landscape.connecivity.2019$GrasslandRef) * 100
connectivity.results$Connect2020 <- (landscape.connecivity.2020$GrasslandCur / landscape.connecivity.2020$GrasslandRef) * 100
connectivity.results$Connect2021 <- (landscape.connecivity.2021$GrasslandCur / landscape.connecivity.2021$GrasslandRef) * 100
connectivity.results$Area <- (landscape.connecivity.2010$GrasslandArea / (landscape.connecivity.2010$UplandForestArea +
                                                                               landscape.connecivity.2010$LowlandForestArea +
                                                                               landscape.connecivity.2010$GrasslandArea)) * 100


connect.2010 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2010", 
                        title = "Grass-Shrub Connectivity 2010")

connect.2018 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2018", 
                        title = "Grass-Shrub Connectivity 2018")

connect.2019 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2019", 
                        title = "Grass-Shrub Connectivity 2019")

connect.2020 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2020", 
                        title = "Grass-Shrub Connectivity 2020")

connect.2021 <- lc_plot(data.in = connectivity.results, 
                        habitat = "Connect2021", 
                        title = "Grass-Shrub Connectivity 2021")

ggsave(filename = "results/figures/indicator/grassland-connectivity-appendix.png",
       plot = ggarrange(connect.2010, connect.2018,
                        connect.2019, connect.2020, connect.2021,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

connect.2010.2021 <- difference_plot(data.in = grassland.trend, 
                                     habitat = "Difference", 
                                     title = "")

grass.area <- area_plot(data.in = grassland.trend, 
                          habitat = "Area", 
                          title = "")


# Define the outline of specific regions
subset <- connectivity.results[connectivity.results$HUC_8 %in% connectivity.results$HUC_8[connectivity.results$LandcoverDominant == "Grassland"], ]
subset$Color <- "#000000"
subset$Fill <- "#000000"
connect.2010.2021 <- connect.2010.2021 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

connect.2010 <- connect.2010 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

connect.2021 <- connect.2021 + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

grass.area <- grass.area + 
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = subset, aes(color = Color, fill = Fill), linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values =  alpha(subset$Color, 0.0), guide = "none") +
  scale_color_manual(values =  alpha(subset$Color, 1), guide = "none")

ggsave(filename = "results/figures/indicator/grassland-connectivity-in-text.png",
       plot = ggarrange(connect.2010, connect.2021, connect.2010.2021, ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = connectivity.results, 
                         x = "2010", 
                         y = "2021", 
                         title = "Grass-Shrub Connectivity")

ggsave(filename = "results/figures/support/grassland-connectivity-trend.png",
       plot = trend.plot,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

#############
# Area plot #
#############

ggsave(filename = "results/figures/indicator/habitat-area.png",
       plot = ggarrange(upland.area, lowland.area, grass.area, ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")


##############
# Resistance # 
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(sf)

# Source scripts
source("src/visualization_functions.R")

# Load the blank shapefile
resistance.2021 <- read_sf("data/processed/huc-8/2021/movecost/huc-8-movecost_2021.dbf")

######################
# Current Resistance #
######################

resist.grassland <- resistance_plot(data.in = resistance.2021, 
                                    habitat = "GrCur", 
                                    legend = "Cost Distance",
                                    title = "Grass-Shrub Cost Distance 2021")

resist.upland <- resistance_plot(data.in = resistance.2021, 
                                    habitat = "UpCur", 
                                    legend = "Cost Distance",
                                    title = "Upland Forest Cost Distance 2021")

resist.lowland <- resistance_plot(data.in = resistance.2021, 
                                    habitat = "LowCur", 
                                    legend = "Cost Distance",
                                    title = "Lowland Forest Cost Distance 2021")

ggsave(filename = "results/figures/support/resistance-current-2021.png",
       plot = ggarrange(resist.grassland, resist.upland,
                        resist.lowland,
                        ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")

########################
# Reference Resistance #
########################

resist.grassland <- resistance_plot(data.in = resistance.2021, 
                                    habitat = "GrRef", 
                                    legend = "Cost Distance",
                                    title = "Grass-Shrub Cost Distance Reference")

resist.upland <- resistance_plot(data.in = resistance.2021, 
                                 habitat = "UpRef", 
                                 legend = "Cost Distance",
                                 title = "Upland Forest Cost Distance Reference")

resist.lowland <- resistance_plot(data.in = resistance.2021, 
                                  habitat = "LowRef", 
                                  legend = "Cost Distance",
                                  title = "Lowland Forest Cost Distance Reference")

ggsave(filename = "results/figures/support/resistance-reference-2021.png",
       plot = ggarrange(resist.grassland, resist.upland,
                        resist.lowland,
                        ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")

##################
# Percent change #
##################

resistance.2021$Grassland <- ifelse(resistance.2021$GrCur > resistance.2021$GrRef,
                                    100 * ((resistance.2021$GrCur - resistance.2021$GrRef) / resistance.2021$GrRef),
                                    -100 * ((resistance.2021$GrRef - resistance.2021$GrCur) / resistance.2021$GrRef))

resistance.2021$Upland <- ifelse(resistance.2021$UpCur > resistance.2021$UpRef,
                                 100 * ((resistance.2021$UpCur - resistance.2021$UpRef) / resistance.2021$UpRef),
                                 -100 * ((resistance.2021$UpRef - resistance.2021$UpCur) / resistance.2021$UpRef))

resistance.2021$Lowland <- ifelse(resistance.2021$LowCur > resistance.2021$LowRef,
                                  100 * ((resistance.2021$LowCur - resistance.2021$LowRef) / resistance.2021$LowRef),
                                  -100 * ((resistance.2021$LowRef - resistance.2021$LowCur) / resistance.2021$LowRef))

resist.grassland <- resistance_plot(data.in = resistance.2021, 
                                habitat = "Grassland", 
                                legend = "Percent Change (%)",
                                title = "Grass-Shrub Cost Distance")

resist.upland <- resistance_plot(data.in = resistance.2021, 
                                habitat = "Upland", 
                                legend = "Percent Change (%)",
                                title = "Upland Forest Cost Distance")

resist.lowland <- resistance_plot(data.in = resistance.2021, 
                                habitat = "Lowland", 
                                legend = "Percent Change (%)",
                                title = "Lowland Forest Cost Distance")

ggsave(filename = "results/figures/support/resistance-change-2021.png",
       plot = ggarrange(resist.grassland, resist.upland,
                        resist.lowland,
                        ncol = 3, nrow = 1),
       height = 1200,
       width = 2700,
       dpi = 100,
       units = "px")

###################
# Forest Recovery # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(MetBrewer)

# Load data
load("data/lookup/harvest-recovery-curves_80years.Rdata")

# Standardize the data
harvest.recovery <- data.frame(Stand = c(rep("Deciduous", 81), 
                                         rep("Coniferous", 81)),
                               Age = c(recovery.curve$Age, 
                                       recovery.curve$Age),
                               Recovery = c(recovery.curve$Deciduous,
                                            recovery.curve$Coniferous))

# We are removing the 80 year class as looks weird visually. Can simply say "After 80 years, recovery is assumed 100%".
harvest.recovery <- harvest.recovery[harvest.recovery$Age != 80, ]

recovery.curve <- ggplot(data = harvest.recovery, aes(x = Age, y = Recovery, fill = Stand, col = Stand)) + 
  geom_point() +
  scale_color_manual(name = "Stand Type",values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
  scale_fill_manual(name = "Stand Type", values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
  xlab("Age (Years)") +
  ylab("Stand Recovery (%)") +
  ylim(c(0,100)) +
  xlim(c(0,80)) +
  theme_light() +
  theme(axis.title = element_text(size=16, face = "bold"),
        title = element_text(size=16, face = "bold"),
        axis.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16), 
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


ggsave(filename = "results/figures/support/forest-recovery-curves.png",
       plot = recovery.curve,
       height = 800,
       width = 1200,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()