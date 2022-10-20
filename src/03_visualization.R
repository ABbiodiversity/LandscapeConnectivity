#
# Title: Visualization of landscape connectivity
# Created: October 11th, 2022
# Last Updated: October 11th, 2022
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
library(ggplot2)
library(MetBrewer)
library(sf)

# Load results
load("results/tables/connectivity_HFI2018_2022-09-27.RData")

# Standardize the data
connectivity <- data.frame(HUC_8 = rownames(results.list$Dist_250$UpCur),
                           UpCur = NA,
                           UpRef = NA,
                           UpArea = NA,
                           LowCur = NA,
                           LowRef = NA,
                           LowArea = NA,
                           GrCur = NA,
                           GrRef = NA,
                           GrArea = NA)

results.1ha <- list(Dist_250 = connectivity, 
                    Dist_Multi = connectivity)

for (disp in names(results.list)) {
        
        disp.temp <- results.list[[disp]]
        
        for (cover in names(disp.temp)) { 
                
                # Subset data
                temp.data <- disp.temp[[cover]]
                
                # Recalculate ECA
                temp.data$ECA <- (temp.data$ECA_Watershed / 100) * (temp.data$Watershed_Area)
                
                # Fill in NA values
                temp.data$HUC_8[is.na(temp.data$HUC_8)] <- rownames(temp.data)[is.na(temp.data$HUC_8)]
                temp.data$ECA[is.na(temp.data$ECA)] <- 0 # Area of 1 prevents NA
                temp.data$Watershed_Area[is.na(temp.data$Watershed_Area)] <- 0 # Prevents areas from counting
                temp.data$Habitat_Area[is.na(temp.data$Habitat_Area)] <- 0 # Prevents areas from counting
                temp.data <- temp.data[connectivity$HUC_8, ]
                
                # Fill in values
                results.1ha[[disp]][[cover]] <- temp.data$ECA
                
                # If reference, add the area
                if(cover %in% c("UpRef", "LowRef", "GrRef")) {
                        
                        results.1ha[[disp]][[gsub("Ref", "Area", cover)]] <- temp.data$Watershed_Area * (temp.data$Habitat_Area/100)
                        
                }
                
                rm(temp.data)
                
        }
        
        # Calculate scaled version
        results.1ha[[disp]]$UplndW <- (results.1ha[[disp]]$UpCur / results.1ha[[disp]]$UpRef) * (results.1ha[[disp]]$UpArea / rowSums(results.1ha[[disp]][, c("UpArea",
                                                                                                                           "LowArea", 
                                                                                                                           "GrArea")]))
        results.1ha[[disp]]$LwlndW <- (results.1ha[[disp]]$LowCur / results.1ha[[disp]]$LowRef) * (results.1ha[[disp]]$LowArea / rowSums(results.1ha[[disp]][, c("UpArea",
                                                                                                                              "LowArea", 
                                                                                                                              "GrArea")]))
        results.1ha[[disp]]$GrsslW <- (results.1ha[[disp]]$GrCur / results.1ha[[disp]]$GrRef) * (results.1ha[[disp]]$GrArea / rowSums(results.1ha[[disp]][, c("UpArea",
                                                                                                                           "LowArea", 
                                                                                                                           "GrArea")]))
        results.1ha[[disp]]$Connect <- rowSums(results.1ha[[disp]][, c("UplndW", "LwlndW", "GrsslW")], na.rm = TRUE) * 100
        
}

save(results.1ha, file = "results/tables/connectivity-250-vs-multi.Rdata")

# Calculate % HF

landcover.classes <- read.csv("data/lookup/landcover-classification_2021-11-17.csv")
connectivity$HFI <- NA

for (boundary.id in connectivity$HUC_8) {
        
        # Load the patch layer
        patch.native <- read_sf(dsn = paste0("data/processed/huc-", 8, "/", 2018, "/gis/", boundary.id, ".gdb"), 
                                layer = "current_landcover")
        
        connectivity[connectivity$HUC_8 %in% boundary.id, "HFI"] <- (sum(st_area(patch.native[!(patch.native$Current %in% unique(landcover.classes$Class)), ])) / sum(st_area(patch.native))) * 100
        print(boundary.id)
}

#
# Compare Overall ECA for the two runs
#

eca.compare <- data.frame(disp_multi = results.1ha$Dist_Multi$Connect,
                          disp_250 = results.1ha$Dist_250$Connect,
                          HFI = connectivity$HFI)

save(eca.compare, file = "results/tables/connectivity-250-vs-multi_2018.Rdata")

#
# Figures
#

rm(list=ls())
gc()

# load data
load("results/tables/connectivity-250-vs-multi_2018.Rdata")
load("results/tables/connectivity-250-vs-multi.Rdata")

# Compare the two dispersal distances
png(filename = "results/figures/multi-vs-250m-dispersal.png",
    height = 1600,
    width = 1600,
    res = 300)

ggplot() +
  geom_point(data = eca.compare, aes(x = disp_multi, y = disp_250)) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Connectivity (Multi)") +
  ylab("Connectivity (Single)") +
  theme_light()

dev.off()

png(filename = "results/figures/multi-footprint-connectivity.png",
    height = 1600,
    width = 1600,
    res = 300)

ggplot(data = eca.compare, aes(x = HFI, y = disp_multi)) +
  geom_point() +
  geom_smooth(method='loess') +
  ylim(0,100) +
  ylab("Connectivity (%)") +
  xlab("Footprint (%)") +
  theme_light()

dev.off()

png(filename = "results/figures/250-footprint-connectivity.png",
    height = 1600,
    width = 1600,
    res = 300)

ggplot(data = eca.compare, aes(x = HFI, y = disp_250)) +
  geom_point() +
  geom_smooth(method='loess') +
  ylim(0,100) +
  ylab("Connectivity (%)") +
  xlab("Footprint (%)") +
  theme_light()

dev.off()

# Boxplot
eca.results <- data.frame(Dispersal = c(rep("250", 422), rep("Multi", 422)),
                          Connectivity = c(eca.compare$disp_250, eca.compare$disp_multi))

png(filename = "results/figures/multi-vs-250m-dispersal-boxplot.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = eca.results) +
        geom_boxplot(aes(x = Dispersal, y = Connectivity)) +
        theme_light()
dev.off()

#
# Habitat comparison
#

# Grassland
habitat.comp <- data.frame(Dispersal = c(rep("250", 422), 
                                         rep("Multi", 422)),
                           Connectivity = c(((results.1ha$Dist_250$GrCur / results.1ha$Dist_250$GrRef) * 100), 
                                     ((results.1ha$Dist_Multi$GrCur / results.1ha$Dist_Multi$GrRef) * 100)),
                           Area = rep(c(results.1ha$Dist_250$GrArea / rowSums(results.1ha$Dist_250[, c("UpArea", "LowArea", "GrArea")], na.rm = TRUE)), 2))

habitat.comp <- habitat.comp[habitat.comp$Area > 0.01, ]

# Boxplots
png(filename = "results/figures/grassland-dispersal-comparison.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = habitat.comp) +
        geom_boxplot(aes(x = Dispersal, y = Connectivity)) +
        ggtitle("Grassland habitats") +
        theme_light()
dev.off()

# Upland
habitat.comp <- data.frame(Dispersal = c(rep("250", 422), 
                                         rep("Multi", 422)),
                           Connectivity = c(((results.1ha$Dist_250$UpCur / results.1ha$Dist_250$UpRef) * 100), 
                                            ((results.1ha$Dist_Multi$UpCur / results.1ha$Dist_Multi$UpRef) * 100)),
                           Area = rep(c(results.1ha$Dist_250$UpArea / rowSums(results.1ha$Dist_250[, c("UpArea", "LowArea", "GrArea")], na.rm = TRUE)), 2))

habitat.comp <- habitat.comp[habitat.comp$Area > 0.01, ]

# Boxplots
png(filename = "results/figures/upland-dispersal-comparison.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = habitat.comp) +
  geom_boxplot(aes(x = Dispersal, y = Connectivity)) +
  ggtitle("Upland habitats") +
  theme_light()
dev.off()


# Lowland
habitat.comp <- data.frame(Dispersal = c(rep("250", 422), 
                                         rep("Multi", 422)),
                           Connectivity = c(((results.1ha$Dist_250$LowCur / results.1ha$Dist_250$LowRef) * 100), 
                                            ((results.1ha$Dist_Multi$LowCur / results.1ha$Dist_Multi$LowRef) * 100)),
                           Area = rep(c(results.1ha$Dist_250$LowArea / rowSums(results.1ha$Dist_250[, c("UpArea", "LowArea", "GrArea")], na.rm = TRUE)), 2))

habitat.comp <- habitat.comp[habitat.comp$Area > 0.01, ]

# Boxplots
png(filename = "results/figures/lowland-dispersal-comparison.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = habitat.comp) +
  geom_boxplot(aes(x = Dispersal, y = Connectivity)) +
  ggtitle("Lowland habitats") +
  theme_light()
dev.off()

#
# Spatial Map
#

# Load results
shape.in <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
eca.compare <- results.1ha$Dist_250[, c("HUC_8", "Connect")]

# Fix the rounding error
eca.compare$Connect <- round(eca.compare$Connect)

shape.in <- merge(shape.in, eca.compare, by = "HUC_8")

# # Save results

png(file = paste0("results/figures/connectivity-1ha-250m-2018HFI.png"),
    width = 1800,
    height = 2400, 
    res = 300)
landscape.map <- ggplot() + 
        geom_sf(data = shape.in, aes(fill = Connect, colour = Connect)) +
        scale_fill_gradientn(name = "Connectivity", colours = c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                                                                "#e0f3f8","#abd9e9", "#74add1", "#4575b4", "#313695")) +
        scale_colour_gradientn(name = "Connectivity", colours = c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                                                                  "#e0f3f8","#abd9e9", "#74add1", "#4575b4", "#313695")) +
        theme(axis.text.y  = element_blank(),
              axis.text.x  = element_blank(),
              axis.title.y = element_text(size=10),
              axis.title.x = element_text(size=10),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))
print(landscape.map)

dev.off()
