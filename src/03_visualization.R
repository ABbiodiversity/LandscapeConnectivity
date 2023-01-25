#
# Title: Visualization of landscape connectivity
# Created: October 11th, 2022
# Last Updated: November 3rd, 2022
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
load("results/tables/connectivity_HFI2018.RData")

# Standardize the data
connectivity <- data.frame(HUC_8 = rownames(results.list$UpCur),
                           UpCur = NA,
                           UpRef = NA,
                           UpArea = NA,
                           LowCur = NA,
                           LowRef = NA,
                           LowArea = NA,
                           GrCur = NA,
                           GrRef = NA,
                           GrArea = NA)

for (cover in names(results.list)) {
  
  # Subset data
  temp.data <- results.list[[cover]]
  
  # Fill in NA values
  temp.data$HUC_8[is.na(temp.data$HUC_8)] <- rownames(temp.data)[is.na(temp.data$HUC_8)]

  # Fill in values
  connectivity[, cover] <- temp.data$ECA
  
  # Pull out the reference area for each habitat
  if(cover %in% c("UpRef", "LowRef", "GrRef")) {
    
    connectivity[, gsub("Ref", "Area", cover)] <- temp.data[, "Habitat_Area"]
    
  }
  
  rm(temp.data)
  
}

# Correct the two watersheds that had single polygons under reference condiion
native.patches <- read_sf(dsn = "data/processed/huc-8/2018/gis/04020501.gdb",
                          layer = "UplandForest_Reference_viable")
native.patches <- sum(round(native.patches$TotalArea / 1000000, 3))

connectivity[connectivity$HUC_8 == "04020501", c("UpRef", "UpArea")] <- c(native.patches, native.patches)

native.patches <- read_sf("data/processed/huc-8/2018/gis/04010602.gdb",
                      layer = "Grassland_Reference_viable")
native.patches <- sum(round(native.patches$TotalArea / 1000000, 3))
connectivity[connectivity$HUC_8 == "04010602", c("GrRef", "GrArea")] <- c(native.patches, native.patches)
rm(cover, native.patches)


# NA values become 0.00001 to avoid INF division
connectivity[is.na(connectivity)] <- 0.00001

# Calculate the combined area-weighted index
connectivity$UpWeighted <- (connectivity$UpCur / connectivity$UpRef) * (connectivity$UpArea / rowSums(connectivity[, c("UpArea",
                                                                                                                          "LowArea", 
                                                                                                                          "GrArea")]))
connectivity$LowWeighted <- (connectivity$LowCur / connectivity$LowRef) * (connectivity$LowArea / rowSums(connectivity[, c("UpArea",
                                                                                                                          "LowArea", 
                                                                                                                          "GrArea")]))
connectivity$GrWeighted <- (connectivity$GrCur / connectivity$GrRef) * (connectivity$GrArea / rowSums(connectivity[, c("UpArea",
                                                                                                                          "LowArea", 
                                                                                                                          "GrArea")]))
connectivity$Connect <- rowSums(connectivity[, c("UpWeighted", "LowWeighted", "GrWeighted")]) * 100
connectivity$Connect[connectivity$Connect > 100] <- 100 # Masking values greater than 100 do to rounding error.

# Aspatial visualization

# Save results
png(paste0("results/figures/connectivity-HFI2018-histogram.png"),
    width = 1800,
    height = 1800, 
    res = 300)

ggplot(data = connectivity, aes(x = Connect, col = "#004f63", fill = "#004f63")) + 
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
shape.in <- merge(shape.in, connectivity, by = "HUC_8")

png(paste0("results/figures/connectivity-HFI2018.png"),
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
  
  png(paste0("results/figures/", i, " resistance.png"),
      width = 1800,
      height = 2400, 
      res = 300)
    
  print(ggplot() + 
    geom_sf(data = resist.in, aes_string(fill = name.id), show.legend = TRUE) +
    scale_fill_gradientn(name = paste0("Resistance"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), guide = "colourbar") +
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

