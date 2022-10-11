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
load(paste0("results/tables/connectivity_HFI2018_2022-09-27.RData"))

# Standardize the data
connectivity <- data.frame(HUC_8 = rownames(results.list$UplndCR),
                           UplndCR = NA,
                           UplndCC = NA,
                           UplndArea = NA,
                           LwlndCR = NA,
                           LwlndCC = NA,
                           LwlndArea = NA,
                           GrsslCR = NA,
                           GrsslCC = NA,
                           GrsslArea = NA)

for (cover in names(results.list)) {
        
        # Subset data
        temp.data <- results.list[[cover]]
        
        # Fill in NA values
        temp.data$HUC_8[is.na(temp.data$HUC_8)] <- rownames(temp.data)[is.na(temp.data$HUC_8)]
        temp.data$ECA[is.na(temp.data$ECA)] <- 1 # Area of 1 prevents NA
        temp.data$Total_Area[is.na(temp.data$Total_Area)] <- 0 # Prevents areas from counting
        temp.data$Native_Cover[is.na(temp.data$Native_Cover)] <- 0 # Prevents areas from counting
        temp.data <- temp.data[connectivity$HUC_8, ]
        
        # Fill in values
        connectivity[, cover] <- temp.data$ECA

        # If reference, add the area
        if(cover %in% c("UplndCR", "LwlndCR", "GrsslCR")) {
                
                connectivity[, gsub("CR", "Area", cover)] <- temp.data$Total_Area * (temp.data$Native_Cover/100)
                
        }
        
        rm(temp.data)
}

# Calculate scaled version
connectivity$UplndW <- (connectivity$UplndCC / connectivity$UplndCR) * (connectivity$UplndArea / rowSums(connectivity[, c("UplndArea",
                                                                                                                          "LwlndArea", 
                                                                                                                          "GrsslArea")]))
connectivity$LwlndW <- (connectivity$LwlndCC / connectivity$LwlndCR) * (connectivity$LwlndArea / rowSums(connectivity[, c("UplndArea",
                                                                                                                          "LwlndArea", 
                                                                                                                          "GrsslArea")]))
connectivity$GrsslW <- (connectivity$GrsslCC / connectivity$GrsslCR) * (connectivity$GrsslArea / rowSums(connectivity[, c("UplndArea",
                                                                                                                          "LwlndArea", 
                                                                                                                          "GrsslArea")]))
connectivity$Connect <- rowSums(connectivity[, c("UplndW", "LwlndW", "GrsslW")]) * 100


# Calculate % HF

landcover.classes <- read.csv("data/lookup/landcover-classification_2021-11-17.csv")
connectivity$HFI <- NA

for (boundary.id in connectivity$HUC_8) {
        
        # Load the patch layer
        patch.native <- read_sf(paste0("data/processed/huc-", 8, "/", 2018, "/gis/", boundary.id, "/", boundary.id, "_current_landcover.shp"))
        
        connectivity[connectivity$HUC_8 %in% boundary.id, "HFI"] <- (sum(st_area(patch.native[!(patch.native$FEATURE_TY %in% unique(landcover.classes$Class)), ])) / sum(st_area(patch.native))) * 100
        print(boundary.id)
}


shape.in <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
shape.in <- merge(shape.in, connectivity, by = "HUC_8")

# Save results
write_sf(shape.in, dsn = "connect_2018_v2.shp")
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



plot(connectivity$HFI ~ connectivity$Connect)

png(file = "test-connect.png",
    height = 1600,
    width = 1600,
    res = 300)
print(landscape.map)

dev.off()

ggplot(data = connectivity, aes(x = HFI, y = Connect)) +
        geom_point() +
        geom_smooth()

connectivity$Native <- 100 - connectivity$HFI

ggplot(data = connectivity, aes(y = Native, x = Connect)) +
        geom_point() +
        geom_smooth()



sf.in <- read_sf("data/processed/huc-8/2018/movecost/huc-8-movecost_2018_2022-02-10.shp")

table(sf.in$UplndCR <= sf.in$UplndCC)
table(sf.in$LwlndCR <= sf.in$LwlndCC)
table(sf.in$GrsslCR <= sf.in$GrsslCC)




for(boundary.id in watershed.ids) {
        
        # Remove files that were created
        arcpy$Delete_management(in_data = paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", boundary.id, "/", boundary.id, "_", native.type, "_current_cleaned.shp"))
        arcpy$Delete_management(in_data = paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", boundary.id, "/", boundary.id, "_", native.type, "_current_predissolve.shp"))
        arcpy$Delete_management(in_data = paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", boundary.id, "/distmatrix.dbf"))
        
        
}

#################
# Visualization # 2.0 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
# 1 ha, 100/250m
#

# Clear memory
rm(list=ls())
gc()

# Load results
load("results/tables/landscape-connectivity-1haMVP-250mDisp_HFI2018_2022-06-14.Rdata")

# Standardize the data
connectivity <- data.frame(HUC_8 = rownames(results.list$Dist_100$UpCur),
                           UpCur = NA,
                           UpRef = NA,
                           UpArea = NA,
                           LowCur = NA,
                           LowRef = NA,
                           LowArea = NA,
                           GrCur = NA,
                           GrRef = NA,
                           GrArea = NA)

results.1ha <- list(Dist_100 = connectivity, 
                    Dist_250 = connectivity)

for (disp in names(results.list)) {
        
        disp.temp <- results.list[[disp]]
        
        for (cover in names(disp.temp)) { 
                
                # Subset data
                temp.data <- disp.temp[[cover]]
                
                # Recalculate ECA
                temp.data$ECA <- (temp.data$ECA_Watershed / 100) * (temp.data$Total_Area)
                
                # Fill in NA values
                temp.data$HUC_8[is.na(temp.data$HUC_8)] <- rownames(temp.data)[is.na(temp.data$HUC_8)]
                temp.data$ECA[is.na(temp.data$ECA)] <- 0 # Area of 1 prevents NA
                temp.data$Total_Area[is.na(temp.data$Total_Area)] <- 0 # Prevents areas from counting
                temp.data$Native_Cover[is.na(temp.data$Native_Cover)] <- 0 # Prevents areas from counting
                temp.data <- temp.data[connectivity$HUC_8, ]
                
                # Fill in values
                results.1ha[[disp]][[cover]] <- temp.data$ECA
                
                # If reference, add the area
                if(cover %in% c("UpRef", "LowRef", "GrRef")) {
                        
                        results.1ha[[disp]][[gsub("Ref", "Area", cover)]] <- temp.data$Total_Area * (temp.data$Native_Cover/100)
                        
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

save(results.1ha, file = "1ha.results.Rdata")

# Calculate % HF

landcover.classes <- read.csv("data/lookup/landcover-classification_2021-11-17.csv")
connectivity$HFI <- NA

for (boundary.id in connectivity$HUC_8) {
        
        # Load the patch layer
        patch.native <- read_sf(paste0("data/processed/huc-", 8, "/", 2018, "/gis/", boundary.id, "/", boundary.id, "_current_landcover.shp"))
        
        connectivity[connectivity$HUC_8 %in% boundary.id, "HFI"] <- (sum(st_area(patch.native[!(patch.native$FEATURE_TY %in% unique(landcover.classes$Class)), ])) / sum(st_area(patch.native))) * 100
        print(boundary.id)
}

#
# Compare Overall ECA for the two runs
#

eca.compare <- data.frame(disp_100 = results.1ha$Dist_100$Connect,
                          disp_250 = results.1ha$Dist_250$Connect,
                          HFI = connectivity$HFI)
ggplot() +
        geom_point(data = eca.compare, aes(x = disp_100, y = disp_250)) +
        geom_abline(slope = 1, intercept = 0)

ggplot(data = eca.compare, aes(x = HFI, y = disp_100)) +
        geom_point() +
        geom_smooth(method='loess') +
        ylim(0,100)

ggplot(data = eca.compare, aes(x = HFI, y = disp_250)) +
        geom_point() +
        geom_smooth(method='loess') +
        ylim(0,100)

hist(eca.compare$disp_100, ylim = c(0,200))
hist(eca.compare$disp_250, ylim = c(0,200))

save(eca.compare, file = "connectivity-1ha-100-250disp.R")


#
# QS 100, 250 disp
#

# Clear memory
rm(list=ls())
gc()

# Load results
load("results/tables/landscape-connectivity-1qsMVP-250mDisp_HFI2018_2022-06-15.Rdata")

# Standardize the data
connectivity <- data.frame(HUC_8 = rownames(results.list$Dist_100$UpCur),
                           UpCur = NA,
                           UpRef = NA,
                           UpArea = NA,
                           LowCur = NA,
                           LowRef = NA,
                           LowArea = NA,
                           GrCur = NA,
                           GrRef = NA,
                           GrArea = NA)

results.1ha <- list(Dist_100 = connectivity, 
                    Dist_250 = connectivity)

for (disp in names(results.list)) {
        
        disp.temp <- results.list[[disp]]
        
        for (cover in names(disp.temp)) { 
                
                # Subset data
                temp.data <- disp.temp[[cover]]
                
                # Recalculate ECA
                temp.data$ECA <- (temp.data$ECA_Watershed / 100) * (temp.data$Total_Area)
                
                # Fill in NA values
                temp.data$HUC_8[is.na(temp.data$HUC_8)] <- rownames(temp.data)[is.na(temp.data$HUC_8)]
                temp.data$ECA[is.na(temp.data$ECA)] <- 0 # Area of 1 prevents NA
                temp.data$Total_Area[is.na(temp.data$Total_Area)] <- 0 # Prevents areas from counting
                temp.data$Native_Cover[is.na(temp.data$Native_Cover)] <- 0 # Prevents areas from counting
                temp.data <- temp.data[connectivity$HUC_8, ]
                
                # Fill in values
                results.1ha[[disp]][[cover]] <- temp.data$ECA
                
                # If reference, add the area
                if(cover %in% c("UpRef", "LowRef", "GrRef")) {
                        
                        results.1ha[[disp]][[gsub("Ref", "Area", cover)]] <- temp.data$Total_Area * (temp.data$Native_Cover/100)
                        
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
results.1qs <- results.1ha
save(results.1qs, file = "1qs.results.Rdata")

# Calculate % HF

landcover.classes <- read.csv("data/lookup/landcover-classification_2021-11-17.csv")
connectivity$HFI <- NA

for (boundary.id in connectivity$HUC_8) {
        
        # Load the patch layer
        patch.native <- read_sf(paste0("data/processed/huc-", 8, "/", 2018, "/gis/", boundary.id, "/", boundary.id, "_current_landcover.shp"))
        
        connectivity[connectivity$HUC_8 %in% boundary.id, "HFI"] <- (sum(st_area(patch.native[!(patch.native$FEATURE_TY %in% unique(landcover.classes$Class)), ])) / sum(st_area(patch.native))) * 100
        print(boundary.id)
}

#
# Compare Overall ECA for the two runs
#

eca.compare <- data.frame(disp_100 = results.1ha$Dist_100$Connect,
                          disp_250 = results.1ha$Dist_250$Connect,
                          HFI = connectivity$HFI)
ggplot() +
        geom_point(data = eca.compare, aes(x = disp_100, y = disp_250)) +
        geom_abline(slope = 1, intercept = 0)

ggplot(data = eca.compare, aes(x = HFI, y = disp_100)) +
        geom_point() +
        geom_smooth(method='loess') +
        ylim(0,100)

ggplot(data = eca.compare, aes(x = HFI, y = disp_250)) +
        geom_point() +
        geom_smooth(method='loess') +
        ylim(0,100)

hist(eca.compare$disp_100, ylim = c(0,200))
hist(eca.compare$disp_250, ylim = c(0,200))

save(eca.compare, file = "connectivity-1qs-100-250disp.R")

# 
# BMF Assessment
#

# Clear memory
rm(list=ls())
gc()

library(ggplot2)
library(ggpubr)

# Load results
load("connectivity-1qs-100-250disp.R")

qs.results <- data.frame(Scenario = c(rep("QS_100", nrow(eca.compare)),
                                       rep("QS_250", nrow(eca.compare))),
                          Index = c(eca.compare$disp_100, 
                                    eca.compare$disp_250))

load("connectivity-1ha-100-250disp.R")

ha.results <- data.frame(Scenario = c(rep("HA_100", nrow(eca.compare)),
                                      rep("HA_250", nrow(eca.compare))),
                         Index = c(eca.compare$disp_100, 
                                   eca.compare$disp_250))

eca.results <- rbind(qs.results, ha.results)
rm(qs.results, ha.results)

eca.results$Scenario <- factor(eca.results$Scenario, levels = c("QS_100", "QS_250", "HA_100", "HA_250"))

# Boxplots
png(filename = "disp-mvp-boxplot_2022-07-13.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = eca.results) +
        geom_boxplot(aes(x = Scenario, y = Index)) +
        theme_light()
dev.off()

# Histograms
png(filename = "disp-mvp-histogram_2022-07-13.png",
    height = 1200,
    width = 1600,
    res = 300)

ha.100 <- eca.results[eca.results$Scenario == "HA_100", ]
ha.100 <- ggplot(data = ha.100) +
        geom_histogram(aes(x = Index, col = Scenario, fill = Scenario), alpha=0.5, position="identity", bins = 50, show.legend = FALSE) +
        ggtitle("1ha, Dispersal 100m") +
        ylim(c(0,100)) +
        theme_light()

ha.250 <- eca.results[eca.results$Scenario == "HA_250", ]
ha.250 <- ggplot(data = ha.250) +
        geom_histogram(aes(x = Index, col = Scenario, fill = Scenario), alpha=0.5, position="identity", bins = 50, show.legend = FALSE) +
        ggtitle("1ha, Dispersal 250m") +
        ylim(c(0,100)) +
        theme_light()

qs.100 <- eca.results[eca.results$Scenario == "QS_100", ]
qs.100 <- ggplot(data = qs.100) +
        geom_histogram(aes(x = Index, col = Scenario, fill = Scenario), alpha=0.5, position="identity", bins = 50, show.legend = FALSE) +
        ggtitle("1QS, Dispersal 100m") +
        ylim(c(0,100)) +
        theme_light()

qs.250 <- eca.results[eca.results$Scenario == "QS_250", ]
qs.250 <- ggplot(data = qs.250) +
        geom_histogram(aes(x = Index, col = Scenario, fill = Scenario), alpha=0.5, position="identity", bins = 50, show.legend = FALSE) +
        ggtitle("1QS, Dispersal 250m") +
        ylim(c(0,100)) +
        theme_light()

ggarrange(ha.100, ha.250, qs.100, qs.250, ncol = 2, nrow = 2)

dev.off()

# Scenario comparison
# Clear memory
rm(list=ls())
gc()

library(ggplot2)
library(ggpubr)

# Load results
load("connectivity-1qs-100-250disp.R")

qs.results <- eca.compare
colnames(qs.results)[1:2] <-c("disp_100_qs", "disp_250_qs")

load("connectivity-1ha-100-250disp.R")

ha.results <- eca.compare
colnames(ha.results)[1:2] <-c("disp_100_ha", "disp_250_ha")

eca.results <- cbind(qs.results, ha.results)
rm(qs.results, ha.results)

eca.results <- data.frame(Parameter = c(rep("Dispersal", nrow(eca.results) * 2), rep("Patch", nrow(eca.results) * 2)),
                          Difference = c((eca.results$disp_100_qs - eca.results$disp_250_qs), 
                                         (eca.results$disp_100_ha - eca.results$disp_250_ha),
                                         (eca.results$disp_250_qs - eca.results$disp_250_ha),
                                         (eca.results$disp_100_qs - eca.results$disp_100_ha)))

png(filename = "disp-mvp-difference-boxplot_2022-07-13.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = eca.results) +
        geom_boxplot(aes(x = Parameter, y = Difference)) +
        theme_light()
dev.off()

#
# Impact across habitat
#
load("1ha.results.Rdata")
load("1qs.results.Rdata")

# Grassland
habitat.comp <- data.frame(Scenario = c(rep("HA_100", nrow(results.1ha$Dist_100)),
                                        rep("HA_250", nrow(results.1ha$Dist_100)),
                                        rep("QS_100", nrow(results.1ha$Dist_100)),
                                        rep("QS_250", nrow(results.1ha$Dist_100))),
                           Index = c(((results.1ha$Dist_100$GrCur / results.1ha$Dist_100$GrRef) * 100), 
                                     ((results.1ha$Dist_250$GrCur / results.1ha$Dist_250$GrRef) * 100),
                                     ((results.1qs$Dist_100$GrCur / results.1qs$Dist_100$GrRef) * 100),
                                     ((results.1qs$Dist_250$GrCur / results.1qs$Dist_250$GrRef) * 100)),
                           Area = rep(c(results.1ha$Dist_100$GrArea / rowSums(results.1ha$Dist_100[, c("UpArea", "LowArea", "GrArea")], na.rm = TRUE)), 4))

habitat.comp <- habitat.comp[habitat.comp$Area > 0.01, ]
# Boxplots
png(filename = "disp-mvp-grassland-boxplot_2022-07-13.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = habitat.comp) +
        geom_boxplot(aes(x = Scenario, y = Index)) +
        ggtitle("Grassland habitats") +
        theme_light()
dev.off()

# Upland
habitat.comp <- data.frame(Scenario = c(rep("HA_100", nrow(results.1ha$Dist_100)),
                                        rep("HA_250", nrow(results.1ha$Dist_100)),
                                        rep("QS_100", nrow(results.1ha$Dist_100)),
                                        rep("QS_250", nrow(results.1ha$Dist_100))),
                           Index = c(((results.1ha$Dist_100$UpCur / results.1ha$Dist_100$UpRef) * 100), 
                                     ((results.1ha$Dist_250$UpCur / results.1ha$Dist_250$UpRef) * 100),
                                     ((results.1qs$Dist_100$UpCur / results.1qs$Dist_100$UpRef) * 100),
                                     ((results.1qs$Dist_250$UpCur / results.1qs$Dist_250$UpRef) * 100)),
                           Area = rep(c(results.1ha$Dist_100$UpArea / rowSums(results.1ha$Dist_100[, c("UpArea", "LowArea", "GrArea")], na.rm = TRUE)), 4))

habitat.comp <- habitat.comp[habitat.comp$Area > 0.01, ]

# Boxplots
png(filename = "disp-mvp-upland-boxplot_2022-07-13.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = habitat.comp) +
        geom_boxplot(aes(x = Scenario, y = Index)) +
        ggtitle("Upland habitats") +
        theme_light()
dev.off()

# Lowland
habitat.comp <- data.frame(Scenario = c(rep("HA_100", nrow(results.1ha$Dist_100)),
                                        rep("HA_250", nrow(results.1ha$Dist_100)),
                                        rep("QS_100", nrow(results.1ha$Dist_100)),
                                        rep("QS_250", nrow(results.1ha$Dist_100))),
                           Index = c(((results.1ha$Dist_100$LowCur / results.1ha$Dist_100$LowRef) * 100), 
                                     ((results.1ha$Dist_250$LowCur / results.1ha$Dist_250$LowRef) * 100),
                                     ((results.1qs$Dist_100$LowCur / results.1qs$Dist_100$LowRef) * 100),
                                     ((results.1qs$Dist_250$LowCur / results.1qs$Dist_250$LowRef) * 100)),
                           Area = rep(c(results.1ha$Dist_100$LowArea / rowSums(results.1ha$Dist_100[, c("UpArea", "LowArea", "GrArea")], na.rm = TRUE)), 4))

habitat.comp <- habitat.comp[habitat.comp$Area > 0.01, ]

# Boxplots
png(filename = "disp-mvp-lowland-boxplot_2022-07-13.png",
    height = 1200,
    width = 1600,
    res = 300)
ggplot(data = habitat.comp) +
        geom_boxplot(aes(x = Scenario, y = Index)) +
        ggtitle("Lowland habitats") +
        theme_light()
dev.off()

#
# Spatial Map
#

# Clear memory
rm(list=ls())
gc()

library(ggplot2)
library(ggpubr)

# Load results
load("1ha.results.Rdata")
shape.in <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
eca.compare <- results.1ha$Dist_250[, c("HUC_8", "Connect")]

shape.in <- merge(shape.in, eca.compare, by = "HUC_8")

# # Save results
# write_sf(shape.in, dsn = "connect_2018_v2.shp")

png(file = paste0("connectivity-2018-1ha-250m-spatial.png"),
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

load("connectivity-1ha-100-250disp.R")

png(file = paste0("connectivity-2018-1ha-250m.png"),
    width = 1800,
    height = 1800, 
    res = 300)
landscape.map <- ggplot(data = eca.compare) + 
        geom_point(aes(x = HFI, y = disp_250)) +
        geom_abline(slope = -1, intercept = 100) +
        xlim(c(0,100)) +
        ylim(c(0,100)) +
        ylab("Connectivity") +
        xlab("% Footprint") +
        theme_light()

print(landscape.map)

dev.off()
