#
# Title: Packaging Results
# Created: November 21st, 2023
# Last Updated: November 28th, 2023
# Author: Brandon Allen
# Objectives: Package results for landscape connectivity
# Keywords: Notes, Packaging
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 
#############
# Packaging # 
#############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(sf)

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

# Align the rows
landscape.connecivity.2010 <- landscape.connecivity.2010[connectivity.results$HUC_8, ]
landscape.connecivity.2018 <- landscape.connecivity.2018[connectivity.results$HUC_8, ]
landscape.connecivity.2019 <- landscape.connecivity.2019[connectivity.results$HUC_8, ]
landscape.connecivity.2020 <- landscape.connecivity.2020[connectivity.results$HUC_8, ]
landscape.connecivity.2021 <- landscape.connecivity.2021[connectivity.results$HUC_8, ]

# Select attributes
connectivity.results <- connectivity.results[, c("HUC_8", "HUC_6", "HUC_4", "HUC_2")]

# Add total connectivity results
connectivity.results$LC2010 <- landscape.connecivity.2010$Connect
connectivity.results$LC2018 <- landscape.connecivity.2018$Connect
connectivity.results$LC2019 <- landscape.connecivity.2019$Connect
connectivity.results$LC2020 <- landscape.connecivity.2020$Connect
connectivity.results$LC2021 <- landscape.connecivity.2021$Connect

# Add the Upland forest results
connectivity.results$Upland2010 <- (landscape.connecivity.2010$UplandForestCur / landscape.connecivity.2010$UplandForestRef) * 100
connectivity.results$Upland2018 <- (landscape.connecivity.2018$UplandForestCur / landscape.connecivity.2018$UplandForestRef) * 100
connectivity.results$Upland2019 <- (landscape.connecivity.2019$UplandForestCur / landscape.connecivity.2019$UplandForestRef) * 100
connectivity.results$Upland2020 <- (landscape.connecivity.2020$UplandForestCur / landscape.connecivity.2020$UplandForestRef) * 100
connectivity.results$Upland2021 <- (landscape.connecivity.2021$UplandForestCur / landscape.connecivity.2021$UplandForestRef) * 100

# Add the lowland forest results
connectivity.results$Low2010 <- (landscape.connecivity.2010$LowlandForestCur / landscape.connecivity.2010$LowlandForestRef) * 100
connectivity.results$Low2018 <- (landscape.connecivity.2018$LowlandForestCur / landscape.connecivity.2018$LowlandForestRef) * 100
connectivity.results$Low2019 <- (landscape.connecivity.2019$LowlandForestCur / landscape.connecivity.2019$LowlandForestRef) * 100
connectivity.results$Low2020 <- (landscape.connecivity.2020$LowlandForestCur / landscape.connecivity.2020$LowlandForestRef) * 100
connectivity.results$Low2021 <- (landscape.connecivity.2021$LowlandForestCur / landscape.connecivity.2021$LowlandForestRef) * 100

# Add the grassland results
connectivity.results$Grass2010 <- (landscape.connecivity.2010$GrasslandCur / landscape.connecivity.2010$GrasslandRef) * 100
connectivity.results$Grass2018 <- (landscape.connecivity.2018$GrasslandCur / landscape.connecivity.2018$GrasslandRef) * 100
connectivity.results$Grass2019 <- (landscape.connecivity.2019$GrasslandCur / landscape.connecivity.2019$GrasslandRef) * 100
connectivity.results$Grass2020 <- (landscape.connecivity.2020$GrasslandCur / landscape.connecivity.2020$GrasslandRef) * 100
connectivity.results$Grass2021 <- (landscape.connecivity.2021$GrasslandCur / landscape.connecivity.2021$GrasslandRef) * 100

# Truncate values to 100 percent as a few have rounding errors
for(column.id in colnames(connectivity.results)[6:25]) {
  
  connectivity.results[, column.id] <- ifelse(as.data.frame(connectivity.results)[, column.id] > 100, 
                                              100, as.data.frame(connectivity.results)[, column.id])
  
}

for(column.id in colnames(connectivity.results)[6:25]) {
  
  connectivity.results[is.na(as.data.frame(connectivity.results)[, column.id]), column.id] <- -9999
  
}

# Save
write_sf(connectivity.results, dsn = "results/gis/landscape_connectivity_2010_2021.shp")

# Clear memory
rm(list=ls())
gc()
