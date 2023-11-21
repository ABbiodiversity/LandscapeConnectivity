#
# Title: Packaging Results
# Created: November 21st, 2023
# Last Updated: November 21st, 2023
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

# Add results
connectivity.results$Con2010 <- landscape.connecivity.2010$Connect
connectivity.results$Con2018 <- landscape.connecivity.2018$Connect
connectivity.results$Con2019 <- landscape.connecivity.2019$Connect
connectivity.results$Con2020 <- landscape.connecivity.2020$Connect
connectivity.results$Con2021 <- landscape.connecivity.2021$Connect

# Save
write_sf(connectivity.results, dsn = "results/gis/landscape_connectivity_2010_2021.shp")

# Clear memory
rm(list=ls())
gc()
