#
# Title: Calculation of landscape connectivity
# Created: February 9th, 2022
# Last Updated: August 24th, 2023
# Author: Brandon Allen
# Objectives: Process the cleaned GIS files for the connectivity index
# Keywords: Notes, Multi-class Landscape Connectivity, Visualization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) Movement cost raster will be calculated at the HUC 8 scale across the province.
# 3) If we want to filter using quarter sections, we should use the following value 647497
# 4) Full implementation for harvest recovery curves
#
######################################
# Multi-class Landscape Connectivity # 
######################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries and source functions
library(foreign)
library(igraph)
library(Matrix)
library(NCmisc) # Matrix size estimation
library(raster)
library(reticulate)
library(sf)

# Load function
source("src/landscape-connectivity_functions.R")

# Define HUC units, and cost matrix
HFI <- 2018
huc.unit <- 8
watershed.costs <- read_sf("data/processed/huc-8/2018/movecost/huc-8-movecost_2018.shp")
watershed.ids <- unique(watershed.costs$HUC_8)

# Define the recovery curve
load("data/lookup/harvest-recovery-curves_80years.Rdata")

# Define threshold for the probability distribution.
# Threshold equals 5% probability of reach the distance threshold (250m; log(0.05) / 250)
dispersal.distance <- 250
dispersal.threshold <- log(0.05) / dispersal.distance

# Define minimum patch size (m2; 1ha)
minimum.patch.size <- 10000

# Initialize arcpy
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy')

# Define the scratch space otherwise functions without defined outputs will fail
scratch.space <- "C:/Users/ballen/Desktop/LandscapeConnectivity/data/processed/huc-8/2018/scratch/"
arcpy$env$scratchWorkspace <- scratch.space

# Create matrix to store the results
results.matrix <- data.frame(matrix(nrow = length(watershed.ids), ncol = 4, dimnames = list(c(watershed.ids), c("HUC_8", "Habitat_Area", "ECA",  "Watershed_Area"))))
results.list <- list(UpCur = results.matrix,
                     LowCur = results.matrix,
                     GrCur = results.matrix,
                     UpRef = results.matrix,
                     LowRef = results.matrix,
                     GrRef = results.matrix)

# For each HUC in the layer, calculate connectivity
for (HUC in watershed.ids) {
        
  # Prepare the native layers based on dispersal distance
  # This is separated into two loops in case a landcover is present in reference, but completely removed under current conditions.
  native.classes <- data_prep(status = "Current",
                              dispersal.distance = dispersal.distance,
                              minimum.patch.size = minimum.patch.size, 
                              harvest.recovery = recovery.curve,
                              HUC.scale = huc.unit,
                              HUC.id = HUC,
                              HFI.year = HFI,
                              arcpy = arcpy)
  
  for (native.type in native.classes) {
    
    results.list <- landscape_connectivity(status = "Current",
                                           native.type = native.type,
                                           watershed.costs = watershed.costs,
                                           dispersal.threshold = dispersal.threshold,
                                           HUC.scale = huc.unit,
                                           HUC.id = HUC,
                                           HFI.year = HFI,
                                           results = results.list, 
                                           arcpy = arcpy)

  }
  
  # Prepare the reference layers based on dispersal distance
  native.classes <- data_prep(status = "Reference",
                              dispersal.distance = dispersal.distance,
                              minimum.patch.size = minimum.patch.size, 
                              harvest.recovery = recovery.curve,
                              HUC.scale = huc.unit,
                              HUC.id = HUC,
                              HFI.year = HFI,
                              arcpy = arcpy)
  
  for (native.type in native.classes) {
    
    results.list <- landscape_connectivity(status = "Reference",
                                           native.type = native.type,
                                           watershed.costs = watershed.costs,
                                           dispersal.threshold = dispersal.threshold,
                                           HUC.scale = huc.unit,
                                           HUC.id = HUC,
                                           HFI.year = HFI,
                                           results = results.list, 
                                           arcpy = arcpy)

  }
  
  # Add comment information 
  comment(results.list) <- c("Landscape connectivity analysis based on the 2018 HFI",
                             "Backfill version 7.0", 
                             "Started on August 23rd, 2023")
  
  # Save each watershed iteration
  save(results.list, file = paste0("results/tables/connectivity_HFI", HFI, ".RData"))

  print(HUC)
        
}

rm(list=ls())
gc()
