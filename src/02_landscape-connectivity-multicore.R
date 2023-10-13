#
# Title: Calculation of landscape connectivity (Multicore)
# Created: February 9th, 2022
# Last Updated: October 12th, 2023
# Author: Brandon Allen
# Objectives: Process the cleaned GIS files for the connectivity index
# Keywords: Notes, Multi-class Landscape Connectivity, Visualization
#

# ISSUES TO ADDRESS
# 1) Identify where rounding error that results in current condition being greater than reference (HUC 18020102)

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
library(parallel)
library(sf)

# Load function
source("src/landscape-connectivity-v7_functions.R")

# Define threshold for the probability distribution.
# Threshold equals 5% probability of reach the distance threshold (250m; log(0.05) / 250)
dispersal.distance <- 250
dispersal.threshold <- log(0.05) / dispersal.distance

# Define minimum patch size (m2; 1ha)
minimum.patch.size <- 10000

# Define HUC units, and cost matrix
HFI <- 2018
huc.unit <- 8
watershed.costs <- read_sf("data/processed/huc-8/2018/movecost/huc-8-movecost_2018.shp")
watershed.ids <- unique(watershed.costs$HUC_8)[1:14]

# Define the recovery curve
load("data/lookup/harvest-recovery-curves_80years.Rdata")

# Create blank lists to store the results
results.current <- list()
results.reference <- list()

##################################
# Initialize parallel processing #
##################################

# Define the cores for parallel processing
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("dispersal.distance", "dispersal.threshold", "minimum.patch.size", 
                            "huc.unit", "HFI", "watershed.costs", "data_prep", "landscape_connectivity"))
clusterEvalQ(core.input, {
  
  # Load libraries
  library(foreign)
  library(igraph)
  library(Matrix)
  library(NCmisc) 
  library(raster)
  library(reticulate)
  library(sf)
  
  # Initialize arcpy
  py_discover_config() # We need version 3.9
  py_config() # Double check it is version 3.9
  
  # Set python 
  use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
  
  # Load arcpy
  arcpy <- import('arcpy') 
  numpy <- import("numpy", convert = FALSE)
  
  # Define parallel processing factor
  # This needs to be set to 0 when performing parallel processing on workers.
  # If not set to 0, processes get jumbled and may fail.
  arcpy$env$parallelProcessingFactor <- "0%"
  
})

# Prepare the native layers based on dispersal distance
# This is separated into two loops in case a landcover is present in reference, but completely removed under current conditions.

###########
# Current #
###########

start.time <- Sys.time()

# Clean the landscapes
parLapply(core.input, 
          as.list(watershed.ids), 
          fun = function(HUC) tryCatch(data_prep(status = "Current",
                                                 dispersal.distance = dispersal.distance,
                                                 minimum.patch.size = minimum.patch.size, 
                                                 harvest.recovery = recovery.curve,
                                                 HUC.scale = huc.unit,
                                                 HUC.id = HUC,
                                                 HFI.year = HFI,
                                                 arcpy = arcpy), error = function(e) e)
)

landscape.cleaning.time <- Sys.time() - start.time

start.time <- Sys.time()
# Calculate connectivity
results.current[["Grassland"]] <- parLapply(core.input, 
                                            as.list(watershed.ids), 
                                            fun = function(HUC) tryCatch(landscape_connectivity(status = "Current",
                                                                                                native.type = "Grassland",
                                                                                                watershed.costs = watershed.costs,
                                                                                                dispersal.threshold = dispersal.threshold,
                                                                                                HUC.scale = huc.unit,
                                                                                                HUC.id = HUC,
                                                                                                HFI.year = HFI,
                                                                                                arcpy = arcpy,
                                                                                                numpy = numpy), error = function(e) e)
)

results.current[["LowlandForest"]] <- parLapply(core.input, 
                                            as.list(watershed.ids), 
                                            fun = function(HUC) tryCatch(landscape_connectivity(status = "Current",
                                                                                                native.type = "LowlandForest",
                                                                                                watershed.costs = watershed.costs,
                                                                                                dispersal.threshold = dispersal.threshold,
                                                                                                HUC.scale = huc.unit,
                                                                                                HUC.id = HUC,
                                                                                                HFI.year = HFI,
                                                                                                arcpy = arcpy,
                                                                                                numpy = numpy), error = function(e) e)
)

results.current[["UplandForest"]] <- parLapply(core.input, 
                                            as.list(watershed.ids), 
                                            fun = function(HUC) tryCatch(landscape_connectivity(status = "Current",
                                                                                                native.type = "UplandForest",
                                                                                                watershed.costs = watershed.costs,
                                                                                                dispersal.threshold = dispersal.threshold,
                                                                                                HUC.scale = huc.unit,
                                                                                                HUC.id = HUC,
                                                                                                HFI.year = HFI,
                                                                                                arcpy = arcpy,
                                                                                                numpy = numpy), error = function(e) e)
)

connect.time <- Sys.time() - start.time
#############
# Reference #
#############

start.time <- Sys.time()

# Clean the landscapes
parLapply(core.input, 
          as.list(watershed.ids), 
          fun = function(HUC) tryCatch(data_prep(status = "Reference",
                                                 dispersal.distance = dispersal.distance,
                                                 minimum.patch.size = minimum.patch.size, 
                                                 harvest.recovery = recovery.curve,
                                                 HUC.scale = huc.unit,
                                                 HUC.id = HUC,
                                                 HFI.year = HFI,
                                                 arcpy = arcpy), error = function(e) e)
)

landscape.cleaning.time <- Sys.time() - start.time

start.time <- Sys.time()
# Calculate connectivity
results.reference[["Grassland"]] <- parLapply(core.input, 
                                            as.list(watershed.ids), 
                                            fun = function(HUC) tryCatch(landscape_connectivity(status = "Reference",
                                                                                                native.type = "Grassland",
                                                                                                watershed.costs = watershed.costs,
                                                                                                dispersal.threshold = dispersal.threshold,
                                                                                                HUC.scale = huc.unit,
                                                                                                HUC.id = HUC,
                                                                                                HFI.year = HFI,
                                                                                                arcpy = arcpy,
                                                                                                numpy = numpy), error = function(e) e)
)

results.reference[["LowlandForest"]] <- parLapply(core.input, 
                                                as.list(watershed.ids), 
                                                fun = function(HUC) tryCatch(landscape_connectivity(status = "Reference",
                                                                                                    native.type = "LowlandForest",
                                                                                                    watershed.costs = watershed.costs,
                                                                                                    dispersal.threshold = dispersal.threshold,
                                                                                                    HUC.scale = huc.unit,
                                                                                                    HUC.id = HUC,
                                                                                                    HFI.year = HFI,
                                                                                                    arcpy = arcpy,
                                                                                                    numpy = numpy), error = function(e) e)
)

results.reference[["UplandForest"]] <- parLapply(core.input, 
                                               as.list(watershed.ids), 
                                               fun = function(HUC) tryCatch(landscape_connectivity(status = "Reference",
                                                                                                   native.type = "UplandForest",
                                                                                                   watershed.costs = watershed.costs,
                                                                                                   dispersal.threshold = dispersal.threshold,
                                                                                                   HUC.scale = huc.unit,
                                                                                                   HUC.id = HUC,
                                                                                                   HFI.year = HFI,
                                                                                                   arcpy = arcpy,
                                                                                                   numpy = numpy), error = function(e) e)
)

connect.time <- Sys.time() - start.time
stopCluster(core.input)

#######################
# Package the results #
#######################

# Load information and package

# Add comment information 
comment(results.list) <- c("Landscape connectivity analysis based on the 2018 HFI",
                           "Backfill version 7.0", 
                           "Started on August 23rd, 2023")

# Save each watershed iteration
save(results.list, file = paste0("results/tables/connectivity_HFI", HFI, "_v7.RData"))

rm(list=ls())
gc()
