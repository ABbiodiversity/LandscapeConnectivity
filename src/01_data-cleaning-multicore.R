#
# Title: Data cleaning for the landscape connectivity indicator (Multicore)
# Created: February 4th, 2022
# Last Updated: September 21st, 2022
# Author: Brandon Allen
# Objectives: Clean GIS data required for creating the landscape connectivity indicator. 
# Keywords: Notes, Initialization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) Movement cost raster will be calculated at the HUC 8 scale across the province.
# 3) Results are all stored in geodatabases for improved processing time and storage space
# 4) For wildlife crossings, we select infrastructure designed for wildlife, buffer to 75m, then assign an upland class (Pine).
# This is done because wildlife corridors are generally meant to improve upland connectivity. Also, currently they are restricted to the national parks.
#
##################
# Initialization # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory, source scripts
rm(list=ls())
gc()

# Load libraries
library(parallel)
library(readxl)
library(reticulate)
library(scales)
library(sf)

source("src/data-cleaning-v7_functions.R")

# Define HUC units and HFI inventories
HFI <- 2018
huc.unit <- 8
watershed.ids <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
watershed.ids <- watershed.ids$HUC_8

# Landcover lookup
landcover.classes <- read.csv("data/lookup/landcover-classification.csv")

# Load barriers information
barrier.costs <- read_excel("data/lookup/landcover-resistance.xlsx", 
                            sheet = "resistance")

# Create the scaled barrier costs for each habitat type
barrier.costs$UplandCost <- rescale(x = barrier.costs$FD_HF, to = c(1,10))
barrier.costs$LowlandCost <- rescale(x = barrier.costs$FD_HF, to = c(1,10))
barrier.costs$GrasslandCost <- rescale(x = barrier.costs$AD_HF, to = c(1,10)) # Use the AD as grassland is mostly in the south

# Update resistance costs for non-like habitat types (Grassland -> Upland Forest -> Lowland Forest)
# Values were approximated from the Scotland paper
barrier.costs$UplandCost[match(c("LowlandForest", "Grassland"), barrier.costs$FEATURE_TY_ABMI)] <- c(1.75, 2.5) 
barrier.costs$LowlandCost[match(c("Grassland", "UplandForest", "Coniferous", "Deciduous"), barrier.costs$FEATURE_TY_ABMI)] <- c(1.75, 1.75, 1.75, 1.75)
barrier.costs$GrasslandCost[match(c("LowlandForest", "UplandForest", "Coniferous", "Deciduous"), barrier.costs$FEATURE_TY_ABMI)] <- c(4.375, 3.2, 3.2, 3.2)

# Load shapefile for storing movement cost results
# No movement cost should ever be 0, so this will flag if watersheds are not processed
move.costs <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")

##################################
# Initialize parallel processing #
##################################

# Define the cores for parallel processing
start.time <- Sys.time()
n.clusters <- 6
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.unit", "HFI", "barrier.costs", "landscape_cleaning", "cost_assign", "cost_distance"))
clusterEvalQ(core.input, {
  
  # Load libraries
  library(reticulate)
  library(sf)
  library(raster)
  
  # Initialize arcpy
  py_discover_config() # We need version 3.9
  py_config() # Double check it is version 3.9
  
  # Set python 
  use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
  
  # Load arcpy
  arcpy <- import('arcpy') 
  
  # Define parallel processing factor
  arcpy$env$parallelProcessingFactor <- "100%"
  
})

########################
# Landscape Extraction #
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean the landscapes
parLapply(core.input, 
          as.list(watershed.ids[1:6]), 
          fun = function(HUC) tryCatch(landscape_cleaning(landcover.layer = "D:/backfill/Version7.0/gdb_veghf_reference_condition_2018.gdb/veghf_2018",
                                                        boundary.layer = "data/base/gis/boundaries/HUC_8_EPSG3400.shp",
                                                        wildlife.layer = "data/base/wildlife-crossings/wildlife_crossings_100m.shp",
                                                        HUC.scale = huc.unit,
                                                        HUC.id = HUC,
                                                        arcpy = arcpy,
                                                        HFI.year = HFI), error = function(e) e)
)

landscape.clean.time <- Sys.time() - start.time

# Assign costs
parLapply(core.input, 
          as.list(watershed.ids[1:6]), 
          fun = function(HUC) tryCatch(cost_assign(barrier.lookup = barrier.costs,
                                                   HUC.scale = huc.unit,
                                                   HUC.id = HUC,
                                                   arcpy = arcpy,
                                                   HFI.year = HFI), error = function(e) e)
)
cost.assign.time <- Sys.time() - start.time

# Calculate costs
reference.cost <- parLapply(core.input, 
                            as.list(watershed.ids[1:6]), 
                            fun = function(HUC) tryCatch(cost_distance(status = "reference",
                                                                       HUC.scale = huc.unit,
                                                                       HUC.id = HUC,
                                                                       arcpy = arcpy,
                                                                       HFI.year = HFI), error = function(e) e)
)

cost.distance.time <- Sys.time() - start.time

current.cost <- parLapply(core.input, 
                          as.list(watershed.ids[1:6]), 
                          fun = function(HUC) tryCatch(cost_distance(status = "current",
                                                                     HUC.scale = huc.unit,
                                                                     HUC.id = HUC,
                                                                     arcpy = arcpy,
                                                                     HFI.year = HFI), error = function(e) e)
)

cost.distance.time.2 <- Sys.time() - start.time

stopCluster(core.input)

# Format the reference and current cost to align with the shapefile
huc.names <- unlist(lapply(current.cost, function(x) rownames(x)))
current.cost <- matrix(unlist(current.cost), ncol = 3, nrow = nrow(move.costs), byrow = TRUE,
                       dimnames = list(huc.names, c("UpCur", "LowCur", "GrCur")))
current.cost <- current.cost[move.costs$HUC_8, ]
move.costs <- cbind(move.costs, current.cost)

huc.names <- unlist(lapply(reference.cost, function(x) rownames(x)))
reference.cost <- matrix(unlist(reference.cost), ncol = 3, nrow = nrow(move.costs), byrow = TRUE,
                         dimnames = list(huc.names, c("UpRef", "LowRef", "GrRef")))
reference.cost <- reference.cost[move.costs$HUC_8, ]
move.costs <- cbind(move.costs, reference.cost)

# Store results
write_sf(obj = move.costs,
         dsn = paste0("data/processed/huc-", huc.unit, "/", HFI, "/movecost/huc-", huc.unit, "-movecost_", HFI, ".shp"),
         quiet = TRUE)

# Clear memory
rm(list=ls())
gc()
