#
# Title: Data cleaning for the landscape connectivity indicator (Multicore)
# Created: February 4th, 2022
# Last Updated: November 2nd, 2023
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
# 4) For wildlife crossings, we select infrastructure designed for wildlife, buffer to 100m, then assign an upland class (Pine).
# This is done because wildlife corridors are generally meant to improve upland connectivity. Also, currently they are restricted to the national parks.
#
##################
# Initialization # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory, source scripts
rm(list=ls())
gc()

# Load libraries
library(foreach)
library(parallel)
library(readxl)
library(reticulate)
library(scales)
library(sf)

source("src/data-cleaning_functions.R")

# Define HUC units and HFI inventories
hfi.series <- c(2010, 2018, 2019, 2020, 2021)
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

##################################
# Initialize parallel processing #
##################################

# Define the cores for parallel processing
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.unit", "barrier.costs", "landscape_cleaning", "cost_assign", "cost_distance"))
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
  # This needs to be set to 0 when performing parallel processing on workers.
  # If not set to 0, processes get jumbled and may fail.
  arcpy$env$parallelProcessingFactor <- "0%"
  
})

########################
# Landscape Extraction #
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean the landscapes for each inventory
foreach(hfi = hfi.series) %dopar% 
  
  parLapply(core.input, 
            as.list(watershed.ids), 
            fun = function(HUC) tryCatch(landscape_cleaning(landcover.layer = paste0("D:/backfill/Version7.0/gdb_veghf_reference_condition_", hfi, ".gdb/veghf_", hfi, ""),
                                                            boundary.layer = "data/base/gis/boundaries/HUC_8_EPSG3400.shp",
                                                            wildlife.layer = "data/base/wildlife-crossings/wildlife_crossings_100m.shp",
                                                            HUC.scale = huc.unit,
                                                            HUC.id = HUC,
                                                            arcpy = arcpy,
                                                            HFI.year = hfi), error = function(e) e)
  )
  
# Assign costs for each inventory
foreach(hfi = hfi.series) %dopar% 
  
  parLapply(core.input, 
            as.list(watershed.ids), 
            fun = function(HUC) tryCatch(cost_assign(barrier.lookup = barrier.costs,
                                                     HUC.scale = huc.unit,
                                                     HUC.id = HUC,
                                                     arcpy = arcpy,
                                                     HFI.year = hfi), error = function(e) e)
  )


# Calculate costs for each inventory under reference condition
foreach(hfi = hfi.series) %dopar%  
  
  parLapply(core.input, 
            as.list(watershed.ids), 
            fun = function(HUC) tryCatch(cost_distance(status = "reference",
                                                       HUC.scale = huc.unit,
                                                       HUC.id = HUC,
                                                       arcpy = arcpy,
                                                       HFI.year = hfi), error = function(e) e)
  )

# Calculate costs for each inventory under current condition
foreach(hfi = hfi.series) %dopar%  
  
  parLapply(core.input, 
            as.list(watershed.ids), 
            fun = function(HUC) tryCatch(cost_distance(status = "current",
                                                       HUC.scale = huc.unit,
                                                       HUC.id = HUC,
                                                       arcpy = arcpy,
                                                       HFI.year = hfi), error = function(e) e)
  )

stopCluster(core.input)

#
# Store move cost results in shapefile
#

for (HFI in hfi.series) {
  
  # Load shapefile for storing the results
  # No movement cost should ever be 0, so this will flag if watersheds are not processed
  move.costs <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
  move.costs$UpCur <- 0 
  move.costs$LowCur <- 0 
  move.costs$GrCur <- 0 
  move.costs$UpRef <- 0 
  move.costs$LowRef <- 0 
  move.costs$GrRef <- 0 
  
  # Determine files that were created
  results.list <- list.files(paste0("data/processed/huc-8/", HFI, "/movecost/"), full.names = TRUE)
  
  # Data packaging
  for(watershed in watershed.ids) {
    
    # Current
    load(results.list[grep(paste0("current-", watershed, ".Rdata"), results.list)])
    move.costs[move.costs$HUC_8 == watershed, "UpCur"] <- move.results[, "UFSource"]
    move.costs[move.costs$HUC_8 == watershed, "LowCur"] <- move.results[, "LFSource"]
    move.costs[move.costs$HUC_8 == watershed, "GrCur"] <- move.results[, "GSource"]
    
    # Reference
    load(results.list[grep(paste0("reference-", watershed, ".Rdata"), results.list)])
    move.costs[move.costs$HUC_8 == watershed, "UpRef"] <- move.results[, "UFSource"]
    move.costs[move.costs$HUC_8 == watershed, "LowRef"] <- move.results[, "LFSource"]
    move.costs[move.costs$HUC_8 == watershed, "GrRef"] <- move.results[, "GSource"]
    
  }
  
  # Store results
  write_sf(obj = move.costs,
           dsn = paste0("data/processed/huc-", huc.unit, "/", HFI, "/movecost/huc-", huc.unit, "-movecost_", HFI, ".shp"),
           quiet = TRUE)
  
  
}

rm(list=ls())
gc()
