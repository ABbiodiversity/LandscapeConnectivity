#
# Title: Data cleaning for the landscape connectivity indicator
# Created: February 4th, 2022
# Last Updated: September 5th, 2022
# Author: Brandon Allen
# Objectives: Clean GIS data required for creating the landscape connectivity indicator. 
# Keywords: Notes, Initialization
#

# ALSO FIGURE OUT HOW TO ADD THE VEG LOOKUP TABLE INTO THE REPLACE FUNCTION SO YOU DON'T HAVE TO HARD CODE IT
# RGDAL AND RGEOS ARE BECOMING LEGACY PACKAGES, NEED TO ADDRESS CHANGES TO SP PACKAGE
#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) Movement cost raster will be calculated at the HUC 8 scale across the province.
# 3) Results are all stored in geodatabases for improved processing time and storage space
#
##################
# Initialization #  Need to confirm that the new distance_cost results are stored properly. Had to updated due to parallel processing.
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory, source scripts
rm(list=ls())
gc()

# Load libraries
library(parallel)
library(readxl)
library(reticulate)
library(scales)
library(sf)

source("src/data-cleaning_functions.R")

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

# Create shapefile for storing movement cost results
# No movement cost should ever be 0, so this will flag if watersheds are not processed
move.costs <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
move.costs$UpCur <- 0 
move.costs$LowCur <- 0 
move.costs$GrCur <- 0 
move.costs$UpRef <- 0 
move.costs$LowRef <- 0 
move.costs$GrRef <- 0 

####################
# Initialize ArcPy # 
#################### 

# Initialize arcpy
py_discover_config() # We need version 3.9
py_config() # Double check it is version 3.9

# Set python 
use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

########################
# Landscape Extraction # 
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (HUC in watershed.ids) { 
  
  # Clean the landscape
  landscape_cleaning(landcover.layer = "D:/backfill/Veg7HFI2018/gdb_SC_veg7hf_2018_rev01.gdb/vegv7hf2018",
                     boundary.layer = "data/base/gis/boundaries/HUC_8_EPSG3400.shp",
                     wildlife.layer = "data/base/wildlife-crossings/wildlife_crossings_100m.shp",
                     HUC.scale = huc.unit,
                     HUC.id = HUC,
                     arcpy = arcpy,
                     HFI.year = HFI)
  
  # Assign costs
  cost_assign(barrier.lookup = barrier.costs,
              HUC.scale = huc.unit,
              HUC.id = HUC,
              arcpy = arcpy,
              HFI.year = HFI)
  
  # Calculate costs
  move.costs <- cost_distance(status = "reference",
                              HUC.scale = huc.unit,
                              HUC.id = HUC,
                              arcpy = arcpy,
                              HFI.year = HFI)
  
  move.costs[move.costs$HUC_8 == HUC, c("UpRef", "LowRef", "GrRef")] <- move.costs
  
  move.costs <- cost_distance(status = "current",
                              HUC.scale = huc.unit,
                              HUC.id = HUC,
                              arcpy = arcpy,
                              HFI.year = HFI)
  
  move.costs[move.costs$HUC_8 == HUC, c("UpCur", "LowCur", "GrCur")] <- move.costs
  
  # Store results
  write_sf(obj = move.costs,
           dsn = paste0("data/processed/huc-", huc.unit, "/", HFI, "/movecost/huc-", huc.unit, "-movecost_", HFI, ".shp"),
           quiet = TRUE)

  print(HUC)
        
}

rm(list=ls())
gc()
