#
# Title: Data cleaning for the landscape connectivity indicator
# Created: February 4th, 2022
# Last Updated: August 17th, 2022
# Author: Brandon Allen
# Objectives: Clean GIS data required for creating the landscape connectivity indicator. 
# Keywords: Notes, Multi-class Landscape Extraction
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) Movement cost raster will be calculated at the HUC 8 scale across the province.
#
#
####################################
# Multi-class Landscape Extraction # 
####################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory, source scripts
rm(list=ls())
gc()

# Load libraries
library(foreign)
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
landcover.classes <- read.csv("data/lookup/landcover-classification_2021-11-17.csv")

# Load barriers information
barrier.costs <- read_excel("data/lookup/landscape-connectivity-resistance-final_2021-05-17.xlsx", 
                            sheet = "resistance")

# Create the scaled barrier costs for each habitat type
barrier.costs$UplandCost <- rescale(x = barrier.costs$FD_HF, to = c(1,10))
barrier.costs$LowlandCost <- rescale(x = barrier.costs$FD_HF, to = c(1,10))
barrier.costs$GrasslandCost <- rescale(x = barrier.costs$AD_HF, to = c(1,10)) # Use the AD as grassland is mostly in the south

# Update resistance costs for non-like habitat types (Grassland -> Upland Forest -> Lowland Forest)
# Values were approximated from the Scotland paper
barrier.costs$UplandCost[match(c("LowlandForest", "Grassland"), barrier.costs$FEATURE_TY_ABMI)] <- c(1.75, 2.5) 
barrier.costs$LowlandCost[match(c("UplandForest", "Grassland"), barrier.costs$FEATURE_TY_ABMI)] <- c(1.75, 1.75)
barrier.costs$GrasslandCost[match(c("UplandForest", "LowlandForest"), barrier.costs$FEATURE_TY_ABMI)] <- c(4.375, 3.2)

# Add the individual stand values
barrier.costs[barrier.costs$FEATURE_TY_MARREC %in% c("Spruce", "Spruce20", "Spruce40", "Spruce60",
                                                     "Pine", "Pine20", "Pine40", "Pine60",
                                                     "Decid", "Decid20", "Decid40", "Decid60",
                                                     "Mixedwood", "Mixedwood20" , "Mixedwood40" , "Mixedwood60"), c("UplandCost", 
                                                                                                                    "LowlandCost",
                                                                                                                    "GrasslandCost")] <- barrier.costs[barrier.costs$FEATURE_TY_MARREC %in% c("UplandForest"), c("UplandCost", "LowlandCost","GrasslandCost")]

# Create folders required for the data analysis on the first run
folders.exist <- TRUE

if(folders.exist == FALSE) {
        
        for(HUC in watershed.ids) {
                
                dir.create(paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", HUC))
                dir.create(paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", HUC, "/temporary-databases"))
                dir.create(paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", HUC, "/temporary-shapefiles"))
                
        }
        
}

# Create shapefile for storing movement cost results
move.costs <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
move.costs$UpCur <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$LowCur <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$GrCur <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$UpRef <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$LowRef <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$GrRef <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed

########################
# Landscape Extraction #
########################

# Initialize arcpy
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define the scratch space otherwise functions without defined outputs will fail
scratch.space <- "C:/Users/ballen/Desktop/LandscapeConnectivity/data/processed/huc-8/2018/scratch/"
arcpy$env$scratchWorkspace <- scratch.space

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Path to landcover layer # \\gisserver.abmi.ca\GIS\Landcover\Backfill\backfilledV61\veg61hf2018_bdqt.gdb
i <- 1

for (HUC in watershed.ids) { 
        
        # Version 1.0
        move.costs <- multiclass_landscape_extraction(landcover.layer = "D:/backfill//veg61hf2018_bdqt.gdb/veg61hf2018_BDQT_mtos",
                                      boundary.layer = "data/base/gis/boundaries/HUC_8_EPSG3400.shp",
                                      move.layer = "data/base/gis/boundaries/HUC_8_EPSG3400.shp",
                                      HUC.scale = huc.unit,
                                      HUC.id = HUC,
                                      barrier.lookup = barrier.costs,
                                      landcover.lookup = landcover.classes,
                                      move.results = move.costs,
                                      arcpy = arcpy,
                                      HFI.year = HFI,
                                      workspace = "data/processed/huc-8/2018/",
                                      scratch = scratch.space)

        # Used for saving the results after each step
        write_sf(obj = move.costs, dsn = paste0("data/processed/huc-8/2018/movecost/huc-8-movecost_", HFI, "_2022-08-24.shp"), quiet = TRUE)
        
        print(i)
        i <- i + 1
        
}

rm(list=ls())
gc()


####################################
# Multi-class Landscape Extraction # 2010 HFI
####################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory, source scripts
rm(list=ls())
gc()

# Load libraries
library(foreign)
library(readxl)
library(reticulate)
library(scales)
library(sf)

source("src/data-cleaning_functions.R")

# Define HUC units and HFI inventories
HFI <- 2010
huc.unit <- 8
watershed.ids <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
watershed.ids <- watershed.ids$HUC_8

# Landcover lookup
landcover.classes <- read.csv("data/lookup/landcover-classification_2021-11-17.csv")

# Load barriers information
barrier.costs <- read_excel("data/lookup/landscape-connectivity-resistance-final_2021-05-17.xlsx", 
                            sheet = "resistancev2")

# Create the scaled barrier costs for each habitat type
barrier.costs$UplandCost <- rescale(x = barrier.costs$FD_HF, to = c(1,10))
barrier.costs$LowlandCost <- rescale(x = barrier.costs$FD_HF, to = c(1,10))
barrier.costs$GrasslandCost <- rescale(x = barrier.costs$AD_HF, to = c(1,10)) # Use the AD as grassland is mostly in the south

# Update resistance costs for non-like habitat types (Grassland -> Upland Forest -> Lowland Forest)
# Values were approximated from the Scotland paper
barrier.costs$UplandCost[match(c("LowlandForest", "Grassland"), barrier.costs$FEATURE_TY_ABMI)] <- c(1.75, 2.5) 
barrier.costs$LowlandCost[match(c("UplandForest", "Grassland"), barrier.costs$FEATURE_TY_ABMI)] <- c(1.75, 1.75)
barrier.costs$GrasslandCost[match(c("UplandForest", "LowlandForest"), barrier.costs$FEATURE_TY_ABMI)] <- c(4.375, 3.2)

# Add the individual stand values
barrier.costs[barrier.costs$FEATURE_TY_MARREC %in% c("Spruce", "Spruce20", "Spruce40", "Spruce60",
                                                     "Pine", "Pine20", "Pine40", "Pine60",
                                                     "Decid", "Decid20", "Decid40", "Decid60",
                                                     "Mixedwood", "Mixedwood20" , "Mixedwood40" , "Mixedwood60"), c("UplandCost", 
                                                                                                       "LowlandCost",
                                                                                                       "GrasslandCost")] <- barrier.costs[barrier.costs$FEATURE_TY_MARREC %in% c("UplandForest"), c("UplandCost", "LowlandCost","GrasslandCost")]

# Create folders required for the data analysis on the first run
folders.exist <- TRUE

if(folders.exist == FALSE) {
        
        for(HUC in watershed.ids) {
                
                dir.create(paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", HUC))
                dir.create(paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", HUC, "/temporary-databases"))
                dir.create(paste0("data/processed/huc-", huc.unit, "/", HFI, "/gis/", HUC, "/temporary-shapefiles"))
                
        }
        
}

# Create shapefile for storing movement cost results
move.costs <- read_sf("data/base/gis/boundaries/HUC_8_EPSG3400.shp")
move.costs$UpCur <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$LowCur <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$GrCur <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$UpRef <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$LowRef <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed
move.costs$GrRef <- 0 # No movement cost should ever be 0, so this will flag if watersheds are no processed

########################
# Landscape Extraction #
########################

# Initialize arcpy
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define the scratch space otherwise functions without defined outputs will fail
scratch.space <- "C:/Users/ballen/Desktop/LandscapeConnectivity/data/processed/huc-8/2010/scratch/"
arcpy$env$scratchWorkspace <- scratch.space

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

i <- 1

for (HUC in watershed.ids) { 
        
        # Version 1.0
        move.costs <- multiclass_landscape_extraction(landcover.layer = "D:/backfill/Veg61_2010_HFI_2010v2_Grid_1KM.gdb/Veg61_2010_HFI_2010v2_Grid_1KM_GenHabTY",
                                                      boundary.layer = "data/base/gis/boundaries/HUC_8_EPSG3400.shp",
                                                      move.layer = "data/base/gis/boundaries/HUC_8_EPSG3400.shp",
                                                      HUC.scale = huc.unit,
                                                      HUC.id = HUC,
                                                      barrier.lookup = barrier.costs,
                                                      landcover.lookup = landcover.classes,
                                                      move.results = move.costs,
                                                      arcpy = arcpy,
                                                      HFI.year = HFI,
                                                      workspace = "data/processed/huc-8/2010/",
                                                      scratch = scratch.space)
        
        # Used for saving the results after each step
        write_sf(obj = move.costs, dsn = paste0("data/processed/huc-8/2010/movecost/huc-8-movecost_", HFI, "_2022-08-17.shp"), quiet = TRUE)
        
        print(i)
        i <- i + 1
        
}

rm(list=ls())
gc()
