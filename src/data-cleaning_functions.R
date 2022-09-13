#
# Title: Data cleaning functions
# Created: November 24th, 2020
# Last Updated: August 17th, 2022
# Author: Brandon Allen
# Objectives: Functions required for cleaning the layers required for landscape connectivity
# Keywords: Landscape Extraction, Multi-class Landscape Extraction
#

########################
# Landscape Extraction # Not updated for ArcPro
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landscape_extraction <- function(hfi.layer, boundary.layer, move.layer, HUC.scale, HUC.id, HFI.year, barrier.lookup, move.results, arcpy, workspace) {
  
  require(sf)
  require(raster)
  
  ###################################
  # Processing HFI and Patch Layers #
  ###################################
  
  ############################
  # Select Analysis boundary #
  ############################
  
  arcpy$Select_analysis(in_features = boundary.layer, 
                        out_feature_class = paste0("gis/", HUC.id, "/", HUC.id, "_boundary.shp"), 
                        where_clause =  paste0('"HUC_', HUC.scale,'" = ', "'", paste(HUC.id), "'"))
  
  ################### 
  # Clip HFI to HUC # 
  ###################
  
  arcpy$Clip_analysis(in_features = hfi.layer, 
                      clip_features = paste0("gis/", HUC.id, "/", HUC.id, "_boundary.shp"), 
                      out_feature_class = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI.shp"))
  
  ################## Question: Should we buffer the human footprint layer to remove artifacts?
  # DECISION POINT # Implementation: We perform a 5m and -5m buffer to remove artifacts that would impact patch delineation
  ################## Final Decision: Not made.
  
  # November 27th, 2020
  # The buffering takes significant amount of time (e.g., more than 12 hours for a single watershed)
  # Ignore until you can look into this further.
  
  # arcpy$Buffer_analysis(in_features = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI.shp"), 
  #                       out_feature_class = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI_buffer_out.shp"), 
  #                       buffer_distance_or_field = 5)
  # 
  # arcpy$Buffer_analysis(in_features = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI_buffer_out.shp"), 
  #                       out_feature_class = paste0("gis/", HUC.id, "/", HUC.id, "_HFI_buffer_in.shp"), 
  #                       buffer_distance_or_field = -5)
  
  #########################
  # Dissolve HUC from HFI #
  #########################
  
  ################## Question: Do we classify all landcover as either native or human footprint, or do we look at different native classes?
  # DECISION POINT # Implementation: Native polygons are defined as the inverse of human footprint (all native cover is equivalent).
  ################## Final Decision: 
  
  ################## Question: Should all human footprint features be considered barriers to connectivity?
  # DECISION POINT # Implementation: All human footprint types are used for patch delineation.
  ################## Final Decision: 
  
  # Changed the in_features to the non-buffered version
  
  arcpy$SymDiff_analysis(in_features = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI.shp"), 
                        update_features = paste0("gis/", HUC.id, "/", HUC.id, "_boundary.shp"), 
                        out_feature_class = paste0("gis/", HUC.id, "/", HUC.id, "_native.shp"))
  
  ##############################
  # Processing Movecost layers #
  ##############################
  
  # Identify all HUC-8 within the HUC-scale for estimating movement costs
  # Will be redundant at certain scales. Adjust for final version.
  move.huc <- move.results$HUC_8[move.results[[paste0("HUC_", HUC.scale)]] %in% HUC.id]
  
  for (move.id in move.huc) {
    
    ############################
    # Select Analysis boundary #
    ############################
    
    arcpy$Select_analysis(in_features = move.layer, 
                          out_feature_class = paste0("movecost/temporary-shapefiles/boundary.shp"), 
                          where_clause =  paste0('"HUC_8" = ', "'", paste(move.id), "'"))
    
    ################### 
    # Clip HFI to HUC # 
    ###################
    
    arcpy$Clip_analysis(in_features = hfi.layer, 
                        clip_features = paste0("movecost/temporary-shapefiles/boundary.shp"), 
                        out_feature_class = paste0("movecost/temporary-shapefiles/hfi.shp"))
    
    ################## Question: Should we buffer the human footprint layer to remove artifacts?
    # DECISION POINT # Implementation: We perform a 5m and -5m buffer to remove artifacts that would impact patch delineation
    ################## Final Decision: Not made.
    
    # See decision above on buffering
    
    # arcpy$Buffer_analysis(in_features = paste0("movecost/temporary-shapefiles/hfi.shp"), 
    #                       out_feature_class = paste0("movecost/temporary-shapefiles/hfi_buffer_out.shp"), 
    #                       buffer_distance_or_field = 5)
    # 
    # arcpy$Buffer_analysis(in_features = paste0("movecost/temporary-shapefiles/hfi_buffer_out.shp"), 
    #                       out_feature_class = paste0("movecost/temporary-shapefiles/hfi_buffer_in.shp"), 
    #                       buffer_distance_or_field = -5)
    
    #########################
    # Dissolve HUC from HFI #
    #########################
    
    ################## Question: Do we classify all landcover as either native or human footprint, or do we look at different native classes?
    # DECISION POINT # Implementation: Native polygons are defined as the inverse of human footprint (all native cover is equivalent).
    ################## Final Decision: 
    
    ################## Question: Should all human footprint features be considered barriers to connectivity?
    # DECISION POINT # Implementation: All human footprint types are used for patch delineation.
    ################## Final Decision: 
    
    arcpy$SymDiff_analysis(in_features = paste0("movecost/temporary-shapefiles/hfi.shp"), 
                           update_features = paste0("movecost/temporary-shapefiles/boundary.shp"), 
                           out_feature_class = paste0("movecost/temporary-shapefiles/native.shp"))
    
    ########################
    # Assign barrier costs #
    ########################
    
    # Load region of interest
    patch.hfi <- read_sf(paste0(workspace, "/movecost/temporary-shapefiles/hfi.shp"))
    patch.native <- read_sf(paste0(workspace, "/movecost/temporary-shapefiles/native.shp"))
    
    # If there is no HFI in the area, move cost is defaulted to 1
    
    if(nrow(patch.hfi) == 0) {
      
      move.results[move.results$HUC_8 == move.id, "FD_10"] <- 1
      move.results[move.results$HUC_8 == move.id, "FD_5"] <- 1
      move.results[move.results$HUC_8 == move.id, "AD_10"] <- 1
      move.results[move.results$HUC_8 == move.id, "AD_5"] <- 1
      
      next()
      
    }
    
    # Calculate the total area of the study region (m2)
    patch.hfi[["Area_km"]] <- round(as.numeric(st_area(patch.hfi)) / 1000000, 3)
    
    # If Harvest areas exist in the layer, update FEATURE_TY 
    # Need to account for current HFI year
    if ("HARVEST-AREA" %in% unique(patch.hfi$FEATURE_TY) == TRUE) {
      
      patch.hfi$FEATURE_TY[patch.hfi$FEATURE_TY == "HARVEST-AREA" & patch.hfi$YEAR == 0] <- "HARVEST-AREA-15" # Unknown
      patch.hfi$FEATURE_TY[patch.hfi$FEATURE_TY == "HARVEST-AREA" & (HFI.year - patch.hfi$YEAR) >= 15] <- "HARVEST-AREA-15" # Old
      patch.hfi$FEATURE_TY[patch.hfi$FEATURE_TY == "HARVEST-AREA" & (HFI.year - patch.hfi$YEAR) < 15 & (HFI.year - patch.hfi$YEAR) >= 4] <- "HARVEST-AREA-4-15" # Medium
      patch.hfi$FEATURE_TY[patch.hfi$FEATURE_TY == "HARVEST-AREA" & (HFI.year - patch.hfi$YEAR) < 4] <- "HARVEST-AREA-4" # Young
      
    }
    
    # Combine the Native and HFI layers
    patch.native <- st_cast(x = patch.native, to = "POLYGON", warn = FALSE)
    patch.native[["Area_km"]] <- round(as.numeric(st_area(patch.native)) / 1000000, 3) # convert to km2 for saving space
    patch.native[["FEATURE_TY"]] <- "Native"
    
    patch.native <- patch.native[, c("FEATURE_TY", "Area_km", "geometry")]
    patch.hfi <- patch.hfi[, c("FEATURE_TY", "Area_km", "geometry")]
    
    merged.landscape <- rbind(patch.native, patch.hfi)
    
    # Assign the four different cost estimates
    
    # FD_10
    merged.landscape[["FD_10"]] <- barrier.lookup$FD_HF_10[match(merged.landscape$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
    merged.landscape[["FD_10"]][is.na(merged.landscape[["FD_10"]])] <- 10 # Assign missing values maximum value 
    
    # FD_5
    merged.landscape[["FD_5"]] <- barrier.lookup$FD_HF_5[match(merged.landscape$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
    merged.landscape[["FD_5"]][is.na(merged.landscape[["FD_5"]])] <- 5 # Assign missing values maximum value 
    
    # AD_10
    merged.landscape[["AD_10"]] <- barrier.lookup$AD_HF_10[match(merged.landscape$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
    merged.landscape[["AD_10"]][is.na(merged.landscape[["AD_10"]])] <- 10 # Assign missing values maximum value 
    
    # AD_5
    merged.landscape[["AD_5"]] <- barrier.lookup$AD_HF_5[match(merged.landscape$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
    merged.landscape[["AD_5"]][is.na(merged.landscape[["AD_5"]])] <- 5 # Assign missing values maximum value 
    
    # Define the source creation attribute. Native needs to be less than HF so that it aligns with cells identified in the resistance layer
    merged.landscape[["Source"]] <- ifelse(merged.landscape$FEATURE_TY == "Native", 0, 1)
    
    # Save the shapefiles and begin processing in ArcGIS
    write_sf(obj = merged.landscape, dsn = paste0(workspace, "/movecost/temporary-shapefiles/merged_landscape.shp"), quiet = TRUE)
    
    rm(merged.landscape, patch.hfi, patch.native)
    gc()
    
    ########################
    # Processes Cost Layer #
    ########################
    
    ################## Question: Should their be a movement cost when moving within patches?
    # DECISION POINT # Implementation: We only implement movement penalties between patches, not within.
    ################## Final Decision: Not made.
    
    ################## Question: Which programs should be used to perform the processing
    # DECISION POINT # Implementation: ArcPy is used for all geospatial processing
    ################## Final Decision: ArcPy.
    
    ################## Question: What is the cell size for our cost rasters
    # DECISION POINT # Implementation: 10m cells
    ################## Final Decision: Not made.
    
    # # FD_10
    # # Create cost raster (10m)
    # arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
    #                                  value_field = "FD_10", 
    #                                  out_rasterdataset = paste0("movecost/temporary-shapefiles/FD_10/costraster.tif"), 
    #                                  cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "FD_10", 
    #                                  cellsize = 10)
    # 
    # # Create source raster (10m)
    # arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
    #                                  value_field = "Source", 
    #                                  out_rasterdataset = paste0("movecost/temporary-shapefiles/FD_10/sourceclass.tif"), 
    #                                  cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "Source", 
    #                                  cellsize = 10)
    # 
    # # Create source raster (Native has value of 0)
    # arcpy$Reclassify_3d(in_raster = paste0("movecost/temporary-shapefiles/FD_10/sourceclass.tif"),
    #                     reclass_field = "Value",
    #                     remap = "0 1",
    #                     out_raster = paste0("movecost/temporary-shapefiles/FD_10/sourceraster.tif"),
    #                     missing_values = "NODATA")
    # 
    # # Create the Euclidean distance raster
    # arcpy$sa$EucDistance(in_source_data = paste0("movecost/temporary-shapefiles/FD_10/sourceraster.tif")) %>%
    #   rpygeo_save(filename = paste0("movecost/temporary-shapefiles/FD_10/eucraster.tif"))
    # 
    # # Create cumulative cost raster
    # arcpy$sa$CostDistance(in_source_data = paste0("movecost/temporary-shapefiles/FD_10/sourceraster.tif"), 
    #                       in_cost_raster = paste0("movecost/temporary-shapefiles/FD_10/costraster.tif")) %>%
    #   rpygeo_save(filename = paste0("movecost/temporary-shapefiles/FD_10/weightedraster.tif"))
    # 
    # # Divide the two rasters to get the average multiplier cost for moving 
    # arcpy$Divide_3d(in_raster_or_constant1 = paste0("movecost/temporary-shapefiles/FD_10/weightedraster.tif"), 
    #                 in_raster_or_constant2 = paste0("movecost/temporary-shapefiles/FD_10/eucraster.tif"), 
    #                 out_raster = paste0("movecost/temporary-shapefiles/FD_10/movecost.tif"))
    # 
    # # Store results
    # cost.weight <- raster(paste0(workspace, "/movecost/temporary-shapefiles/FD_10/movecost.tif"))
    # cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA
    # cost.weight <- cost.weight[cost.weight >= 1]
    # move.results[move.results$HUC_8 == move.id, "FD_10"] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))
    # 
    # rm(cost.weight)
    # 
    # FD_5
    # Create cost raster (10m)
    arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
                                     value_field = "FD_5", 
                                     out_rasterdataset = paste0("movecost/temporary-shapefiles/FD_5/costraster.tif"), 
                                     cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "FD_5", 
                                     cellsize = 10)
    
    # Create source raster (10m)
    arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
                                     value_field = "Source", 
                                     out_rasterdataset = paste0("movecost/temporary-shapefiles/FD_5/sourceclass.tif"), 
                                     cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "Source", 
                                     cellsize = 10)
    
    # Create source raster (Native has value of 0)
    arcpy$Reclassify_3d(in_raster = paste0("movecost/temporary-shapefiles/FD_5/sourceclass.tif"),
                        reclass_field = "Value",
                        remap = "0 1",
                        out_raster = paste0("movecost/temporary-shapefiles/FD_5/sourceraster.tif"),
                        missing_values = "NODATA")
    
    # Create the Euclidean distance raster
    arcpy$sa$EucDistance(in_source_data = paste0("movecost/temporary-shapefiles/FD_5/sourceraster.tif")) %>%
      rpygeo_save(filename = paste0("movecost/temporary-shapefiles/FD_5/eucraster.tif"))
    
    # Create cumulative cost raster
    arcpy$sa$CostDistance(in_source_data = paste0("movecost/temporary-shapefiles/FD_5/sourceraster.tif"), 
                          in_cost_raster = paste0("movecost/temporary-shapefiles/FD_5/costraster.tif")) %>%
      rpygeo_save(filename = paste0("movecost/temporary-shapefiles/FD_5/weightedraster.tif"))
    
    # Divide the two rasters to get the average multiplier cost for moving 
    arcpy$Divide_3d(in_raster_or_constant1 = paste0("movecost/temporary-shapefiles/FD_5/weightedraster.tif"), 
                    in_raster_or_constant2 = paste0("movecost/temporary-shapefiles/FD_5/eucraster.tif"), 
                    out_raster = paste0("movecost/temporary-shapefiles/FD_5/movecost.tif"))
    
    # Store results
    cost.weight <- raster(paste0(workspace, "/movecost/temporary-shapefiles/FD_5/movecost.tif"))
    cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA
    cost.weight <- cost.weight[cost.weight >= 1]
    move.results[move.results$HUC_8 == move.id, "FD_5"] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))
    
    rm(cost.weight)
    
    # # AD_10
    # # Create cost raster (10m)
    # arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
    #                                  value_field = "AD_10", 
    #                                  out_rasterdataset = paste0("movecost/temporary-shapefiles/AD_10/costraster.tif"), 
    #                                  cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "AD_10", 
    #                                  cellsize = 10)
    # 
    # # Create source raster (10m)
    # arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
    #                                  value_field = "Source", 
    #                                  out_rasterdataset = paste0("movecost/temporary-shapefiles/AD_10/sourceclass.tif"), 
    #                                  cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "Source", 
    #                                  cellsize = 10)
    # 
    # # Create source raster (Native has value of 0)
    # arcpy$Reclassify_3d(in_raster = paste0("movecost/temporary-shapefiles/AD_10/sourceclass.tif"),
    #                     reclass_field = "Value",
    #                     remap = "0 1",
    #                     out_raster = paste0("movecost/temporary-shapefiles/AD_10/sourceraster.tif"),
    #                     missing_values = "NODATA")
    # 
    # # Create the Euclidean distance raster
    # arcpy$sa$EucDistance(in_source_data = paste0("movecost/temporary-shapefiles/AD_10/sourceraster.tif")) %>%
    #   rpygeo_save(filename = paste0("movecost/temporary-shapefiles/AD_10/eucraster.tif"))
    # 
    # # Create cumulative cost raster
    # arcpy$sa$CostDistance(in_source_data = paste0("movecost/temporary-shapefiles/AD_10/sourceraster.tif"), 
    #                       in_cost_raster = paste0("movecost/temporary-shapefiles/AD_10/costraster.tif")) %>%
    #   rpygeo_save(filename = paste0("movecost/temporary-shapefiles/AD_10/weightedraster.tif"))
    # 
    # # Divide the two rasters to get the average multiplier cost for moving 
    # arcpy$Divide_3d(in_raster_or_constant1 = paste0("movecost/temporary-shapefiles/AD_10/weightedraster.tif"), 
    #                 in_raster_or_constant2 = paste0("movecost/temporary-shapefiles/AD_10/eucraster.tif"), 
    #                 out_raster = paste0("movecost/temporary-shapefiles/AD_10/movecost.tif"))
    # 
    # # Store results
    # cost.weight <- raster(paste0(workspace, "/movecost/temporary-shapefiles/AD_10/movecost.tif"))
    # cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA 
    # cost.weight <- cost.weight[cost.weight >= 1]
    # move.results[move.results$HUC_8 == move.id, "AD_10"] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))
    # 
    # rm(cost.weight)
    # 
    # # AD_5
    # # Create cost raster (10m)
    # arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
    #                                  value_field = "AD_5", 
    #                                  out_rasterdataset = paste0("movecost/temporary-shapefiles/AD_5/costraster.tif"), 
    #                                  cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "AD_5", 
    #                                  cellsize = 10)
    # 
    # # Create source raster (10m)
    # arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/merged_landscape.shp"), 
    #                                  value_field = "Source", 
    #                                  out_rasterdataset = paste0("movecost/temporary-shapefiles/AD_5/sourceclass.tif"), 
    #                                  cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = "Source", 
    #                                  cellsize = 10)
    # 
    # # Create source raster (Native has value of 0)
    # arcpy$Reclassify_3d(in_raster = paste0("movecost/temporary-shapefiles/AD_5/sourceclass.tif"),
    #                     reclass_field = "Value",
    #                     remap = "0 1",
    #                     out_raster = paste0("movecost/temporary-shapefiles/AD_5/sourceraster.tif"),
    #                     missing_values = "NODATA")
    # 
    # # Create the Euclidean distance raster
    # arcpy$sa$EucDistance(in_source_data = paste0("movecost/temporary-shapefiles/AD_5/sourceraster.tif")) %>%
    #   rpygeo_save(filename = paste0("movecost/temporary-shapefiles/AD_5/eucraster.tif"))
    # 
    # # Create cumulative cost raster
    # arcpy$sa$CostDistance(in_source_data = paste0("movecost/temporary-shapefiles/AD_5/sourceraster.tif"), 
    #                       in_cost_raster = paste0("movecost/temporary-shapefiles/AD_5/costraster.tif")) %>%
    #   rpygeo_save(filename = paste0("movecost/temporary-shapefiles/AD_5/weightedraster.tif"))
    # 
    # # Divide the two rasters to get the average multiplier cost for moving 
    # arcpy$Divide_3d(in_raster_or_constant1 = paste0("movecost/temporary-shapefiles/AD_5/weightedraster.tif"), 
    #                 in_raster_or_constant2 = paste0("movecost/temporary-shapefiles/AD_5/eucraster.tif"), 
    #                 out_raster = paste0("movecost/temporary-shapefiles/AD_5/movecost.tif"))
    # 
    # # Store results
    # cost.weight <- raster(paste0(workspace, "/movecost/temporary-shapefiles/AD_5/movecost.tif"))
    # cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA
    # cost.weight <- cost.weight[cost.weight >= 1]
    # move.results[move.results$HUC_8 == move.id, "AD_5"] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))
    # 
    # rm(cost.weight)

    # Clean up files (All)
    do.call(file.remove, list(list.files(paste0(workspace, "movecost/temporary-shapefiles/AD_10/"), full.names = TRUE)))
    do.call(file.remove, list(list.files(paste0(workspace, "movecost/temporary-shapefiles/AD_5/"), full.names = TRUE)))
    do.call(file.remove, list(list.files(paste0(workspace, "movecost/temporary-shapefiles/FD_10/"), full.names = TRUE)))
    do.call(file.remove, list(list.files(paste0(workspace, "movecost/temporary-shapefiles/FD_5/"), full.names = TRUE)))
    
  }
  
  ############
  # Clean up # 
  ############
  
  #do.call(file.remove, list(list.files(paste0(workspace, HUC.id, "/temporary-shapefiles/"), full.names = TRUE)))

  return(move.results)
        
}

####################################
# Multi-class Landscape Extraction # 
####################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

multiclass_landscape_extraction <- function(landcover.layer, boundary.layer, move.layer, HUC.scale, HUC.id, HFI.year, barrier.lookup, landcover.lookup, move.results, arcpy, workspace, scratch) {
        
        require(foreign)
        require(sf)
        require(raster)
        
        ###############################
        # Processing landcover Layers #
        ###############################
        
        ############################
        # Select Analysis boundary #
        ############################
        
        arcpy$Select_analysis(in_features = boundary.layer, 
                              out_feature_class = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_boundary.shp"), 
                              where_clause =  paste0('"HUC_', HUC.scale,'" = ', "'", paste(HUC.id), "'"))
        
        ######################### 
        # Clip Landcover to HUC # 
        #########################
        
        # Extract landcover backfill layer
        arcpy$PairwiseClip_analysis(in_features = landcover.layer, 
                                    clip_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_boundary.shp"), 
                                    out_feature_class = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_landcover.shp"))
        
        ############################
        # Simplify landcover layer #
        ############################

        # Load attribute table
        landcover.dbf <- read.dbf(paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_landcover.dbf"))
        
        # Simplify the layer by removing attributes
        landcover.dbf$FEATURE_TY <- as.character(landcover.dbf$FEATURE_TY)
        landcover.dbf$HABITAT <- as.character(landcover.dbf$Combined_C)
        
        # Add check for UID
        if(!("UID" %in% colnames(landcover.dbf))) {
                
                landcover.dbf$UID <- 1:nrow(landcover.dbf)
                
        }
        
        landcover.dbf <- landcover.dbf[, c("UID", "HABITAT", "FEATURE_TY", "YEAR")]
        
        # Reclassify the land cover
        landcover.dbf$HABITAT <- landcover.lookup$Class[match(landcover.dbf$HABITAT, landcover.lookup$Habitat)]
        landcover.dbf$FEATURE_TY[is.na(landcover.dbf$FEATURE_TY)] <- landcover.dbf$HABITAT[is.na(landcover.dbf$FEATURE_TY)]
        
        # If Harvest areas exist in the layer, update FEATURE_TY 
        # Need to account for current HFI year
        if ("HARVEST-AREA" %in% unique(landcover.dbf$FEATURE_TY) == TRUE) {
                
                landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA" & landcover.dbf$YEAR == 0] <- "HARVEST-AREA-15" # Unknown
                landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA" & (HFI.year - landcover.dbf$YEAR) >= 15] <- "HARVEST-AREA-15" # Old
                landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA" & (HFI.year - landcover.dbf$YEAR) < 15 & (HFI.year - landcover.dbf$YEAR) >= 4] <- "HARVEST-AREA-4-15" # Medium
                landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA" & (HFI.year - landcover.dbf$YEAR) < 4] <- "HARVEST-AREA-4" # Young
                
                # Create new footprint categories based on the age categories for partial recovery
                for(forest.type in c("PineSpruce", "MixDecid")) {
                        
                        landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA-15" & landcover.dbf$HABITAT == forest.type & (HFI.year - landcover.dbf$YEAR) >= 20 & (HFI.year - landcover.dbf$YEAR) < 40] <- paste0(forest.type, "20") # 20-40
                        landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA-15" & landcover.dbf$HABITAT == forest.type & (HFI.year - landcover.dbf$YEAR) >= 40 & (HFI.year - landcover.dbf$YEAR) < 60] <- paste0(forest.type, "40") # 40-60
                        landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA-15" & landcover.dbf$HABITAT == forest.type & (HFI.year - landcover.dbf$YEAR) >= 60 & (HFI.year - landcover.dbf$YEAR) < 80] <- paste0(forest.type, "60") # 60-80
                        landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY == "HARVEST-AREA-15" & landcover.dbf$HABITAT == forest.type & (HFI.year - landcover.dbf$YEAR) >= 80] <- forest.type # Full recovered
                        
                }
                
        }
        
        # Update attributes
        attributes(landcover.dbf)$data_types <- c("N", "C", "C", "N")
        
        # Save the database and begin processing in ArcGIS
        write.dbf(dataframe = landcover.dbf, 
                  file = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_landcover.dbf"))
        
        # Repair any stray geometry
        arcpy$RepairGeometry_management(in_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_landcover.shp"),
                                        delete_null = "KEEP_NULL")
        
        # Dissolve landcover into simplified current (FEATURE_TY) and reference (Combined_C) landcovers.
        arcpy$PairwiseDissolve_analysis(in_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_landcover.shp"), 
                                        out_feature_class = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_current_landcover.shp"), 
                                        dissolve_field = "FEATURE_TY", 
                                        multi_part = "SINGLE_PART")
        
        arcpy$PairwiseDissolve_analysis(in_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_landcover.shp"), 
                                        out_feature_class = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_reference_landcover.shp"), 
                                        dissolve_field = "HABITAT", 
                                        multi_part = "SINGLE_PART")
        
        ################## Question: Should we buffer the human footprint layer to remove artifacts?
        # DECISION POINT # Implementation: We perform a 5m and -5m buffer to remove artifacts that would impact patch delineation
        ################## Final Decision: Not made, but processing limitation mean we won't be buffering at the moment.
        
        # November 27th, 2020
        # The buffering takes significant amount of time (e.g., more than 12 hours for a single watershed)
        # Ignore until you can look into this further.
        
        # arcpy$Buffer_analysis(in_features = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI.shp"), 
        #                       out_feature_class = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI_buffer_out.shp"), 
        #                       buffer_distance_or_field = 5)
        # 
        # arcpy$Buffer_analysis(in_features = paste0("gis/", HUC.id, "/temporary-shapefiles/", HUC.id, "_HFI_buffer_out.shp"), 
        #                       out_feature_class = paste0("gis/", HUC.id, "/", HUC.id, "_HFI_buffer_in.shp"), 
        #                       buffer_distance_or_field = -5)
        
        ##############################
        # Processing Movecost layers #
        ##############################
        
        # Identify all HUC-8 within the HUC-scale for estimating movement costs
        # Will be redundant at certain scales. Adjust for final version.
        move.huc <- move.results$HUC_8[move.results[[paste0("HUC_", HUC.scale)]] %in% HUC.id]
        
        for (move.id in move.huc) {
                
                ############################
                # Select Analysis boundary #
                ############################
                
                arcpy$Select_analysis(in_features = move.layer, 
                                      out_feature_class = paste0(workspace, "movecost/temporary-shapefiles/boundary.shp"), 
                                      where_clause =  paste0('"HUC_8" = ', "'", paste(move.id), "'"))
                
                #########################
                # Clip Landcover to HUC # Take from the first subset to save processing time
                #########################
                
                # Current
                arcpy$PairwiseClip_analysis(in_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_current_landcover.shp"), 
                                    clip_features = paste0(workspace, "movecost/temporary-shapefiles/boundary.shp"), 
                                    out_feature_class = paste0(workspace, "movecost/temporary-shapefiles/current_landcover.shp"))
                
                # Reference
                arcpy$PairwiseClip_analysis(in_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_reference_landcover.shp"), 
                                    clip_features = paste0(workspace, "movecost/temporary-shapefiles/boundary.shp"), 
                                    out_feature_class = paste0(workspace, "movecost/temporary-shapefiles/reference_landcover.shp"))
                
                
                ################## Question: Should we buffer the human footprint layer to remove artifacts?
                # DECISION POINT # Implementation: We perform a 5m and -5m buffer to remove artifacts that would impact patch delineation
                ################## Final Decision: Not made.
                
                # See decision above on buffering
                
                # arcpy$Buffer_analysis(in_features = paste0("movecost/temporary-shapefiles/hfi.shp"), 
                #                       out_feature_class = paste0("movecost/temporary-shapefiles/hfi_buffer_out.shp"), 
                #                       buffer_distance_or_field = 5)
                # 
                # arcpy$Buffer_analysis(in_features = paste0("movecost/temporary-shapefiles/hfi_buffer_out.shp"), 
                #                       out_feature_class = paste0("movecost/temporary-shapefiles/hfi_buffer_in.shp"), 
                #                       buffer_distance_or_field = -5)
                
                ########################
                # Assign barrier costs # 
                ########################
                
                # For each landscape type (current vs reference) and habitat type, extract mean cost values
                
                for (landscape.state in c("reference", "current")) {
                        
                        # Load database
                        landcover.dbf <- read.dbf(paste0(workspace, 
                                                         "movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"))
                        
                        # Landcover will have a single attribute. Rename to keep consistency between current and reference
                        colnames(landcover.dbf) <- "FEATURE_TY"
                        
                        #########################
                        # Assign Cost Estimates #
                        #########################
                        
                        # Assign cost for each habitat type and reference period
                        landcover.dbf[["UFCost"]] <- barrier.lookup[["UplandCost"]][match(landcover.dbf$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
                        landcover.dbf[["LFCost"]] <- barrier.lookup[["LowlandCost"]][match(landcover.dbf$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
                        landcover.dbf[["GCost"]] <- barrier.lookup[["GrasslandCost"]][match(landcover.dbf$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
                        
                        # Assign sources for each habitat type
                        landcover.dbf[["UFSource"]] <- ifelse(landcover.dbf$FEATURE_TY %in% c("PineSpruce", "PineSpruce20", "PineSpruce40", "PineSpruce60",
                                                                                              "MixDecid", "MixDecid20", "MixDecid40", "MixDecid60"), 0, 1)
                        landcover.dbf[["LFSource"]] <- ifelse(landcover.dbf$FEATURE_TY == "LowlandForest", 0, 1)
                        landcover.dbf[["GSource"]] <- ifelse(landcover.dbf$FEATURE_TY == "Grassland", 0, 1)
                        
                        # Update attributes
                        attributes(landcover.dbf)$data_types <- c("C", "N", "N", "N", "N", "N", "N")
                        
                        # Save the shapefiles and begin processing in ArcGIS
                        write.dbf(dataframe = landcover.dbf, 
                                  file = paste0(workspace, 
                                                "movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"))
                        
                        if(landscape.state == "current") {
                                
                                # Added the aged harvest areas that now count as sources.
                                hf.count <- length(landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY %in% c(unique(landcover.classes$Class),
                                                                                                            "PineSpruce20", "PineSpruce40", "PineSpruce60",
                                                                                                            "MixDecid20", "MixDecid40", "MixDecid60")])
                                
                                # Check if there is footprint within the boundary. If not, assume same value as reference condition
                                if(hf.count == nrow(landcover.dbf)) {
                                        
                                        move.results$UpCur[move.results$HUC_8 == move.id] <- move.results$UpRef[move.results$HUC_8 == move.id]
                                        move.results$LowCur[move.results$HUC_8 == move.id] <- move.results$LowRef[move.results$HUC_8 == move.id]
                                        move.results$GrCur[move.results$HUC_8 == move.id] <- move.results$GrRef[move.results$HUC_8 == move.id]
                                        
                                        next()
                                        
                                }
                                
                        }
                        
                        # Define potential habitat types (since sources are defined as 0, remove source types that equal nrows; Check that 2 patches are present
                        habitat.avail <- c(colnames(landcover.dbf[, 5:7]))[ifelse(colSums(landcover.dbf[, 5:7]) <= (nrow(landcover.dbf) - 2), TRUE, FALSE)]
                        
                        # Remove attribute table
                        rm(landcover.dbf)
                        gc()
                        
                        for (habitat.type in habitat.avail) {
                                
                                ########################
                                # Processes Cost Layer #
                                ########################
                                
                                # Define cost and source columns
                                cost.value <- gsub("Source", "Cost", habitat.type)
                                source.value <- habitat.type
                                
                                ################## Question: Should their be a movement cost when moving within patches?
                                # DECISION POINT # Implementation: We only implement movement penalties between patches, not within.
                                ################## Final Decision: Not made, but highly likely.
                                
                                ################## Question: Which programs should be used to perform the processing
                                # DECISION POINT # Implementation: ArcPy is used for all geospatial processing
                                ################## Final Decision: ArcPy.
                                
                                ################## Question: What is the cell size for our cost rasters
                                # DECISION POINT # Implementation: 10m cells
                                ################## Final Decision: Not made, but likely 10m.
                                
                                # Create cost raster (10m)
                                arcpy$PolygonToRaster_conversion(in_features = paste0(workspace, "movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"), 
                                                                 value_field = cost.value, 
                                                                 out_rasterdataset = paste0(workspace, "movecost/temporary-shapefiles/costraster.tif"), 
                                                                 cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = cost.value, 
                                                                 cellsize = 10)
                                
                                # Create source raster (10m)
                                arcpy$PolygonToRaster_conversion(in_features = paste0(workspace, "movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"), 
                                                                 value_field = source.value, 
                                                                 out_rasterdataset = paste0(workspace, "movecost/temporary-shapefiles/sourceclass.tif"), 
                                                                 cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = source.value, 
                                                                 cellsize = 10)
                                
                                # Create source raster (Native has value of 0)
                                arcpy$Reclassify_3d(in_raster = paste0(workspace, "movecost/temporary-shapefiles/sourceclass.tif"),
                                                    reclass_field = "Value",
                                                    remap = "0 1",
                                                    out_raster = paste0(workspace, "movecost/temporary-shapefiles/sourceraster.tif"),
                                                    missing_values = "NODATA")
                                
                                # Create the Euclidean distance raster
                                arcpy$sa$EucDistance(in_source_data = paste0(workspace, "movecost/temporary-shapefiles/sourceraster.tif"))
                                
                                # Create cumulative cost raster
                                arcpy$sa$CostDistance(in_source_data = paste0(workspace, "movecost/temporary-shapefiles/sourceraster.tif"), 
                                                      in_cost_raster = paste0(workspace, "movecost/temporary-shapefiles/costraster.tif"))
                                
                                # Divide the two rasters to get the average multiplier cost for moving 
                                arcpy$Divide_3d(in_raster_or_constant1 = paste0(scratch, "CostDis_sourcer1.tif"), 
                                                in_raster_or_constant2 = paste0(scratch, "EucDist_sourcer1.tif"), 
                                                out_raster = paste0(workspace, "movecost/temporary-shapefiles/movecost.tif"))
                                
                                # Store results
                                cost.weight <- raster(paste0(workspace, "/movecost/temporary-shapefiles/movecost.tif"))
                                cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA
                                cost.weight <- cost.weight[cost.weight >= 1]
                                
                                # Define names
                                state.id <- ifelse(landscape.state == "current", "Cur", "Ref")
                                if(habitat.type == "UFSource") {col.id <- paste0("Up", state.id)}
                                if(habitat.type == "LFSource") {col.id <- paste0("Low", state.id)}
                                if(habitat.type == "GSource") {col.id <- paste0("Gr", state.id)}
                                
                                move.results[move.results$HUC_8 == move.id, col.id] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))
                                rm(cost.weight)
                                
                                # Remove all layers
                                arcpy$Delete_management(in_data = list.files(scratch, full.names = TRUE))
                                arcpy$Delete_management(in_data = paste0(workspace, "movecost/temporary-shapefiles/costraster.tif"))
                                arcpy$Delete_management(in_data = paste0(workspace, "movecost/temporary-shapefiles/sourceclass.tif"))
                                arcpy$Delete_management(in_data = paste0(workspace, "movecost/temporary-shapefiles/sourceraster.tif"))
                                arcpy$Delete_management(in_data = paste0(workspace, "/movecost/temporary-shapefiles/movecost.tif"))
                                
                        }
                        
                }
                
                # Clean up remaining shapefiles files (All)
                do.call(file.remove, list(list.files(paste0(workspace, "movecost/temporary-shapefiles/"), full.names = TRUE)))
                
        }
        
        # Return the results
        return(move.results)
        
}

####################################
# Multi-class Landscape Cost only #
####################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

multiclass_cost <- function(landcover.layer, boundary.layer, move.layer, HUC.scale, HUC.id, HFI.year, barrier.lookup, landcover.lookup, move.results, arcpy, workspace) {
  
  require(foreign)
  require(sf)
  require(raster)
  
  ##############################
  # Processing Movecost layers #
  ##############################
  
  # Identify all HUC-8 within the HUC-scale for estimating movement costs
  # Will be redundant at certain scales. Adjust for final version.
  move.huc <- move.results$HUC_8[move.results[[paste0("HUC_", HUC.scale)]] %in% HUC.id]
  
  for (move.id in move.huc) {
    
    ############################
    # Select Analysis boundary #
    ############################
    
    arcpy$Select_analysis(in_features = move.layer, 
                          out_feature_class = paste0("movecost/temporary-shapefiles/boundary.shp"), 
                          where_clause =  paste0('"HUC_8" = ', "'", paste(move.id), "'"))
    
    #########################
    # Clip Landcover to HUC # Take from the first subset to save processing time
    #########################
    
    # Current
    arcpy$Clip_analysis(in_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_current_landcover.shp"), 
                        clip_features = "movecost/temporary-shapefiles/boundary.shp", 
                        out_feature_class = "movecost/temporary-shapefiles/current_landcover.shp")
    
    # Reference
    arcpy$Clip_analysis(in_features = paste0(workspace, "gis/", HUC.id, "/", HUC.id, "_reference_landcover.shp"), 
                        clip_features = "movecost/temporary-shapefiles/boundary.shp", 
                        out_feature_class = "movecost/temporary-shapefiles/reference_landcover.shp")
    
    
    ################## Question: Should we buffer the human footprint layer to remove artifacts?
    # DECISION POINT # Implementation: We perform a 5m and -5m buffer to remove artifacts that would impact patch delineation
    ################## Final Decision: Not made.
    
    # See decision above on buffering
    
    # arcpy$Buffer_analysis(in_features = paste0("movecost/temporary-shapefiles/hfi.shp"), 
    #                       out_feature_class = paste0("movecost/temporary-shapefiles/hfi_buffer_out.shp"), 
    #                       buffer_distance_or_field = 5)
    # 
    # arcpy$Buffer_analysis(in_features = paste0("movecost/temporary-shapefiles/hfi_buffer_out.shp"), 
    #                       out_feature_class = paste0("movecost/temporary-shapefiles/hfi_buffer_in.shp"), 
    #                       buffer_distance_or_field = -5)
    
    ########################
    # Assign barrier costs # 
    ########################
    
    # For each landscape type (current vs reference) and habitat type, extract mean cost values
    
    for (landscape.state in c("current", "reference")) {
      
      # Load database
      landcover.dbf <- read.dbf(paste0(workspace, 
                                       "movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"))
      
      # Landcover will have a single attribute. Rename to keep consistency between current and reference
      colnames(landcover.dbf) <- "FEATURE_TY"
      
      #########################
      # Assign Cost Estimates #
      #########################
      
      # Assign cost for each habitat type and reference period
      landcover.dbf[["UFCost"]] <- barrier.lookup[["UplandCost"]][match(landcover.dbf$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
      landcover.dbf[["LFCost"]] <- barrier.lookup[["LowlandCost"]][match(landcover.dbf$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
      landcover.dbf[["GCost"]] <- barrier.lookup[["GrasslandCost"]][match(landcover.dbf$FEATURE_TY, barrier.lookup$FEATURE_TY_ABMI)]
      
      # Assign sources for each habitat type
      landcover.dbf[["UFSource"]] <- ifelse(landcover.dbf$FEATURE_TY == "UplandForest", 0, 1)
      landcover.dbf[["LFSource"]] <- ifelse(landcover.dbf$FEATURE_TY == "LowlandForest", 0, 1)
      landcover.dbf[["GSource"]] <- ifelse(landcover.dbf$FEATURE_TY == "Grassland", 0, 1)
      
      # Update attributes
      attributes(landcover.dbf)$data_types <- c("C", "N", "N", "N", "N", "N", "N")
      
      # Save the shapefiles and begin processing in ArcGIS
      write.dbf(dataframe = landcover.dbf, 
                file = paste0(workspace, 
                              "movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"))
      
      if(landscape.state == "current") {
        
        hf.count <- length(landcover.dbf$FEATURE_TY[landcover.dbf$FEATURE_TY %in% c(unique(landcover.classes$Class))])
        
        # Check if there is footprint within the boundary. If not, default all costs to 1
        if(hf.count == nrow(landcover.dbf)) {
          
          move.results[move.results$HUC_8 == move.id, c("UplandCostCur", 
                                                        "LowlandCostCur",
                                                        "GrasslandCostCur")] <- 1
          
          next()
          
        }
        
      }
      
      # Define potential habitat types (since sources are defined as 0, remove source types that equal nrows; Check that 2 patches are present
      habitat.avail <- c(colnames(landcover.dbf[, 5:7]))[ifelse(colSums(landcover.dbf[, 5:7]) <= (nrow(landcover.dbf) - 2), TRUE, FALSE)]
      
      # Remove attribute table
      rm(landcover.dbf)
      gc()
      
      for (habitat.type in habitat.avail) {
        
        ########################
        # Processes Cost Layer #
        ########################
        
        # Define cost and source columns
        cost.value <- gsub("Source", "Cost", habitat.type)
        source.value <- habitat.type
        
        ################## Question: Should their be a movement cost when moving within patches?
        # DECISION POINT # Implementation: We only implement movement penalties between patches, not within.
        ################## Final Decision: Not made, but highly likely.
        
        ################## Question: Which programs should be used to perform the processing
        # DECISION POINT # Implementation: ArcPy is used for all geospatial processing
        ################## Final Decision: ArcPy.
        
        ################## Question: What is the cell size for our cost rasters
        # DECISION POINT # Implementation: 10m cells
        ################## Final Decision: Not made, but likely 10m.
        
        # Create cost raster (10m)
        arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"), 
                                         value_field = cost.value, 
                                         out_rasterdataset = paste0("movecost/temporary-shapefiles/costraster.tif"), 
                                         cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = cost.value, 
                                         cellsize = 10)
        
        # Create source raster (10m)
        arcpy$PolygonToRaster_conversion(in_features = paste0("movecost/temporary-shapefiles/",landscape.state, "_landcover.dbf"), 
                                         value_field = source.value, 
                                         out_rasterdataset = paste0("movecost/temporary-shapefiles/sourceclass.tif"), 
                                         cell_assignment = "MAXIMUM_COMBINED_AREA", priority_field = source.value, 
                                         cellsize = 10)
        
        # Create source raster (Native has value of 0)
        arcpy$Reclassify_3d(in_raster = paste0("movecost/temporary-shapefiles/sourceclass.tif"),
                            reclass_field = "Value",
                            remap = "0 1",
                            out_raster = paste0("movecost/temporary-shapefiles/sourceraster.tif"),
                            missing_values = "NODATA")
        
        # Create the Euclidean distance raster
        arcpy$sa$EucDistance(in_source_data = paste0("movecost/temporary-shapefiles/sourceraster.tif")) %>%
          rpygeo_save(filename = paste0("movecost/temporary-shapefiles/eucraster.tif"))
        
        # Create cumulative cost raster
        arcpy$sa$CostDistance(in_source_data = paste0("movecost/temporary-shapefiles/sourceraster.tif"), 
                              in_cost_raster = paste0("movecost/temporary-shapefiles/costraster.tif")) %>%
          rpygeo_save(filename = paste0("movecost/temporary-shapefiles/weightedraster.tif"))
        
        # Divide the two rasters to get the average multiplier cost for moving 
        arcpy$Divide_3d(in_raster_or_constant1 = paste0("movecost/temporary-shapefiles/weightedraster.tif"), 
                        in_raster_or_constant2 = paste0("movecost/temporary-shapefiles/eucraster.tif"), 
                        out_raster = paste0("movecost/temporary-shapefiles/movecost.tif"))
        
        # Store results
        cost.weight <- raster(paste0(workspace, "/movecost/temporary-shapefiles/movecost.tif"))
        cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA
        cost.weight <- cost.weight[cost.weight >= 1]
        
        # Define names
        state.id <- ifelse(landscape.state == "current", "Cur", "Ref")
        if(habitat.type == "UFSource") {col.id <- paste0("UplandCost", state.id)}
        if(habitat.type == "LFSource") {col.id <- paste0("LowlandCost", state.id)}
        if(habitat.type == "GSource") {col.id <- paste0("GrasslandCost", state.id)}
        
        move.results[move.results$HUC_8 == move.id, col.id] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))
        rm(cost.weight)
        
      }
      
    }
    
    # Clean up files (All)
    do.call(file.remove, list(list.files(paste0(workspace, "movecost/temporary-shapefiles/"), full.names = TRUE)))
    
    ############
    # Clean up # 
    ############
    
    #do.call(file.remove, list(list.files(paste0(workspace, HUC.id, "/temporary-shapefiles/"), full.names = TRUE)))
    
  }
  
  # Return the results
  return(move.results)
}

