#
# Title: Data cleaning functions
# Created: November 24th, 2020
# Last Updated: October 21st, 2022
# Author: Brandon Allen
# Objectives: Functions required for cleaning the layers required for landscape connectivity
# Keywords: Landscape cleaning, Cost assign, Cost distance
#

#
# NOTE NEED TO CHECK THAT THE DIVIDE3d FUNCTION IS PROPERLY DIVIDING THE COST RASTERS
#

######################
# Landscape cleaning # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landscape_cleaning <- function(landcover.layer, boundary.layer, HUC.scale, HUC.id, HFI.year, arcpy) {
  
  ################## Question: Should we buffer the human footprint layer to remove artifacts?
  # DECISION POINT # Implementation: We perform a 5m and -5m buffer to remove artifacts that would impact patch delineation
  ################## Final Decision: We will not buffer the polygons.
  
  ##########################
  # Create the geodatabase #
  ##########################
  
  # Create geodatabase
  arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/huc-8/", HFI.year, "/gis/"), 
                                 out_name = paste0(HUC.id, ".gdb"))
  
  # Define workspace
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-8/", HFI.year, "/gis/", HUC.id, ".gdb")
  
  ############################
  # Select Analysis boundary #
  ############################
  
  arcpy$Select_analysis(in_features = boundary.layer, 
                        out_feature_class = "boundary", 
                        where_clause =  paste0('"HUC_', HUC.scale,'" = ', "'", paste(HUC.id), "'"))
  
  ######################### 
  # Clip Landcover to HUC # 
  #########################
  
  # Extract landcover backfill layer
  arcpy$PairwiseClip_analysis(in_features = landcover.layer, 
                              clip_features = "boundary", 
                              out_feature_class = "landcover")
  
  ############################
  # Simplify landcover layer #
  ############################
  
  # Add new fields to store simplified landcover
  arcpy$AddField_management("landcover", "Current", "TEXT")
  arcpy$AddField_management("landcover", "Reference", "TEXT")
  
  # Replace "-" in landcover names
  arcpy$CalculateField_management(in_table = "landcover",
                                  field = "Combined_ChgByCWCS",
                                  expression = "!Combined_ChgByCWCS!.replace('-', '')",
                                  expression_type = "PYTHON")
  
  # Create the replace function for landcovers
  replace_function <- "
def Reclass(arg):
    if arg is 'Alkali':
        return 'Grassland'
    if arg is 'AlpineLarch':
        return 'Coniferous'
    if arg is 'Bare':
        return 'Other'
    if arg is 'Conif':
        return 'Coniferous'
    if arg is 'Decid':
        return 'Deciduous'
    if arg is 'Fir':
        return 'Coniferous'
    if arg is 'GraminoidFen':
        return 'LowlandForest'
    if arg is 'GrassHerb':
        return 'Grassland'
    if arg is 'Marsh':
        return 'LowlandForest'
    if arg is 'Mixedwood':
        return 'Deciduous'
    if arg is 'Pine':
        return 'Coniferous'
    if arg is 'Shrub':
        return 'Grassland'
    if arg is 'ShrubbyBog':
        return 'LowlandForest'
    if arg is 'ShrubbyFen':
        return 'LowlandForest'
    if arg is 'ShrubbySwamp':
        return 'LowlandForest'
    if arg is 'SnowIce':
        return 'Water'
    if arg is 'Spruce':
        return 'Coniferous'
    if arg is 'TreedBogBSpr':
        return 'LowlandForest'
    if arg is 'TreedFenBSpr':
        return 'LowlandForest'
    if arg is 'TreedFenDecid':
        return 'LowlandForest'
    if arg is 'TreedFenLarch':
        return 'LowlandForest'
    if arg is 'TreedFenMixedwood':
        return 'LowlandForest'
    if arg is 'TreedSwampConif':
        return 'LowlandForest'
    if arg is 'TreedSwampDecid':
        return 'LowlandForest'
    if arg is 'TreedSwampFir':
        return 'LowlandForest'
    if arg is 'TreedSwampMixedwood':
        return 'LowlandForest'
    if arg is 'TreedSwampSpruce':
        return 'LowlandForest'
    if arg is 'TreedSwampForest':
        return 'LowlandForest'
    if arg is 'TreedWetlandMixedwood':
        return 'LowlandForest'
    if arg is 'Water':
        return 'Water'"
  
  # Create simplified reference condition
  arcpy$CalculateField_management(in_table = "landcover",
                                  field = "Reference",
                                  expression = "Reclass(!Combined_ChgByCWCS!)",
                                  expression_type = "PYTHON", 
                                  code_block = replace_function)
  
  # Copy FEATURE_TY to a new column
  arcpy$CalculateField_management(in_table = "landcover",
                                  field = "Current",
                                  expression = "!FEATURE_TY!",
                                  expression_type = "PYTHON")
  
  # Create UpdateCursor to replace blank footprint with reference & add ages to HARVEST-AREA
  cursor <- arcpy$da$UpdateCursor(in_table = "landcover", 
                                 field_names = list("Current", "YEAR", "Reference"),
                                 where_clause = "\"Current\" IN ('', 'HARVEST-AREA')")
  
  # Update each row in the cursor
  row <- iter_next(cursor)
  while (!is.null(row)) {
    
    # If no footprint is present, replace with native cover
    if(row[[1]] == "") {
      
      # Update the value
      row[[1]] <- row[[3]]
      cursor$updateRow(row)

    } 
    
    if(row[[1]] == "HARVEST-AREA") {
      
      # Harvest areas with resistance (0-15 years)
      if(HFI.year - row[[2]] == 15) {row[[1]] <- "HARVEST-AREA-15"} # Old harvest areas
      if(HFI.year - row[[2]] < 15 & HFI.year - row[[2]] >= 4) {row[[1]] <- "HARVEST-AREA-4-15"} # Medium harvest areas
      if(HFI.year - row[[2]] < 4) {row[[1]] <- "HARVEST-AREA-4"} # Young harvest areas
      
      # If harvest has an unknown age, assume it was harvested in 1940
      if(row[[2]] == 0) {row[[1]] <- paste0(row[[3]], "-", HFI.year-1940)} # Unknown ages are assumed old
      
      # Otherwise, calculate stand age
      if(row[[1]] == "HARVEST-AREA" & HFI.year - row[[2]] > 15) {row[[1]] <- paste0(row[[3]], "-", HFI.year-row[[2]])}
      
      # Update the value
      cursor$updateRow(row)
      
    }
    
    # Go to the next row
    row <- iter_next(cursor)  
    
  }
  
  # Clear row and cursor
  rm(row, cursor)

  # # Repair any stray geometry
  # arcpy$RepairGeometry_management(in_features = "landcover",
  #                                 delete_null = "KEEP_NULL")
  
  # Dissolve landcover into simplified current and reference landcovers.
  arcpy$PairwiseDissolve_analysis(in_features = "landcover", 
                                  out_feature_class = "current_landcover", 
                                  dissolve_field = "Current", 
                                  multi_part = "SINGLE_PART")
  
  arcpy$PairwiseDissolve_analysis(in_features = "landcover", 
                                  out_feature_class = "reference_landcover", 
                                  dissolve_field = "Reference", 
                                  multi_part = "SINGLE_PART")
  
  # Remove the original landcover layer as it is no longer needed
  arcpy$Delete_management("landcover")
  
}

###############
# Cost assign # 
###############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_assign <- function(HUC.scale, HUC.id, HFI.year, barrier.lookup, arcpy) {
  
  # Define the workspace
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/", HUC.id, ".gdb")
  
  ########################
  # Assign barrier costs # 
  ########################
  
  # Create the new fields
  for (layer in c("reference_landcover", "current_landcover")) {
    
    # Create the new fields
    arcpy$AddField_management(layer, "UFCost", "DOUBLE")
    arcpy$AddField_management(layer, "LFCost", "DOUBLE")
    arcpy$AddField_management(layer, "GCost", "DOUBLE")
    arcpy$AddField_management(layer, "UFSource", "DOUBLE")
    arcpy$AddField_management(layer, "LFSource", "DOUBLE")
    arcpy$AddField_management(layer, "GSource", "DOUBLE")
    
    # Use an update cursor to loop through the cells
    landcover.name <- ifelse(layer == "reference_landcover", "Reference", "Current")
    cursor <- arcpy$da$UpdateCursor(in_table = layer, 
                                    field_names = list(landcover.name, "UFCost", "LFCost", "GCost", 
                                                       "UFSource", "LFSource", "GSource"))
    
    # Update each row in the cursor
    row <- iter_next(cursor)
    while (!is.null(row)) {
      
      # Source assignment
      # Assuming no source unless native cover types
      row[[5]] <- 1
      row[[6]] <- 1
      row[[7]] <- 1
      
      if (length(grep("Coniferous", row[[1]], value = FALSE)) == 1){
        
        row[[5]] <- 0
        
      }
      
      if (length(grep("Deciduous", row[[1]], value = FALSE)) == 1){
        
        row[[5]] <- 0
        
      }
      
      if (row[[1]] == "LowlandForest"){
        
        row[[6]] <- 0
        
      }
      
      if (row[[1]] == "Grassland"){
        
        row[[7]] <- 0
        
      }
      
      # Get the corrected feature type name
      feature.name <- row[[1]]
      if (length(grep("Coniferous", row[[1]], value = FALSE)) == 1) {feature.name <- "Coniferous"}
      if (length(grep("Deciduous", row[[1]], value = FALSE)) == 1) {feature.name <- "Deciduous"}
      
      # Cost assignment
      row[[2]] <- barrier.lookup$UplandCost[barrier.lookup$FEATURE_TY_ABMI == feature.name][1] # Only take the first value
      row[[3]] <- barrier.lookup$LowlandCost[barrier.lookup$FEATURE_TY_ABMI == feature.name][1]
      row[[4]] <- barrier.lookup$GrasslandCost[barrier.lookup$FEATURE_TY_ABMI == feature.name][1]

      # Update the value
      cursor$updateRow(row)
      
      # Go to the next row
      row <- iter_next(cursor)  
      
    }
  
    # Clear row and cursor
    rm(row, cursor)
    
  }
  
}

#################
# Cost distance # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_distance <- function(status, HUC.scale, HUC.id, HFI.year, move.results, arcpy) {
  
  # Require packages
  require(sf)
  require(raster)
  
  # Define the workspace
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/", HUC.id, ".gdb")
  arcpy$Statistics_analysis(in_table = paste0(status, "_landcover"), 
                            out_table = paste0(status, "_classes"), 
                            statistics_fields = list(c("UFSource", "SUM"),
                                                     c("LFSource", "SUM"),
                                                     c("GSource", "SUM")))
  
  # Determine which landcover types are present (minimum of 2 patches)
  habitat.avail <- read_sf(dsn = arcpy$env$workspace,
                           layer = paste0(status, "_classes"))
  habitat.avail <- habitat.avail$FREQUENCY - habitat.avail
  habitat.avail <- colnames(habitat.avail)[habitat.avail > 2] 
  habitat.avail <- gsub("SUM_", "",  habitat.avail)
  
  # Delete the table
  arcpy$Delete_management(in_data = paste0(status, "_classes"))
  
  ###########################
  # Calculate cost distance #
  ###########################
  
  for (habitat.type in habitat.avail) {
    
    ########################
    # Processes Cost Layer #
    ########################
    
    # Define cost and source columns
    cost.value <- gsub("Source", "Cost", habitat.type)
    source.value <- habitat.type
    
    ################## Question: Should their be a movement cost when moving within patches?
    # DECISION POINT # Implementation: We only implement movement penalties between patches, not within.
    ################## Final Decision: No cost within patches.
    
    ################## Question: What is the cell size for our cost raster?
    # DECISION POINT # Implementation: 10m cells.
    ################## Final Decision: 10m cells.
    
    # Create cost raster (10m)
    arcpy$PolygonToRaster_conversion(in_features = paste0(status, "_landcover"), 
                                     value_field = cost.value, 
                                     out_rasterdataset = paste0("costraster"), 
                                     cell_assignment = "MAXIMUM_COMBINED_AREA", 
                                     priority_field = cost.value, 
                                     cellsize = 10)
    
    # Create source raster (10m)
    arcpy$PolygonToRaster_conversion(in_features = paste0(status, "_landcover"), 
                                     value_field = source.value, 
                                     out_rasterdataset = "sourceclass", 
                                     cell_assignment = "MAXIMUM_COMBINED_AREA", 
                                     priority_field = source.value, 
                                     cellsize = 10)
    
    # Create source raster (Native has value of 0)
    arcpy$Reclassify_3d(in_raster = "sourceclass",
                        reclass_field = "Value",
                        remap = "0 1",
                        out_raster = "sourceraster",
                        missing_values = "NODATA")
    
    # Create the Euclidean distance raster
    arcpy$sa$EucDistance(in_source_data = "sourceraster")
    
    # Create cumulative cost raster
    arcpy$sa$CostDistance(in_source_data = "sourceraster", 
                          in_cost_raster = "costraster")
    
    # Divide the two rasters to get the average multiplier cost for moving 
    arcpy$Divide_3d(in_raster_or_constant1 = "CostDis_sour1", 
                    in_raster_or_constant2 = "EucDist_sour1", 
                    out_raster = paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/scratch/movecost.tif"))
    
    # Store results
    cost.weight <- raster(paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/scratch/movecost.tif"))
    cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA
    cost.weight <- cost.weight[cost.weight >= 1]
    
    # Define names
    state.id <- ifelse(status == "current", "Cur", "Ref")
    if(habitat.type == "UFSource") {col.id <- paste0("Up", state.id)}
    if(habitat.type == "LFSource") {col.id <- paste0("Low", state.id)}
    if(habitat.type == "GSource") {col.id <- paste0("Gr", state.id)}
    
    move.results[move.results$HUC_8 == HUC.id, col.id] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))

    # Remove all layers
    arcpy$Delete_management(in_data = "costraster")
    arcpy$Delete_management(in_data = "sourceclass")
    arcpy$Delete_management(in_data = "sourceraster")
    arcpy$Delete_management(in_data = "CostDis_sour1")
    arcpy$Delete_management(in_data = "EucDist_sour1")
    arcpy$Delete_management(in_data = paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/scratch/movecost.tif"))
    rm(cost.weight)
    
  }
  
  # Return the cost results
  return(move.results)
  
}
