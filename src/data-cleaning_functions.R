#
# Title: Data cleaning functions
# Created: November 24th, 2020
# Last Updated: November 2nd, 2023
# Author: Brandon Allen
# Objectives: Functions required for cleaning the layers required for landscape connectivity
# Keywords: Landscape cleaning, Cost assign, Cost distance
#

#
# NOTE NEED TO CHECK THAT THE DIVIDE3d FUNCTION IS PROPERLY DIVIDING THE COST RASTERS
#

######################
# Landscape cleaning # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landscape_cleaning <- function(landcover.layer, boundary.layer, wildlife.layer, forestry.repair, HUC.scale, HUC.id, HFI.year, arcpy) {
  
  ################## Question: Should we buffer the human footprint layer to remove artifacts?
  # DECISION POINT # Implementation: We perform a 5m and -5m buffer to remove artifacts that would impact patch delineation
  ################## Final Decision: We will not buffer the polygons.
  
  ##########################
  # Create the geodatabase #
  ##########################
  
  # Create geodatabase
  arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/"), 
                                 out_name = paste0(HUC.id, ".gdb"))
  
  # Define workspace
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/", HUC.id, ".gdb")
  
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
                              out_feature_class = "landcover_nocrossing")
  
  ##################################
  # Clip Wildlife Crossings to HUC #
  ##################################
  
  arcpy$PairwiseClip_analysis(in_features = wildlife.layer, 
                              clip_features = "boundary", 
                              out_feature_class = "wildlife_crossing")
  wildlife.logical <- arcpy$GetCount_management(in_rows = "wildlife_crossing")
  
  ############################################
  # Union with Wildlife Corridors if present # 
  ############################################
  
  if(wildlife.logical[0] != "0") {
    
    arcpy$Union_analysis(in_features = "landcover_nocrossing; wildlife_crossing", 
                         out_feature_class = "landcover_crossing")
    
    arcpy$PairwiseClip_analysis(in_features = "landcover_crossing", 
                                clip_features = "boundary", 
                                out_feature_class = "landcover")
    
    # Remove intermediate files
    arcpy$Delete_management("landcover_crossing")
    
  } else {
    
    arcpy$PairwiseClip_analysis(in_features = "landcover_nocrossing", 
                                clip_features = "boundary", 
                                out_feature_class = "landcover")
    
  }
  
  # Remove intermediate files
  arcpy$Delete_management("wildlife_crossing")
  arcpy$Delete_management("landcover_nocrossing")

  ################################# HFI 2022 reclassified forestry footprint. This section standardizes the
  # Repair the forestry footprint # Forestry polygons across years
  #################################
  
  if (forestry.repair == TRUE) {
    
    # If 2022, just copy and clean
    if(HFI.year == 2022) {
      
      # Correct the naming convention in the new harvest polygons ("HARVEST-AREA")
      cursor <- arcpy$da$UpdateCursor(in_table = "landcover", 
                                      field_names = c("FEATURE_TY", "YEAR"), 
                                      where_clause = "FEATURE_TY IN ('TIMBER-HARVEST-GREEN-AREA', 'TIMBER-HARVEST-WHITE-AREA')")
      
      # Update each row in the cursor
      row <- iter_next(cursor)
      while (!is.null(row)) {
        
        # Update Feature Type
        row[[1]] <- "HARVEST-AREA"
        
        # Update the value
        cursor$updateRow(row)
        
        # Go to the next row
        row <- iter_next(cursor)  
        
      }
      
      rm(row, cursor)
      
    } else {
      
      # Otherwise perform the standardization
      
      # Select the potential harvest area polygons from the layer of interest
      arcpy$Select_analysis(in_features = "landcover", 
                            out_feature_class = "potential_harvest", 
                            where_clause = "FEATURE_TY IN ('HARVEST-AREA', 'HARVEST-AREA-WHITE-ZONE')")
      
      # Select the harvest area polygons from the 2022 HFI
      arcpy$Select_analysis(in_features = "K:/Anthropogenic/HumanFootprint/HFI/HFI_2022/HFI2022_v1_1.gdb/HFI2022", 
                            out_feature_class = "harvest_2022", 
                            where_clause = "FEATURE_TY IN ('TIMBER-HARVEST-GREEN-AREA', 'TIMBER-HARVEST-WHITE-AREA')")
      
      # Clip the potential polygons to the extent of the 2022 harvest
      arcpy$PairwiseClip_analysis(in_features = "potential_harvest", 
                                  clip_features = "harvest_2022",
                                  out_feature_class = "harvest")
      
      # Erase the defined harvest areas from the original focal year to isolate permanent polygons that
      # are in a transition to another feature type.
      arcpy$PairwiseErase_analysis(in_features = "potential_harvest", 
                                   erase_features = "harvest", 
                                   out_feature_class = "disturbance")
      
      # Correct the naming convention in the new harvest polygons ("HARVEST-AREA")
      cursor <- arcpy$da$UpdateCursor(in_table = "harvest", 
                                      field_names = c("FEATURE_TY", "YEAR"))
      
      # Update each row in the cursor
      row <- iter_next(cursor)
      while (!is.null(row)) {
        
        # Update Feature Type
        row[[1]] <- "HARVEST-AREA"
        
        # Update the value
        cursor$updateRow(row)
        
        # Go to the next row
        row <- iter_next(cursor)  
        
      }
      
      # Clear row and cursor
      rm(row, cursor)
      
      # Correct the naming convention in the remaining disturbed polygons ("WOODY-VEGETATION-REMOVAL")
      cursor <- arcpy$da$UpdateCursor(in_table = "disturbance", 
                                      field_names = c("FEATURE_TY", "YEAR"))
      
      # Update each row in the cursor
      row <- iter_next(cursor)
      while (!is.null(row)) {
        
        # Update Feature Type
        row[[1]] <-"WOODY-VEGETATION-REMOVAL"
        
        # Update the value
        cursor$updateRow(row)
        
        # Go to the next row
        row <- iter_next(cursor)  
        
      }
      
      # Clear row and cursor
      rm(row, cursor)
      
      # Create a new HFI layer with the harvest areas erased
      arcpy$PairwiseErase_analysis(in_features = "landcover", 
                                   erase_features = "potential_harvest", 
                                   out_feature_class = "hfi_no_harvest")
      
      arcpy$Delete_management(in_data = "landcover")
      
      # Add the newly updated harvest and disturbed polygons
      arcpy$Merge_management(inputs = "hfi_no_harvest; disturbance; harvest", 
                             output = "landcover")
      
      arcpy$Delete_management(in_data = "harvest")
      arcpy$Delete_management(in_data = "harvest_2022")
      arcpy$Delete_management(in_data = "potential_harvest")
      arcpy$Delete_management(in_data = "disturbance")
      arcpy$Delete_management(in_data = "hfi_no_harvest")
      
    }
    
  }
  
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
  
  # Replace " " in landcover names
  arcpy$CalculateField_management(in_table = "landcover",
                                  field = "Combined_ChgByCWCS",
                                  expression = "!Combined_ChgByCWCS!.replace(' ', '')",
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
        return 'Water'
    if arg is 'Stream':
        return 'Water'
    if arg is 'Openwater':
        return 'Water'
    if arg is 'Fen':
        return 'LowlandForest'
    if arg is 'Bog':
        return 'LowlandForest'
    if arg is 'Swamp':
        return 'LowlandForest'"
  
  # Create simplified reference condition
  arcpy$CalculateField_management(in_table = "landcover",
                                  field = "Reference",
                                  expression = "Reclass(!Combined_ChgByCWCS!)",
                                  expression_type = "PYTHON", 
                                  code_block = replace_function)
  
  # Define the python function for defining the current condition
  replace_function <- "
def Reclass(current, reference, year, hfiyear):
    if current == '':
        return reference
    if current == 'HARVEST-AREA' and ((hfiyear - year) < 4):
        return 'HARVEST-AREA-4'
    if current == 'HARVEST-AREA' and ((hfiyear - year) < 15) and ((hfiyear - year) >= 4):
        return 'HARVEST-AREA-4-15'
    if current == 'HARVEST-AREA' and ((hfiyear - year) == 15):
        return 'HARVEST-AREA-15'
    if current == 'HARVEST-AREA' and ((hfiyear - year) >= 80):
        return 'reference'
    if current == 'HARVEST-AREA' and ((hfiyear - year) > 15) and ((hfiyear - year) < 80) and reference == 'Deciduous':
        return 'Deciduous-{age}'.format(age = hfiyear - year)
    if current == 'HARVEST-AREA' and ((hfiyear - year) > 15) and ((hfiyear - year) < 80) and reference == 'Coniferous':
        return 'Coniferous-{age}'.format(age = hfiyear - year)
    if current != '':
        return current"
  
  arcpy$CalculateField_management(in_table = "landcover",
                                  field = "Current",
                                  expression = paste0("Reclass(!FEATURE_TY!, !Reference!, !YEAR!, ", HFI.year, ")"),
                                  expression_type = "PYTHON", 
                                  code_block = replace_function)
  
  # If wildlife crossings are present, correct the landcover type
  if(wildlife.logical[0] != "0") {
    
    # Define the python function for correcting wildlife corridors. Need to correct both current and reference conditions
    replace_function <- "
def Reclass(habitat, purpose):
    if purpose == 'wildlife':
        return 'Coniferous'
    if purpose != 'wildlife':
        return habitat"

    arcpy$CalculateField_management(in_table = "landcover",
                                    field = "Current",
                                    expression = "Reclass(!Current!, !Purpose!)",
                                    expression_type = "PYTHON", 
                                    code_block = replace_function)
    
    arcpy$CalculateField_management(in_table = "landcover",
                                    field = "Reference",
                                    expression = "Reclass(!Reference!, !Purpose!)",
                                    expression_type = "PYTHON", 
                                    code_block = replace_function)
    
  }
  
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
###############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_distance <- function(status, HUC.scale, HUC.id, HFI.year, arcpy) {
  
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
  
  # Create matrix for storing the results
  move.results <- matrix(data = NA, ncol = 3, nrow = 1,
                         dimnames = list(HUC.id, c("UFSource", "LFSource", "GSource")))
  
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
                    out_raster = paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/scratch/movecost", HUC.id, ".tif"))
    
    # Store results
    cost.weight <- raster(paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/scratch/movecost", HUC.id, ".tif"))
    cost.weight <- values(cost.weight)[!is.na(values(cost.weight))] # Remove NA
    cost.weight <- cost.weight[cost.weight >= 1]
    move.results[, habitat.type] <- ifelse(is.na(mean(cost.weight)) == TRUE, 1, mean(cost.weight))
    
    # Remove all layers
    arcpy$Delete_management(in_data = "costraster")
    arcpy$Delete_management(in_data = "sourceclass")
    arcpy$Delete_management(in_data = "sourceraster")
    arcpy$Delete_management(in_data = "CostDis_sour1")
    arcpy$Delete_management(in_data = "EucDist_sour1")
    arcpy$Delete_management(in_data = paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI.year, "/scratch/movecost", HUC.id, ".tif"))
    rm(cost.weight)
    
  }
  
  # Return the cost results
  save(move.results, file = paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", 
              HFI.year, "/movecost/", status, "-", HUC.id, ".Rdata"))
 
}
