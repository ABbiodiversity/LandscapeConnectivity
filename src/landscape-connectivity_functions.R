#
# Title: Landscape connectivity index
# Created: Sept 14th, 2020
# Last Updated: October 21st, 2022
# Author: Brandon Allen
# Objectives: Functions required to calculate landscape connectivity
# Keywords: Data preparation, Landscape Connectivity, Network Visualization
#

####################
# Data preparation #
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_prep <- function(status, dispersal.distance, minimum.patch.size, harvest.recovery, HUC.scale, HUC.id, HFI.year, arcpy) {
  
  # Define the geodatabase location
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb")
  
  # Identify which landcover classes are available
  layer.id <- ifelse(status == "Current", "current_landcover", "reference_landcover")
  patch.native <- read_sf(dsn = paste0("data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb"),
                          layer = layer.id)
  
  # Identify which native landcovers are present
  unique.covers <- unique(as.data.frame(patch.native[, status])[, 1])
  native.covers <- unique.covers[unique.covers %in% c("Grassland", "LowlandForest")]
  
  if(length(grep("Coniferous", unique.covers)) > 0 | length(grep("Deciduous", unique.covers)) > 0) {
    
    native.covers <- c(native.covers, "UplandForest")
    
  }
  
  rm(patch.native)
  
  # Loop through each landcover type
  for (native.type in native.covers) {
    
    ############################
    # Select Landcover classes #
    ############################
    
    # Subset the patches
    if(native.type == "UplandForest") {
      
      ################## Question: How should upland forests be merged together?
      # DECISION POINT # Implementation: All upland forests are merged together (native and harvest). The total area of the polygon is weighted based on the recovery curve.
      ################## Final Decision: All upland forests are merged together (native and harvest). The total area of the polygon is weighted based on the recovery curve.
      
      # Select all upland forests 
      forest.types <- unique.covers[grep("Coniferous", unique.covers)]
      forest.types <- c(forest.types, unique.covers[grep("Deciduous", unique.covers)])
      
      arcpy$Select_analysis(in_features = layer.id, 
                            out_feature_class = paste0(native.type, "_", status, "_predissolve"), 
                            where_clause =  paste0('"', status,'" IN ', "('", paste(forest.types, sep = "", collapse = "', '"), "')"))
      
      # Recalculate areas
      arcpy$CalculateGeometryAttributes_management(in_features = paste0(native.type, "_", status, "_predissolve"), 
                                                   geometry_property = list(c("TotalArea", "AREA_GEODESIC")), 
                                                   area_unit = "SQUARE_METERS")
      
      # Dissolve the polygons to determine groups
      arcpy$PairwiseDissolve_analysis(in_features = paste0(native.type, "_", status, "_predissolve"), 
                                      out_feature_class = paste0(native.type, "_", status, "_groups"), 
                                      multi_part = "SINGLE_PART")
      
      # Intersect dissolved polygons with original layer to determine clusters
      arcpy$PairwiseIntersect_analysis(in_features = paste0(paste0(native.type, "_", status, "_predissolve"), ";", 
                                                            paste0(native.type, "_", status, "_groups")), 
                                       out_feature_class = paste0(native.type, "_", status, "_predissolve_groups"))
      
      # Create UpdateCursor to calculate the area corrected areas for harvest areas.
      cursor <- arcpy$da$UpdateCursor(in_table = paste0(native.type, "_", status, "_predissolve_groups"), 
                                      field_names = list(status, "TotalArea"),
                                      where_clause = paste0('"', status,'" IN ', "('", paste(forest.types, sep = "", collapse = "', '"), "')"))
      
      # Update each row in the cursor
      row <- iter_next(cursor)
      while (!is.null(row)) {
        
        # If it is a native stand, use whole area
        if(!(row[[1]] %in% c("Coniferous", "Deciduous"))) {
          
          # Determine forest stand type
          if(length(grep("Coniferous", row[[1]]) == 1)) {stand.type <- "Coniferous"}
          if(length(grep("Deciduous", row[[1]]) == 1)) {stand.type <- "Deciduous"}
          
          # Determine the age of the habitat
          harvest.age <- as.numeric(gsub(paste0(stand.type, "-"), "", row[[1]]))
          
          # Update age
          row[[2]] <- row[[2]] * (harvest.recovery[harvest.recovery$Age == harvest.age, stand.type] / 100)
          
          # Update the value
          cursor$updateRow(row)
          
        }
  
        # Go to the next row
        row <- iter_next(cursor)  
        row

      }
      
      # Clear row and cursor
      rm(row, cursor)
      
      # Dissolve the polygons by cluster using the recalculated areas
      arcpy$PairwiseDissolve_analysis(in_features =paste0(native.type, "_", status, "_predissolve_groups"), 
                                      out_feature_class = paste0(native.type, "_", status, "_dissolve"), 
                                      dissolve_field = paste0("FID_", paste0(native.type, "_", status, "_groups")), 
                                      statistics_fields = list(c("TotalArea", "SUM")), 
                                      multi_part = "SINGLE_PART")
      
      # Rename attribute fields so they match the lowland/upland case
      arcpy$AlterField_management(in_table = paste0(native.type, "_", status, "_dissolve"), 
                                  field = "SUM_TotalArea", 
                                  new_field_name = "TotalArea",
                                  new_field_alias = "TotalArea")
      
      # Remove the layers that aren't required.
      arcpy$Delete_management(paste0(native.type, "_", status, "_groups"))
      arcpy$Delete_management(paste0(native.type, "_", status, "_predissolve_groups"))

    } else {
      
      # Lowland and Grassland have a simplified process since no recovery curve is implemented
      arcpy$Select_analysis(in_features = layer.id, 
                            out_feature_class = paste0(native.type, "_", status, "_predissolve"), 
                            where_clause =  paste0('"', status,'" = ', "'", native.type, "'"))
      
      # Dissolve the polygons
      arcpy$PairwiseDissolve_analysis(in_features = paste0(native.type, "_", status, "_predissolve"), 
                                      out_feature_class = paste0(native.type, "_", status, "_dissolve"), 
                                      dissolve_field = status, 
                                      multi_part = "SINGLE_PART")
      # Recalculate areas
      arcpy$CalculateGeometryAttributes_management(in_features = paste0(native.type, "_", status, "_dissolve"), 
                                                   geometry_property = list(c("TotalArea", "AREA_GEODESIC")), 
                                                   area_unit = "SQUARE_METERS")
      
    }
    
    # Remove small native patches that are unsuitable
    arcpy$Select_analysis(in_features = paste0(native.type, "_", status, "_dissolve"), 
                          out_feature_class = paste0(native.type, "_", status, "_viable"), 
                          where_clause =  paste0('"TotalArea" >= ', minimum.patch.size))
    
    ################## Question: How should distances between polygons be calculated e.g., (centroid, edge-edge)?
    # DECISION POINT # Implementation: Distance between polygons is calculated based edge-edge distance. (Still investigating this is true)
    ################## Final Decision: Edge-edge distances are used.
    
    ################## Question: When determining if direct connections are possible between polygons, should we implement a threshold?
    # DECISION POINT # Implementation: We currently define direct connections as 2x the dispersal distance.
    ################## Final Decision: 2x the dispersal distance (250 dispersal, 500 maximum)
    
    # Using a distance that is 2x the maximum distance tested (twice maximum dispersal)
    arcpy$GenerateNearTable_analysis(in_features = paste0(native.type, "_", status, "_viable"),
                                     near_features = paste0(native.type, "_", status, "_viable"),
                                     out_table = paste0(native.type, "_", status, "_distmatrix"),
                                     search_radius =  paste(dispersal.distance*2, "Meters"),
                                     closest = "ALL",
                                     method = "GEODESIC")
    
    # Remove the layers that aren't required.
    arcpy$Delete_management(paste0(native.type, "_", status, "_predissolve"))
    arcpy$Delete_management(paste0(native.type, "_", status, "_dissolve"))

  }
  
  return(native.covers)
  
}
  
##########################
# Landscape Connectivity # Need to implement saving function of patch contributions
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landscape_connectivity<- function(status, native.type, watershed.costs, dispersal.threshold, HUC.scale, HUC.id, HFI.year, results, arcpy) {
        
  # Define the geodatabase location
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb")
  
  # Calculate the area of the focal boundary
  arcpy$CalculateGeometryAttributes_management(in_features = "boundary", 
                                               geometry_property = list(c("TotalArea", "AREA_GEODESIC")), 
                                               area_unit = "SQUARE_METERS")
  
  # Calculate total area of the focal boundary
  total.area <- read_sf(paste0("data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb"),
                        layer = "boundary")
  total.area <- round(total.area$TotalArea / 1000000, 3) # convert to km2
  
  # Load the distance matrix and native polygons into memory
  patch.dist <- read_sf(dsn = paste0("data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/", HUC.id, ".gdb"),
                        layer = paste0(native.type, "_", status, "_distmatrix"))
  
  native.patches <- read_sf(dsn = paste0("data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/", HUC.id, ".gdb"),
                            layer = paste0(native.type, "_", status, "_viable"))
  native.patches$TotalAreakm <- native.patches$TotalArea / 1000000 # Convert to km2
  
  # Get correct column identifier
  col.id <- ifelse(native.type == "Grassland", "Gr",
                   ifelse(native.type == "UplandForest", "Up", "Low"))
  col.id <- ifelse(status == "Current", paste0(col.id, "Cur"), paste0(col.id, "Ref"))
  
  # If there are fewer than 2 native patches, set to NA
  if(nrow(native.patches) < 2) {
    
    results[[col.id]][HUC.id, "HUC_8"] <- HUC.id
    
    return(results)
    
  }
  
  # There are going to be patches that are not connected to any other patch. Add them to the patch distance file. 
  # NEAR_DIST will be given a value of 0
  patch.list <- 1:nrow(native.patches)
  isolated.patched <- patch.list[!(patch.list %in% unique(c(patch.dist$IN_FID, patch.dist$NEAR_FID)))]
  
  for(isolated.id in isolated.patched) {
    
    patch.dist <- rbind(patch.dist, c(isolated.id, isolated.id, 0, 1))
    
  }

  # Extract mean cost value
  mean.cost <- as.data.frame(watershed.costs[watershed.costs$HUC_8 == HUC.id, col.id])[1,1]
  
  # Calculate weighted distances
  patch.dist$Weight <- exp(dispersal.threshold * (patch.dist$NEAR_DIST * mean.cost))
  
  # Convert weights to the log scale and inverse (*-1) so the paths function is correct
  patch.dist$Log <- log(patch.dist$Weight) * -1
  
  ##########################
  # Calculate Connectivity #
  ##########################
  
  # Create graph, make sure to add vertice flag
  landscape.matrix <- graph_from_data_frame(d = patch.dist[, 1:2], vertices = rownames(native.patches))
  E(landscape.matrix)$weight <- patch.dist$Log # Add weights
  
  # Calculate index and store required results
  results[[col.id]][HUC.id, "HUC_8"] <- HUC.id
  
  #
  # Matrix Version
  #
  
  if(estimate.memory(dat = c(nrow(native.patches), nrow(native.patches)), unit = "gb") < 10) {
    
    # Create habitat matrix with the adjusted areas
    habitat.matrix <- outer(native.patches$TotalAreakm, native.patches$TotalAreakm, FUN = "*")
    
    # Using the network with predefined weights, calculate the shortest path between all patches
    # Assumes that the distances between patches are probabilities converted to the log scale.
    dist.matrix <- distances(graph = landscape.matrix) 
    dist.matrix <- exp(dist.matrix * -1) # Convert back to probability
    
    matrix.sum <- rowSums(habitat.matrix * dist.matrix)
    
    results[[col.id]][HUC.id, "Habitat_Area"] <- sum(native.patches$TotalAreakm)
    
    results[[col.id]][HUC.id, "ECA"] <- sqrt(sum(matrix.sum))
    
    results[[col.id]][HUC.id, "Watershed_Area"] <- total.area
    
    # Append the contribution values
    arcpy$AddField_management(paste0(native.type, "_", status, "_viable"), "Contribution", "FLOAT")
    
    # Create UpdateCursor to add the contribution information
    cursor <- arcpy$da$UpdateCursor(in_table = paste0(native.type, "_", status, "_viable"), 
                                    field_names = list("Contribution"))
    
    # Update each row in the cursor
    row <- iter_next(cursor)
    iter <- 1
    while (!is.null(row)) {
      
      row[[1]] <- matrix.sum[iter]
      cursor$updateRow(row)
      row <- iter_next(cursor)  
      
      iter <- iter + 1
      
    }
    
    # Clear row and cursor
    rm(row, cursor)
    
  }

  return(results)
  
}

#########################
# Network Visualization #
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_visualization <- function(edge.network, conversion) {
        
        if (conversion == TRUE) {
                
                edge.network <- unlist(strsplit(as.character(edge.network), "-")) # Split factor/characters as preparation for the network creation.
                
        }
        
        return(graph(as.numeric(edge.network))) # Create network based on edge IDs
        
}
