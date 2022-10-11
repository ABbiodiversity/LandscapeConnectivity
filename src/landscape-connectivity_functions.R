#
# Title: Landscape connectivity index
# Created: Sept 14th, 2020
# Last Updated: September 27th, 2022
# Author: Brandon Allen
# Objectives: Functions required to calculate landscape connectivity
# Keywords: Data preparation, Landscape Connectivity, Network Visualization
#

#
# NOTE: Double check that fix for vertices alignment is correct
#

####################
# Data preparation #
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_prep <- function(status, HUC.scale, HUC.id, HFI.year, arcpy) {
  
  # Define the geodatabase location
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb")
  
  # Identify which landcover classes are available
  layer.id <- ifelse(status == "Current", "current_landcover", "reference_landcover")
  patch.native <- read_sf(dsn = paste0("data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb"),
                          layer = layer.id)
  
  # Identify which native landcovers are present
  unique.covers <- unique(as.data.frame(patch.native[, status])[, 1])
  native.covers <- unique.covers[unique.covers %in% c("Grassland", "LowlandForest")]
  
  if("MixDecid" %in% unique.covers | "PineSpruce" %in% unique.covers) {
    
    native.covers <- c(native.covers, "UplandForest")
    
  }
  
  rm(patch.native, unique.covers)
  
  # Loop through each landcover type
  for (native.type in native.covers) {
    
    ############################
    # Select Landcover classes #
    ############################
    
    # Subset the patches
    if(native.type == "UplandForest") {
      
      ################## Question: Should we merge MixDecid and PineSpruce stands together?
      # DECISION POINT # Implementation: Native stands are merged together into an UplandForest class. Harvest blocks are merged together based on age. 
      ################## Final Decision: Native stands are merged together into an UplandForest class. Harvest blocks are merged together based on age. 
      
      arcpy$Select_analysis(in_features = layer.id, 
                            out_feature_class = paste0(native.type, "_", status, "_predissolve"), 
                            where_clause =  paste0('"', status,'" IN ', "('MixDecid', 'MixDecid20', 'MixDecid40', 'MixDecid60', 'PineSpruce', 'PineSpruce20', 'PineSpruce40', 'PineSpruce60')"))
      
      # Replace "MixDecid" in landcover names
      arcpy$CalculateField_management(in_table = paste0(native.type, "_", status, "_predissolve"),
                                      field = status,
                                      expression = paste0("!", status, "!.replace('MixDecid', 'UplandForest')"),
                                      expression_type = "PYTHON")
      
      # Replace "PineSpruce" in landcover names
      arcpy$CalculateField_management(in_table = paste0(native.type, "_", status, "_predissolve"),
                                      field = status,
                                      expression = paste0("!", status, "!.replace('PineSpruce', 'UplandForest')"),
                                      expression_type = "PYTHON")

    } else {
      
      arcpy$Select_analysis(in_features = layer.id, 
                            out_feature_class = paste0(native.type, "_", status, "_predissolve"), 
                            where_clause =  paste0('"', status,'" = ', "'", native.type, "'"))
      
    }
    
    # Dissolve the polygons
    arcpy$PairwiseDissolve_analysis(in_features = paste0(native.type, "_", status, "_predissolve"), 
                                    out_feature_class = paste0(native.type, "_", status, "_dissolve"), 
                                    dissolve_field = status, 
                                    multi_part = "SINGLE_PART")
    
    # Remove small native patches that are unsuitable
    arcpy$Select_analysis(in_features = paste0(native.type, "_", status, "_dissolve"), 
                          out_feature_class = paste0(native.type, "_", status, "_viable"), 
                          where_clause =  paste0('"Shape_Area" >= ', "10000"))

    ################## Question: How should distances between polygons be calculated e.g., (centroid, edge-edge)?
    # DECISION POINT # Implementation: Distance between polygons is calculated based edge-edge distance. (Still investigating this is true)
    ################## Final Decision: Edge-edge distances are used.
    
    ################## Question: When determining if direct connections are possible between polygons, should we implement a threshold?
    # DECISION POINT # Implementation: We currently define direct connections as 2x the dispersal distance.
    ################## Final Decision: 2x the dispersal distance (250 dispersal, 500 maximum)
    
    # Using a distance that is 2x the maximum distance tested (Choosing  500, twice maximum dispersal)
    arcpy$GenerateNearTable_analysis(in_features = paste0(native.type, "_", status, "_viable"),
                                     near_features = paste0(native.type, "_", status, "_viable"),
                                     out_table = paste0(native.type, "_", status, "_distmatrix"),
                                     search_radius = "1000 Meters",
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

landscape_connectivity_2.0 <- function(status, native.type, watershed.costs, dist.values, HUC.scale, HUC.id, HFI.year, results, arcpy) {
        
  # Define the geodatabase location
  arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb")
  
  # Calculate total area of the focal boundary
  total.area <- read_sf(paste0("data/processed/huc-", HUC.scale, "/", HFI, "/gis/", HUC.id, ".gdb"),
                        layer = "boundary")
  total.area <- round(as.numeric(st_area(total.area)) / 1000000, 3) # convert to km2
  
  # Load the distance matrix and native polygons into memory
  patch.dist <- read_sf(dsn = paste0("data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/", HUC.id, ".gdb"),
                        layer = paste0(native.type, "_", status, "_distmatrix"))
  
  native.patches <- read_sf(dsn = paste0("data/processed/huc-", HUC.scale, "/", HFI.year, "/gis/", HUC.id, ".gdb"),
                            layer = paste0(native.type, "_", status, "_viable"))
  native.patches$Shape_Area <- native.patches$Shape_Area / 1000000 # Convert to km2
  
  # Get correct column identifier
  col.id <- ifelse(native.type == "Grassland", "Gr",
                   ifelse(native.type == "UplandForest", "Up", "Low"))
  col.id <- ifelse(status == "Current", paste0(col.id, "Cur"), paste0(col.id, "Ref"))
  
  # If there are fewer than 2 native patches, set to NA
  if(nrow(native.patches) < 2) {
    
    results[["Dist_250"]][[col.id]][HUC.id, "HUC_8"] <- HUC.id
    results[["Dist_Multi"]][[col.id]][HUC.id, "HUC_8"] <- HUC.id
    
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
  
  # Multiply the distance by the mean cost distance
  # Apply threshold for creating the probability distribution
  
  for(dist.type in names(results)) {
    
    # Select the appropriate distance value depending on if its a single or multi class
    if (dist.type == "Dist_250") {dist.id <- "Dist_250"}
    
    if(dist.type == "Dist_Multi") {
      
      if(native.type == "UplandForest") {dist.id <- "Dist_500"}
      if(native.type == "LowlandForest") {dist.id <- "Dist_150"}
      if(native.type == "Grassland") {dist.id <- "Dist_300"}
      
    }
    
    # Calculate weighted distances
    patch.dist["Weight"] <- exp(as.numeric(dist.values[dist.id]) * (patch.dist$NEAR_DIST * mean.cost))
    
    # Convert weights to the log scale
    patch.dist$Log <- log(patch.dist$Weight) * -1
    
    ##########################
    # Calculate Connectivity #
    ##########################
    
    # Create graph, make sure to add vertice flag
    landscape.matrix <- graph_from_data_frame(d = patch.dist[, 1:2], vertices = rownames(native.patches))
    E(landscape.matrix)$weight <- patch.dist$Log # Add weights
    
    # Calculate index and store required results
    results[[dist.type]][[col.id]][HUC.id, "HUC_8"] <- HUC.id
    
    #
    # Matrix Version
    #
    
    if(estimate.memory(dat = c(nrow(native.patches), nrow(native.patches)), unit = "gb") < 10) {
      
      # Create habitat matrix
      habitat.matrix <- outer(native.patches$Shape_Area, native.patches$Shape_Area, FUN = "*")
      
      # Using the network with predefined weights, calculate the shortest path between all patches
      # Assumes that the distances between patches are probabilities converted to the log scale.
      dist.matrix <- distances(graph = landscape.matrix) 
      dist.matrix <- exp(dist.matrix * -1) # Convert back to probability
      
      matrix.sum <- rowSums(habitat.matrix * dist.matrix)
      
      results[[dist.type]][[col.id]][HUC.id, "ECA"] <- sqrt(sum(matrix.sum))
      
      results[[dist.type]][[col.id]][HUC.id, "Habitat_Area"] <- 100 * (sum(native.patches$Shape_Area) / total.area)
      
      results[[dist.type]][[col.id]][HUC.id, "Watershed_Area"] <- total.area
      
      results[[dist.type]][[col.id]][HUC.id, "ECA_Watershed"] <- 100 * (sqrt(sum(matrix.sum))/ total.area)
      
      #
      # Need to implement saving of the relative contributions once we have decided on a final distance value.
      #
      
    }
    
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

################################
# Landscape Connectivity Index #
################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landscape_connectivity <- function(base.network, habitat.avail, study.area, index) {
  
  ################
  # Dependencies #
  ################
  
  require(igraph)
  require(NCmisc)
  
  #####################
  # Check Matrix Size # If less than 10Gb, calculate normally, else use simplified procedure
  #####################
  
  if (estimate.memory(dat = c(nrow(habitat.avail), nrow(habitat.avail)), unit = "gb") > 10) {
    
    # Memory safe version
    matrix.sum <- 0
    
    # Define subsets
    x <- 1
    group.se <- NULL
    while (x <= nrow(habitat.avail)) {
      
      group.se <- rbind(group.se, c(x, x + 999))
      x <- x + 1000
      
    }
    
    # Adjust values that are greater than the number of possible segments 
    group.se[group.se >= nrow(habitat.avail)] <- nrow(habitat.avail) 
    
    # Loop through the subsets
    for (subset.id in 1:nrow(group.se)) {
      
      # Create habitat matrix
      habitat.matrix <- outer(habitat.avail$Native[group.se[subset.id, 1]:group.se[subset.id, 2]], habitat.avail$Native, FUN = "*")
      
      if ("IIC" %in% index) {
        
        # Create pairwise distance matrix based on number of connections
        dist.matrix <- shortest.paths(graph = base.network, v = V(base.network)[group.se[subset.id, 1]:group.se[subset.id, 2]]) + 1
        
      } else {
        
        # Using the network with predefined weights, calculate the shortest path between all patches
        # Assumes that the distances between patches are probabilities converted to the log scale.
        dist.matrix <- distances(graph = base.network, v = V(base.network)[group.se[subset.id, 1]:group.se[subset.id, 2]]) 
        dist.matrix <- exp(dist.matrix * -1) # Convert back to probability
        
      }
      
      matrix.sum <- matrix.sum + sum(habitat.matrix * dist.matrix)
      rm(dist.matrix, habitat.matrix)
      
    }
    
  } else {
    
    # Matrix Version
    
    # Create habitat matrix
    habitat.matrix <- outer(habitat.avail$Native, habitat.avail$Native, FUN = "*")
    
    if ("IIC" %in% index) {
      
      # Create pairwise distance matrix based on number of connections
      dist.matrix <- shortest.paths(base.network) + 1
      
    } else {
      
      # Using the network with predefined weights, calculate the shortest path between all patches
      # Assumes that the distances between patches are probabilities converted to the log scale.
      dist.matrix <- distances(graph = base.network) 
      dist.matrix <- exp(dist.matrix * -1) # Convert back to probability
      
      
    }
    
    matrix.sum <- sum(habitat.matrix * dist.matrix)
    rm(dist.matrix, habitat.matrix)
    
  }
  
  #####################
  # Returning Results #
  #####################
  
  ######
  # PC # 
  ######
  
  results.return <- matrix(data = NA, nrow = 1, ncol = length(index),
                           dimnames = list(c(), c(index)))
  
  if ("PC" %in% index) {
    
    results.return[, "PC"] <- matrix.sum / sum(study.area)^2
    
  }
  
  #######
  # ECA # 
  #######
  
  if ("ECA" %in% index) {
    
    results.return[, "ECA"] <- sqrt(matrix.sum)
    
  }
  
  #######
  # IIC # 
  #######
  
  if ("IIC" %in% index) {
    
    results.return[, "IIC"] <- matrix.sum / sum(study.area)^2
    
  }
  
  return(results.return)
  
}
