#
# Title: Landscape connectivity index
# Created: Sept 14th, 2020
# Last Updated: June 14th, 2022
# Author: Brandon Allen
# Objectives: Functions required to calculate landscape connectivity
# Keywords: Landscape Connectivity, Network Visualization
#

#
# NOTE: Double check that fix for vertice alignment is correct
#

##########################
# Landscape Connectivity #
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landscape_connectivity_1.0 <- function(base.network, habitat.avail, study.area, index) {
        
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

#########################
# Network Visualization #
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_visualization <- function(edge.network, conversion) {
        
        if (conversion == TRUE) {
                
                edge.network <- unlist(strsplit(as.character(edge.network), "-")) # Split factor/characters as preparation for the network creation.
                
        }
        
        return(graph(as.numeric(edge.network))) # Create network based on edge IDs
        
}
