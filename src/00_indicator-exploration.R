#
# Title: Exploration of landscape connectivity indicator
# Created: July 12th, 2021
# Last Updated: December 5th, 2022
# Authors: Brandon Allen and Chris Mallon
# Objectives: Explore how the Equivalent Connected Area Index responds to fragmentation, prioritization, dispersal simulation, patch size simulation.
# Keywords: Functions, Fragmentation, Prioritization, Dispersal Distance, Minimum Patch Size
# Notes: 
# 1) Pascual-Hortal & Saura 2006 provided guidance on different ways to assess a connectivity indicator.
# They created a series of tests for the precursor to the Probability of Connectivity (PC) and Equivalent Connected Area 
# (ECA) indicators that we can test. I expect to find the same results, but it will help illustrate what the
# indicator is capturing and how it can be used.
#
# 2) For each analysis, we are considering a simplified view of the landscape.
# 3) Only a single habitat type is considered as we don't need to understand the interactions between habitat classes.
# 4) No resistance is calculated. Resistance is a modified for dispersal distance and can be assumed constant.
# 5) No harvest recovery is included. We are only looking at snapshot of simulated landscapes. Therefore, we don't 
# need to include harvest recovery as it only impacts change over time.
#
#############
# Functions #
#############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

#
# Landscape connectivity
#

# base.network <- igraph network that includes the log transformed probabilities (weights) for each edge
# habitat.avail <- data frame that contains habitat information for each polygon
# study.area <- total area of study boundary including both native and disturbed polygons. Same units as habitat.avail.
# index <- can calculate the Index of Integral Connectivity (IIC), Equivalent Connected Area (ECA) or Probability
# of connectivity (PC) indicators. 

landscape_connectivity_1.0 <- function(base.network, habitat.avail, study.area, index) {
        
        ################
        # Dependencies #
        ################
        
        require(igraph)
        require(NCmisc)
        
        #####################
        # Check Matrix Size # If less than 1Gb, calculate normally, else use simplified proceedure
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

#
# Network visualization
#

# edge.network <- vector of edges 
# conversion <- Identifies if the edge network needs to be converted from character "1-1" to numeric "1 1"

network_visualization <- function(edge.network, conversion) {
        
        if (conversion == TRUE) {
                
                edge.network <- unlist(strsplit(as.character(edge.network), "-")) # Split factor/characters as preparation for the network creation.
                
        }
        
        return(graph(as.numeric(edge.network))) # Create network based on edge IDs
        
}

#################
# Fragmentation #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
# Load libraries
#

library(igraph) # Used to create the network required to calculated connectivity
library(intergraph) # Used for visualization
library(GGally) # Used for visualization
library(ggplot2) # Used for visualization
library(ggpubr) # Used for visualization

# Create data frame to store results
fragmentation.results <- data.frame(Type = 1:7,
                                    Undisturbed = NA,
                                    Disturbed = NA,
                                    Response = NA)

# There are multiple ways that a landscape can become fragmented. A good connectivity indicator should be able to track
# the negative impacts of multiple types of fragmentation

# 1) Loss of unconnected patch

landscape.node <- data.frame(Patch = c(1:4),
                             Native = c(100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-1", "4-4"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                        conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99)) * -1

fragmentation.results[fragmentation.results$Type == 1, "Undisturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:4),
                             Native = c(100, 100, 100, 0))

fragmentation.results[fragmentation.results$Type == 1, "Disturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")

fragmentation.results[fragmentation.results$Type == 1, "Response"] <- sign(fragmentation.results[fragmentation.results$Type == 1, "Disturbed"] - fragmentation.results[fragmentation.results$Type == 1, "Undisturbed"])


scenario.1 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396"),
                     edge.color = "#2D415B")
scenario.1 <- scenario.1 + annotate(geom="text", x=1, y=0, label="1") + theme(axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 10),
                                 panel.border = element_rect(colour = "black", fill=NA, size=1))

# 2) Loss of connected patch and links (central)

landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99)) * -1

fragmentation.results[fragmentation.results$Type == 2, "Undisturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 0, 100))

E(landscape.network)$weight <- log(c(0, 0)) * -1

fragmentation.results[fragmentation.results$Type == 2, "Disturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                  habitat.avail = landscape.node, 
                                                                                                  study.area = 500, 
                                                                                                  index = "ECA")

fragmentation.results[fragmentation.results$Type == 2, "Response"] <- sign(fragmentation.results[fragmentation.results$Type == 2, "Disturbed"] - fragmentation.results[fragmentation.results$Type == 2, "Undisturbed"])

scenario.2 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#E8A396", "#A2A1A7"),
                     edge.color = "#2D415B",
                     edge.lty = c(2, 2))
scenario.2 <- scenario.2 + annotate(geom="text", x=1, y=0, label="2") + theme(axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 10),
                                 panel.border = element_rect(colour = "black", fill=NA, size=1))

# 3) Loss of connected patch and links (edge)

landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99)) * -1

fragmentation.results[fragmentation.results$Type == 3, "Undisturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 0))

E(landscape.network)$weight <- log(c(0.99, 0)) * -1

fragmentation.results[fragmentation.results$Type == 3, "Disturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                  habitat.avail = landscape.node, 
                                                                                                  study.area = 500, 
                                                                                                  index = "ECA")

fragmentation.results[fragmentation.results$Type == 3, "Response"] <- sign(fragmentation.results[fragmentation.results$Type == 3, "Disturbed"] - fragmentation.results[fragmentation.results$Type == 3, "Undisturbed"])

scenario.3 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#E8A396"),
                     edge.color = "#2D415B",
                     edge.lty = c(1, 2))
scenario.3 <- scenario.3 + annotate(geom="text", x=1, y=0, label="3") + theme(axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 10),
                                 panel.border = element_rect(colour = "black", fill=NA, size=1))


# 4) Loss of part of a patch

landscape.node <- data.frame(Patch = c(1:4),
                             Native = c(100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-1", "4-4"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99)) * -1

fragmentation.results[fragmentation.results$Type == 4, "Undisturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:4),
                             Native = c(100, 50, 100, 100))

fragmentation.results[fragmentation.results$Type == 4, "Disturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                  habitat.avail = landscape.node, 
                                                                                                  study.area = 500, 
                                                                                                  index = "ECA")

fragmentation.results[fragmentation.results$Type == 4, "Response"] <- sign(fragmentation.results[fragmentation.results$Type == 4, "Disturbed"] - fragmentation.results[fragmentation.results$Type == 4, "Undisturbed"])

scenario.4 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#96B3AE"),
                     edge.color = "#2D415B")
scenario.4 <- scenario.4 + annotate(geom="text", x=1, y=0, label="4") + theme(axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 10),
                                 panel.border = element_rect(colour = "black", fill=NA, size=1))

# 5) Loss of essential link

landscape.node <- data.frame(Patch = c(1:4),
                             Native = c(100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99)) * -1

fragmentation.results[fragmentation.results$Type == 5, "Undisturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")

E(landscape.network)$weight <- log(c(0.99, 0.99, 0)) * -1

fragmentation.results[fragmentation.results$Type == 5, "Disturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                  habitat.avail = landscape.node, 
                                                                                                  study.area = 500, 
                                                                                                  index = "ECA")

fragmentation.results[fragmentation.results$Type == 5, "Response"] <- sign(fragmentation.results[fragmentation.results$Type == 5, "Disturbed"] - fragmentation.results[fragmentation.results$Type == 5, "Undisturbed"])

scenario.5 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7"),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,2))
scenario.5 <- scenario.5 + annotate(geom="text", x=1, y=0, label="5") + theme(axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 10),
                                 panel.border = element_rect(colour = "black", fill=NA, size=1))

# 6) Loss redundant link

landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-1"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99)) * -1

fragmentation.results[fragmentation.results$Type == 6, "Undisturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")

E(landscape.network)$weight <- log(c(0.99, 0.99, 0)) * -1

fragmentation.results[fragmentation.results$Type == 6, "Disturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                  habitat.avail = landscape.node, 
                                                                                                  study.area = 500, 
                                                                                                  index = "ECA")

fragmentation.results[fragmentation.results$Type == 6, "Response"] <- sign(fragmentation.results[fragmentation.results$Type == 6, "Disturbed"] - fragmentation.results[fragmentation.results$Type == 6, "Undisturbed"])

scenario.6 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7"),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,2))
scenario.6 <- scenario.6 + annotate(geom="text", x=1, y=0, label="6") + theme(axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 10),
                                 panel.border = element_rect(colour = "black", fill=NA, size=1))

# 7) Loss of secondary network

landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-1", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99)) * -1

fragmentation.results[fragmentation.results$Type == 7, "Undisturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 100))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0)) * -1

fragmentation.results[fragmentation.results$Type == 7, "Disturbed"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                  habitat.avail = landscape.node, 
                                                                                                  study.area = 500, 
                                                                                                  index = "ECA")

fragmentation.results[fragmentation.results$Type == 7, "Response"] <- sign(fragmentation.results[fragmentation.results$Type == 7, "Disturbed"] - fragmentation.results[fragmentation.results$Type == 7, "Undisturbed"])

scenario.7 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396", "#E8A396"),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,1,2))
scenario.7 <- scenario.7 + annotate(geom="text", x=1, y=0, label="7") +
        theme(axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 10),
                                 panel.border = element_rect(colour = "black", fill=NA, size=1))

# 8) Fragmentation results

# Format data
fragmentation.results <- data.frame(Scenario = c(1:7, 1:7),
                                    Disturbance = c(rep("Undisturbed", 7), rep("Disturbed", 7)),
                                    ECA = c(fragmentation.results$Undisturbed,
                                            fragmentation.results$Disturbed))

frag.boxplot <- ggplot(data = fragmentation.results, 
                       aes(x = Scenario, y = ECA, fill = Disturbance, col = Disturbance)) +
  geom_point() +
  scale_color_manual(values = c("#D5A394", "#2D415B")) +
  scale_fill_manual(values = c("#D5A394", "#2D415B")) +
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  theme_light() +
  theme(axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
  
png(filename = "results/figures/support/fragmentation-types.png",
    width = 3600,
    height = 3600,
    res = 300)

ggarrange(scenario.1, scenario.2, scenario.3, scenario.4,
          scenario.5, scenario.6, scenario.7, ncol = 2, nrow = 4)

dev.off()

png(filename = "results/figures/support/fragmentation-types-summary.png",
    width = 1600,
    height = 1000,
    res = 300)

print(frag.boxplot)

dev.off()


##################
# Prioritization #
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create data frame for storing results
prioritization.results <- data.frame(Type = 1:7,
                                     ScenarioA = NA,
                                     ScenarioB = NA,
                                     Response = NA)

# A good connectivity indicator should also be able to provide guidance on prioritizing future developments. For all
# scenarios, the indicator should identify large negative impacts to connectivity in scenario B vs A.

# 1) Loss of smaller patch (A) vs bigger (B) patch

landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 1, "ScenarioA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 0, 100))
E(landscape.network)$weight <- log(c(0, 0)) * -1

prioritization.results[prioritization.results$Type == 1, "ScenarioA"] <- prioritization.results[prioritization.results$Type == 1, "ScenarioA"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")

landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 200, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 1, "ScenarioB"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 0, 100))
E(landscape.network)$weight <- log(c(0, 0)) * -1

prioritization.results[prioritization.results$Type == 1, "ScenarioB"] <- prioritization.results[prioritization.results$Type == 1, "ScenarioB"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")

prioritization.results[prioritization.results$Type == 1, "Response"] <- ifelse(prioritization.results[prioritization.results$Type == 1, "ScenarioB"] > prioritization.results[prioritization.results$Type == 1, "ScenarioA"],
                                                                               "B",
                                                                               "A")
# Visual
landscape.node <- data.frame(Patch = c(1:6),
                             Native = c(100, 100, 100, 100, 300, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "4-5", "5-6"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

scenario.1 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#E8A396", "#A2A1A7", "#A2A1A7", "#E8A396", "#A2A1A7"),
                     node.size = c(3,3,3,3,6,3),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,1,1),
                     node.label = c("", "A", "", "", "B", "")) +
    guides(color = FALSE, size = FALSE)
scenario.1 <- scenario.1 + annotate(geom="text", x=1, y=0, label="1") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))


# 2) Loss of central patch with redundant connections (A) or central patch with essential connections (B)

landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-1",
                                       "1-5", "2-5", "3-5", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 2, "ScenarioA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 0))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0, 0, 0, 0)) * -1

prioritization.results[prioritization.results$Type == 2, "ScenarioA"] <- prioritization.results[prioritization.results$Type == 2, "ScenarioA"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")

landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-5", "2-5", "3-5", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 2, "ScenarioB"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 0))
E(landscape.network)$weight <- log(c(0, 0, 0, 0)) * -1

prioritization.results[prioritization.results$Type == 2, "ScenarioB"] <- prioritization.results[prioritization.results$Type == 2, "ScenarioB"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")

prioritization.results[prioritization.results$Type == 2, "Response"] <- ifelse(prioritization.results[prioritization.results$Type == 2, "ScenarioB"] > prioritization.results[prioritization.results$Type == 2, "ScenarioA"],
                                                                               "B",
                                                                               "A")

# Visual
landscape.node <- data.frame(Patch = c(1:10),
                             Native = c(100, 100, 100, 100, 100, 
                                        100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-1",
                                       "1-5", "2-5", "3-5", "4-5",
                                       "6-10", "7-10", "8-10", "9-10"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

scenario.2 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396"),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,1,1,
                                  2,2,2,2,
                                  2,2,2,2),
                     node.label = c("", "", "", "", "A", "", "", "", "", "B"))
scenario.2 <- scenario.2 + annotate(geom="text", x=1, y=0, label="2") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))


# 3) Loss of central patch with redundant connections (A) or peripheral patch with essential connections (B)

landscape.node <- data.frame(Patch = c(1:9),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5", "5-1",
                                       "1-6", "2-6", "3-6", "4-6", "5-6",
                                       "1-7", "7-8", "8-9"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99, 0.99, 0.99, 0.99, 0.99,
                                     0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 3, "ScenarioA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 1000, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:9),
                             Native = c(100, 100, 100, 100, 100, 0, 100, 100, 100))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0, 0, 0, 0, 0,
                                     0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 3, "ScenarioA"] <- prioritization.results[prioritization.results$Type == 3, "ScenarioA"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 1000, 
                                                                                                                                                                             index = "ECA")

landscape.node <- data.frame(Patch = c(1:9),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5", "5-1",
                                       "1-6", "2-6", "3-6", "4-6", "5-6",
                                       "1-7", "7-8", "8-9"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99, 0.99, 0.99, 0.99, 0.99,
                                     0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 3, "ScenarioB"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 1000, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:9),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 0, 100))
E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99, 0.99, 0.99, 0.99, 0.99,
                                     0, 0.99, 0)) * -1

prioritization.results[prioritization.results$Type == 3, "ScenarioB"] <- prioritization.results[prioritization.results$Type == 3, "ScenarioB"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 1000, 
                                                                                                                                                                             index = "ECA")

prioritization.results[prioritization.results$Type == 3, "Response"] <- ifelse(prioritization.results[prioritization.results$Type == 3, "ScenarioB"] > prioritization.results[prioritization.results$Type == 3, "ScenarioA"],
                                                                               "B",
                                                                               "A")

# Visual
landscape.node <- data.frame(Patch = c(1:9),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5", "5-1",
                                       "1-6", "2-6", "3-6", "4-6", "5-6",
                                       "1-7", "7-8", "8-9"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

scenario.3 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396", "#A2A1A7", "#E8A396", "#A2A1A7"),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,1,1,1,
                                  2,2,2,2,2,
                                  1,2,2),
                     node.label = c("", "", "", "", "", "B", "", "A", ""))
scenario.3 <- scenario.3 + annotate(geom="text", x=1, y=0, label="3") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

# 4) Remove patch and links to maximize size of largest network (A) or remove patch and links to minimize size of largest network (B)

landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(200, 100, 200, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 4, "ScenarioA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 1000, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(200, 100, 200, 0, 100))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0, 0)) * -1

prioritization.results[prioritization.results$Type == 4, "ScenarioA"] <- prioritization.results[prioritization.results$Type == 4, "ScenarioA"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 1000, 
                                                                                                                                                                             index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(200, 100, 200, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 4, "ScenarioB"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 1000, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(200, 0, 200, 0, 100))

E(landscape.network)$weight <- log(c(0, 0, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 4, "ScenarioB"] <- prioritization.results[prioritization.results$Type == 4, "ScenarioB"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 1000, 
                                                                                                                                                                             index = "ECA")

prioritization.results[prioritization.results$Type == 4, "Response"] <- ifelse(prioritization.results[prioritization.results$Type == 4, "ScenarioB"] > prioritization.results[prioritization.results$Type == 4, "ScenarioA"],
                                                                               "B",
                                                                               "A")

# Visual
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(200, 100, 200, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

scenario.4 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#E8A396", "#A2A1A7", "#E8A396", "#A2A1A7"),
                     node.size = c(6,3,6,3,3),
                     edge.color = "#2D415B",
                     edge.lty = c(2,2,2,2),
                     node.label = c("", "B", "", "A", "")) + 
    guides(color = FALSE, size = FALSE)
scenario.4 <- scenario.4 + annotate(geom="text", x=1, y=0, label="4") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

# 5) Small increase in topological distance (A), or large increase in topological distance (B) Version 1

landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-1",
                                       "1-5", "2-5", "3-5", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 5, "ScenarioA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 0))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0, 0, 0, 0)) * -1

prioritization.results[prioritization.results$Type == 5, "ScenarioA"] <- prioritization.results[prioritization.results$Type == 5, "ScenarioA"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")


landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4",
                                       "1-5", "2-5", "3-5", "4-5"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 5, "ScenarioB"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:5),
                             Native = c(100, 100, 100, 100, 0))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0, 0, 0, 0)) * -1

prioritization.results[prioritization.results$Type == 5, "ScenarioB"] <- prioritization.results[prioritization.results$Type == 5, "ScenarioB"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")

prioritization.results[prioritization.results$Type == 5, "Response"] <- ifelse(prioritization.results[prioritization.results$Type == 5, "ScenarioB"] > prioritization.results[prioritization.results$Type == 5, "ScenarioA"],
                                                                               "B",
                                                                               "A")


# Visual
landscape.node <- data.frame(Patch = c(1:10),
                             Native = c(100, 100, 100, 100, 100,
                                        100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-1",
                                       "1-5", "2-5", "3-5", "4-5",
                                       "6-7", "7-8", "8-9",
                                       "6-10", "7-10", "8-10", "9-10"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

scenario.5 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396"),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,1,1,
                                  2,2,2,2,
                                  1,1,1,
                                  2,2,2,2),
                     node.label = c("", "", "", "", "A", "", "", "", "", "B"))
scenario.5 <- scenario.5 + annotate(geom="text", x=1, y=0, label="5") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

# 6) Small increase in topological distance (A), or large increase in topological distance (B) Version 2

landscape.node <- data.frame(Patch = c(1:8),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5", "5-6", "6-1",
                                       "1-7", "2-7", "3-7", "4-7", "5-7", "6-7",
                                       "2-8"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99)) * -1

prioritization.results[prioritization.results$Type == 6, "ScenarioA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 1000, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:8),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 0))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0)) * -1

prioritization.results[prioritization.results$Type == 6, "ScenarioA"] <- prioritization.results[prioritization.results$Type == 6, "ScenarioA"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 1000, 
                                                                                                                                                                             index = "ECA")


landscape.node <- data.frame(Patch = c(1:8),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5", "5-6", "6-1",
                                       "1-7", "2-7", "3-7", "4-7", "5-7", "6-7",
                                       "2-8"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0.99)) * -1

prioritization.results[prioritization.results$Type == 6, "ScenarioB"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 1000, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:8),
                             Native = c(100, 100, 100, 100, 100, 100, 0, 100))

E(landscape.network)$weight <- log(c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 
                                     0, 0, 0, 0, 0, 0, 
                                     0.99)) * -1

prioritization.results[prioritization.results$Type == 6, "ScenarioB"] <- prioritization.results[prioritization.results$Type == 6, "ScenarioB"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 1000, 
                                                                                                                                                                             index = "ECA")

prioritization.results[prioritization.results$Type == 6, "Response"] <- ifelse(prioritization.results[prioritization.results$Type == 6, "ScenarioB"] > prioritization.results[prioritization.results$Type == 6, "ScenarioA"],
                                                                               "B",
                                                                               "A")

# Visual
landscape.node <- data.frame(Patch = c(1:8),
                             Native = c(100, 100, 100, 100, 100, 100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3", "3-4", "4-5", "5-6", "6-1",
                                       "1-7", "2-7", "3-7", "4-7", "5-7", "6-7",
                                       "2-8"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

scenario.6 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#A2A1A7", "#E8A396", "#E8A396"),
                     edge.color = "#2D415B",
                     edge.lty = c(1,1,1,1,1,1,
                                  2,2,2,2,2,2,2),
                     node.label = c("", "", "", "", "", "", "B", "A"))
scenario.6 <- scenario.6 + annotate(geom="text", x=1, y=0, label="6") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

# 7) Loss of links (A) vs loss of habitat and links (B)

landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 7, "ScenarioA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

E(landscape.network)$weight <- log(c(0, 0.99)) * -1

prioritization.results[prioritization.results$Type == 7, "ScenarioA"] <- prioritization.results[prioritization.results$Type == 7, "ScenarioA"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")


landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

E(landscape.network)$weight <- log(c(0.99, 0.99)) * -1

prioritization.results[prioritization.results$Type == 7, "ScenarioB"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                    habitat.avail = landscape.node, 
                                                                                                    study.area = 500, 
                                                                                                    index = "ECA")
landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 0))

E(landscape.network)$weight <- log(c(0.99, 0)) * -1

prioritization.results[prioritization.results$Type == 7, "ScenarioB"] <- prioritization.results[prioritization.results$Type == 7, "ScenarioB"]  - landscape_connectivity_1.0(base.network = landscape.network,
                                                                                                                                                                             habitat.avail = landscape.node, 
                                                                                                                                                                             study.area = 500, 
                                                                                                                                                                             index = "ECA")

prioritization.results[prioritization.results$Type == 7, "Response"] <- ifelse(prioritization.results[prioritization.results$Type == 7, "ScenarioB"] > prioritization.results[prioritization.results$Type == 7, "ScenarioA"],
                                                                               "B",
                                                                               "A")

# Visual
landscape.node <- data.frame(Patch = c(1:3),
                             Native = c(100, 100, 100))

landscape.edges <- data.frame(Node = c("1-2", "2-3"))

landscape.network <- network_visualization(edge.network = landscape.edges$Node, 
                                           conversion = TRUE) 

scenario.7 <- ggnet2(landscape.network, size = 6, 
                     color = c("#A2A1A7", "#A2A1A7", "#E8A396"),
                     edge.color = "#2D415B",
                     edge.lty = c(2,2),
                     node.label = c("", "", "B"),
                     edge.label = c("A", ""))
scenario.7 <- scenario.7 + annotate(geom="text", x=1, y=0, label="6") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

# 8) Prioritization results

# Format data
prioritization.results <- data.frame(Scenario = c(1:7, 1:7),
                                    Option = c(rep("Option A", 7), rep("Option B", 7)),
                                    ECA = -1*c(prioritization.results$ScenarioA,
                                            prioritization.results$ScenarioB))

# Remove the duplicate prioritization scenario
prioritization.results <- prioritization.results[prioritization.results$Scenario != 6, ]
prioritization.results$Scenario[prioritization.results$Scenario == 7] <- 6

prioritization.boxplot <- ggplot(data = prioritization.results, 
                                 aes(x = Scenario, y = ECA, fill = Option, col = Option)) +
  geom_point() +
  scale_color_manual(values = c("#D5A394", "#2D415B")) +
  scale_fill_manual(values = c("#D5A394", "#2D415B")) +
  scale_x_continuous(breaks=seq(1, 6, 1)) +
  theme_light() +
  theme(axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

png(filename = "results/figures/support/prioritization-types.png",
    width = 3600,
    height = 3600,
    res = 300)

ggarrange(scenario.1, scenario.2, scenario.3, scenario.4,
          scenario.5, scenario.7, ncol = 2, nrow = 3)

dev.off()

png(filename = "results/figures/support/prioritization-types-summary.png",
    width = 1600,
    height = 1000,
    res = 300)

print(prioritization.boxplot)

dev.off()

########################
# Dispersal simulation #
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 
# Load libraries
# 

library(igraph)
library(landscapemetrics)
library(landscapetools)
library(NLMR)
library(raster)
library(sf)

#
# Set seed
#

set.seed(1465)

#########################
# Single landcover type # Rename fragmentation to percent disturbance
#########################

simulation.results <- data.frame(Iteration = 1:100,
                                 Disturbance = rep(c(0.05, seq(0.1,0.9,0.1)), 10),
                                 Dispersal = c(rep(100, 10),
                                               rep(150, 10),
                                               rep(200, 10),
                                               rep(250, 10),
                                               rep(500, 10),
                                               rep(750, 10),
                                               rep(1000, 10),
                                               rep(5000, 10),
                                               rep(10000, 10),
                                               rep(100000, 10)),
                                 ECA = NA,
                                 PECA = NA,
                                 NC = NA,
                                 PNC = NA)


#
# Create simulated landscape
#

raster.row <- 100
raster.col <- 100

# Define disturbance, dispersal, and resistance parameters
disturbance <- c(0.05, seq(0.1,0.9,0.1))
dispersal <- c(100, 150, 200, 250, 500, 750, 1000, 5000, 10000, 100000)
resistance <- 1 # Mean resistance value

for(disturb.id in disturbance) {
    
    # Create base landscape
    
    landscape.matrix <- matrix(ncol = raster.row, nrow = raster.col, data = 1)
    
    # Remove rows and columns until we reach the desired disturbance threshold
    row.sample <- sample(1:100, 100, replace=FALSE)
    col.sample <- sample(1:100, 100, replace=FALSE)
    x <- 1
    disturb.amount <- 0
    
    while(disturb.amount < disturb.id) {
      
      landscape.matrix[row.sample[x], ] <- 0
      landscape.matrix[, col.sample[x]] <- 0
      
      # Update disturbance amount
      disturb.amount <- table(landscape.matrix)[1] / sum(table(landscape.matrix))
      
      # Update tracker
      x <- x + 1
      
    }
    
    # Create raster
    landscape.raster <- raster(landscape.matrix)
    extent(landscape.raster) <- c(0,10000,0,10000)
    res(landscape.raster) <- c(100,100)
    
    # Extract patches
    landscape.classified <- get_patches(landscape.raster, class = 1, directions = 4)
    landscape.classified <- landscape.classified$layer_1$class_1
    
    # Convert to polygon
    landscape.polygon <- rasterToPolygons(landscape.classified)
    dissolved.polygon <- aggregate(landscape.polygon, by = "layer")
    dissolved.polygon <- st_as_sf(dissolved.polygon)
    
    poly.dist <- st_distance(x = dissolved.polygon, y = dissolved.polygon)
    rownames(poly.dist) <- dissolved.polygon$layer
    colnames(poly.dist) <- dissolved.polygon$layer
    
    for(disp.id in dispersal) {
        
        disperse <- log(0.05) / disp.id # 5% dispersal success at maximum dispersal
        
        # Convert distance matrix to data frame
        network.data <- data.frame(matrix(ncol = 3, nrow = ncol(poly.dist)*nrow(poly.dist)))
        colnames(network.data) <- c("Patch_A", "Patch_B", "Dist")
        
        row.start <- 1
        row.end <- nrow(poly.dist)
        
        for(x in 1:ncol(poly.dist)) {
            
            network.data[row.start:row.end, "Patch_A"] <- rep(colnames(poly.dist)[x], nrow(poly.dist))
            network.data[row.start:row.end, "Patch_B"] <- rownames(poly.dist)
            network.data[row.start:row.end, "Dist"] <- poly.dist[, x]
            
            row.start <- row.start + nrow(poly.dist)
            row.end <- row.end + nrow(poly.dist)
            
        }
        
        # Remove connections with more than double maximum distance
        network.data <- network.data[network.data$Dist <= disp.id * 2, ]
        
        # Create network
        landscape.network <- graph_from_data_frame(network.data[, 1:2])
        
        # Calculate weights and convert to log scale
        E(landscape.network)$weight <- log(exp(as.numeric(disperse * network.data$Dist * resistance))) * -1
        
        # Create habitat information
        landscape.node <- data.frame(Patch = dissolved.polygon$layer,
                                     Native = st_area(dissolved.polygon))
        
        # Calculate metrics
        total.area <- raster.row * raster.col * 100 * 100
        row.index <- simulation.results$Disturbance == disturb.id & simulation.results$Dispersal == disp.id
        
        simulation.results[row.index, "ECA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                    habitat.avail = landscape.node, 
                                                                    study.area = total.area, 
                                                                    index = "ECA")
        
        simulation.results[row.index, "PECA"] <- (simulation.results[row.index, "ECA"] / total.area) * 100
        simulation.results[row.index, "NC"] <- sum(landscape.node$Native)
        simulation.results[row.index, "PNC"] <- (sum(landscape.node$Native) / total.area) * 100
        
        # print iteration
        print(disturb.id)
        
    }
    
}

save(simulation.results, file = "results/tables/support/dispersal-simulation-results_2022-12-05.Rdata")

# Visualization
simulation.results$Disturbance <- factor(simulation.results$Disturbance)
simulation.results$Dispersal <- factor(simulation.results$Dispersal)
simulation.results$ECA <- simulation.results$ECA / 1000000 # Convert to km
simulation.results$NC <- simulation.results$NC / 1000000 # Convert to km

eca.disp <- ggplot(data = simulation.results, aes(x = ECA, y = NC, group = Dispersal, color = Dispersal)) +
    geom_point() +
    geom_line() +
    xlim(0,100) +
    ylim(0,100) +
    scale_color_manual("Dispersal (m)",
                     values = c("#C969A1", "#CD484B", 
                                "#E6766A", "#EB7D41",
                                "#F69D36", "#C8AC55",
                                "#79987B", "#4C838D",
                                "#014A61", "#122451")) +
    xlab("Equivalent Connected Area (%)") +
    ylab("Native Cover (%)") +
    theme_light() +
    theme(axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1))


png(filename = "results/figures/support/dispersal-landscape-simulation.png",
    width = 1800,
    height = 1800,
    res = 300)

print(eca.disp)

dev.off()

#########################
# Patch size simulation #
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 
# Load libraries
# 

library(igraph)
library(landscapemetrics)
library(landscapetools)
library(NLMR)
library(raster)
library(sf)

#
# Set seed
#

set.seed(1465)

#########################
# Single landcover type # Rename fragmentation to percent disturbance
#########################

simulation.results <- data.frame(Iteration = 1:80,
                                 Disturbance = rep(c(0.05, seq(0.1,0.9,0.1)), 8),
                                 PatchSize = c(rep(10000, 10),
                                               rep(50000, 10),
                                               rep(100000, 10),
                                               rep(500000, 10),
                                               rep(1000000, 10),
                                               rep(5000000, 10),
                                               rep(10000000, 10),
                                               rep(50000000, 10)),
                                 ECA = NA,
                                 PECA = NA,
                                 NC = NA,
                                 PNC = NA)


#
# Create simulated landscape ()
#

raster.row <- 100
raster.col <- 100

# Define disturbance, dispersal, and resistance parameters
disturbance <- c(0.05, seq(0.1,0.9,0.1))
patch.size <- unique(simulation.results$PatchSize)
disp.id <- 250 # Based on our dispersal distance
resistance <- 1 # Mean resistance value

for(disturb.id in disturbance) {
  
  # Create base landscape
  landscape.matrix <- matrix(ncol = raster.row, nrow = raster.col, data = 1)
  
  # Remove rows and columns until we reach the desired disturbance threshold
  row.sample <- sample(1:100, 100, replace=FALSE)
  col.sample <- sample(1:100, 100, replace=FALSE)
  x <- 1
  disturb.amount <- 0
  
  while(disturb.amount < disturb.id) {
    
    landscape.matrix[row.sample[x], ] <- 0
    landscape.matrix[, col.sample[x]] <- 0
    
    # Update disturbance amount
    disturb.amount <- table(landscape.matrix)[1] / sum(table(landscape.matrix))
    
    # Update tracker
    x <- x + 1
    
  }
  
  # Create raster
  landscape.raster <- raster(landscape.matrix)
  extent(landscape.raster) <- c(0,10000,0,10000)
  res(landscape.raster) <- c(100,100)
  
  # Extract patches
  landscape.classified <- get_patches(landscape.raster, class = 1, directions = 4)
  landscape.classified <- landscape.classified$layer_1$class_1
  
  # Convert to polygon
  landscape.polygon <- rasterToPolygons(landscape.classified)
  landscape.polygon <- aggregate(landscape.polygon, by = "layer")
  landscape.polygon <- st_as_sf(landscape.polygon)
  
  for(patch in patch.size) {
    
    # Determine row index and total area
    total.area <- raster.row * raster.col * 1000 * 1000
    row.index <- simulation.results$Disturbance == disturb.id & simulation.results$PatchSize == patch
    
    # Filter out patches less than the patch size
    dissolved.polygon <- landscape.polygon[st_area(landscape.polygon) >= patch, ]
    
    # If there is one patch left, ECA equals the size of that habitat patch
    if(nrow(dissolved.polygon) == 1) {
    
      simulation.results[row.index, "ECA"] <- as.numeric(st_area(dissolved.polygon))
      simulation.results[row.index, "PECA"] <- (as.numeric(st_area(dissolved.polygon)) / total.area) * 100
      simulation.results[row.index, "NC"] <- sum(as.numeric(st_area(landscape.polygon)))
      simulation.results[row.index, "PNC"] <- (sum(as.numeric(st_area(landscape.polygon))) / total.area) * 100
      
      next()
      
    }
    
    # If there are no patches, ECA equals 0 
    if(nrow(dissolved.polygon) == 0) {
      
      simulation.results[row.index, "ECA"] <- 0
      simulation.results[row.index, "PECA"] <- 0
      simulation.results[row.index, "NC"] <- sum(as.numeric(st_area(landscape.polygon)))
      simulation.results[row.index, "PNC"] <- (sum(as.numeric(st_area(landscape.polygon))) / total.area) * 100
      
      next()
      
    }
    
    # Perform the rest of the calculations
    poly.dist <- st_distance(x = dissolved.polygon, y = dissolved.polygon)
    rownames(poly.dist) <- dissolved.polygon$layer
    colnames(poly.dist) <- dissolved.polygon$layer
    
    disperse <- log(0.05) / disp.id # 5% dispersal success at maximum dispersal
    
    # Convert distance matrix to data frame
    network.data <- data.frame(matrix(ncol = 3, nrow = ncol(poly.dist)*nrow(poly.dist)))
    colnames(network.data) <- c("Patch_A", "Patch_B", "Dist")
    
    row.start <- 1
    row.end <- nrow(poly.dist)
    
    for(x in 1:ncol(poly.dist)) {
      
      network.data[row.start:row.end, "Patch_A"] <- rep(colnames(poly.dist)[x], nrow(poly.dist))
      network.data[row.start:row.end, "Patch_B"] <- rownames(poly.dist)
      network.data[row.start:row.end, "Dist"] <- poly.dist[, x]
      
      row.start <- row.start + nrow(poly.dist)
      row.end <- row.end + nrow(poly.dist)
      
    }
    
    # Remove connections with more than double maximum distance
    network.data <- network.data[network.data$Dist <= disp.id * 2, ]
    
    # Create network
    landscape.network <- graph_from_data_frame(network.data[, 1:2])
    
    # Calculate weights and convert to log scale
    E(landscape.network)$weight <- log(exp(as.numeric(disperse * network.data$Dist * resistance))) * -1
    
    # Create habitat information
    landscape.node <- data.frame(Patch = dissolved.polygon$layer,
                                 Native = st_area(dissolved.polygon))
    
    # Calculate metrics
    total.area <- raster.row * raster.col * 100 * 100

    simulation.results[row.index, "ECA"] <- landscape_connectivity_1.0(base.network = landscape.network,
                                                                       habitat.avail = landscape.node, 
                                                                       study.area = total.area, 
                                                                       index = "ECA")
    
    simulation.results[row.index, "PECA"] <- (simulation.results[row.index, "ECA"] / total.area) * 100
    simulation.results[row.index, "NC"] <- sum(as.numeric(st_area(landscape.polygon)))
    simulation.results[row.index, "PNC"] <- (sum(as.numeric(st_area(landscape.polygon))) / total.area) * 100
    
    # print iteration
    print(disturb.id)
    
  }
  
}

save(simulation.results, file = "results/tables/support/patch-size-simulation-results_2022-12-05.Rdata")

# Visualization
simulation.results$Disturbance <- factor(simulation.results$Disturbance * 100)
simulation.results$PatchSize <- simulation.results$PatchSize / 1000000
simulation.results$PatchSize <- factor(simulation.results$PatchSize)
simulation.results$ECA <- simulation.results$ECA / 1000000 # Convert to km
simulation.results$NC <- simulation.results$NC / 1000000 # Convert to km

eca.patch <- ggplot(data = simulation.results, aes(x = PatchSize, y = ECA, group = Disturbance, color = Disturbance)) +
  geom_point() +
  geom_line() +
  scale_color_manual("Disturbance (%)",
                    values = c("#C969A1", "#CD484B", 
                               "#E6766A", "#EB7D41",
                               "#F69D36", "#C8AC55",
                               "#79987B", "#4C838D",
                               "#014A61", "#122451")) +
  xlab("Minimum Patch Size (km2)") +
  ylab("Equivalent Connected Area") +
  theme_light() +
  theme(axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

png(filename = "results/figures/support/patch-size-landscape-simulation.png",
    width = 1800,
    height = 1800,
    res = 300)

print(eca.patch)

dev.off()