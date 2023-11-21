#
# Title: Functions for visualizing landscape connectivity
# Created: October 31st, 2023
# Last Updated: November 20th, 2023
# Author: Brandon Allen
# Objectives: Define functions for landscape connectivity
# Keywords: Landscape Connectivity, Difference, Trend, Resistance
#


##########################
# Landscape Connectivity # 
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lc_plot <- function(data.in, habitat, title) {
        
        ggplot() + 
                geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
                scale_fill_gradientn(name = paste0("Landscape \nConnectivity (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
                ggtitle(title) + 
                theme_light() +
                theme_abmi(font = "Montserrat") +
                theme(axis.title = element_text(size=14),
                      axis.text.x = element_text(size=14),
                      axis.text.y = element_text(size=14),
                      title = element_text(size=14), 
                      legend.title = element_text(size=14),
                      legend.text = element_text(size=14),
                      legend.key.size = unit(0.5, "cm"),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      legend.position = c(0.20, 0.15)) 
}

##############
# Difference # 
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

difference_plot <- function(data.in, habitat, title) {
        
        # Define the max value so everything is center properly
        max.value <- max(abs(as.numeric(as.data.frame(data.in)[, habitat])), na.rm = TRUE)
        
        ggplot() + 
                geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
                scale_fill_gradientn(name = paste0("Percent Change (%)"), 
                                     colors = met.brewer(name = "Cassatt2", n = 100, type = "continuous"), 
                                     guide = "colourbar", 
                                     limits = c(-1 * max.value, max.value)) +
                ggtitle(title) + 
                theme_light() +
                theme_abmi(font = "Montserrat") +
                theme(axis.title = element_text(size=14),
                      axis.text.x = element_text(size=14),
                      axis.text.y = element_text(size=14),
                      title = element_text(size=14), 
                      legend.title = element_text(size=14),
                      legend.text = element_text(size=14),
                      legend.key.size = unit(0.5, "cm"),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      legend.position = c(0.20, 0.15)) 
}

#########
# Trend # 
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trend_plot <- function(data.in, x, y, title) {
        
        ggplot(data = data.in, aes_string(x = paste0("Connect", x), y = paste0("Connect", y))) + 
                geom_point() +
                ggtitle(title) + 
                geom_abline(slope = 1) +
                ylim(c(0,100)) +
                xlim(c(0,100)) +
                xlab(paste0("Landscape Connectivity ", x, " (%)")) +
                ylab(paste0("Landscape Connectivity ", y, " (%)")) +
                theme_light() +
                theme(axis.title = element_text(size=14)) +
                theme_abmi(font = "Montserrat")
}

##############
# Resistance # 
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

resistance_plot <- function(data.in, habitat, legend, title) {
  
  ggplot() + 
    geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
    scale_fill_gradientn(name = paste0(legend), 
                         colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), 
                         guide = "colourbar") +
    ggtitle(title) + 
    theme_light() +
    theme_abmi(font = "Montserrat") +
    theme(axis.title = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          title = element_text(size=14), 
          legend.title = element_text(size=14),
          legend.text = element_text(size=14),
          legend.key.size = unit(0.5, "cm"),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.20, 0.15)) 
  
}
