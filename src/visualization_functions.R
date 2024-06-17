#
# Title: Functions for visualizing landscape connectivity
# Created: October 31st, 2023
# Last Updated: April 30th, 2024
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
                scale_fill_gradientn(name = paste0("Landscape \nConnectivity (%)"), 
                                     colors = met.brewer(name = "Hiroshige", n = 100, 
                                                         type = "continuous"), 
                                     guide = "colourbar") +
                ggtitle(title) + 
                theme_light() +
                theme(axis.title = element_text(size=14),
                      axis.text.x = element_text(size=14),
                      axis.text.y = element_text(size=14),
                      title = element_text(size=14), 
                      legend.title = element_text(size=14),
                      legend.text = element_text(size=12),
                      legend.key.size = unit(0.5, "cm"),
                      legend.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      legend.position = c(0.20, 0.15)) 
}

##############
# Difference # 
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

difference_plot <- function(data.in, habitat, title) {
        
        # Create the default palette
        centered.colors <- met.brewer(name = "Hiroshige", 
                                      n = 10, 
                                      type = "continuous")
        
        # Define the min and maximum values
        max.value <- max(as.numeric(as.data.frame(data.in)[, habitat]), na.rm = TRUE)
        min.value <- min(as.numeric(as.data.frame(data.in)[, habitat]), na.rm = TRUE)
        
        # Round up to the nearest five for the maximum
        max.value <- 5*ceiling(max.value/5)
        min.value <- (5*ceiling(abs(min.value)/5)) * -1
        
        above.0 <- colorRampPalette(centered.colors[6:10])(max.value)
        below.0 <- colorRampPalette(centered.colors[1:5])(min.value * -1)
        
        # Create the final centered palette
        centered.colors <- c(below.0, 
                             "#FFFFFF", 
                             above.0)

        # Visualize the plot
        ggplot() + 
                geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
                scale_fill_gradientn(name = paste0("Connectivity Change (%)"), 
                                     colors = centered.colors, 
                                     guide = "colourbar", 
                                     limits = c(min.value, max.value),
                                     breaks = c(rev(seq(0, min.value, -10)), max.value),
                                     labels = c(rev(seq(0, min.value, -10)), max.value)) +
                ggtitle(title) + 
                theme_light() +
                theme(axis.title = element_text(size=14),
                      axis.text.x = element_text(size=14),
                      axis.text.y = element_text(size=14),
                      title = element_text(size=14), 
                      legend.title = element_text(size=14),
                      legend.text = element_text(size=12),
                      legend.key.size = unit(0.5, "cm"),
                      legend.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      legend.position = c(0.20, 0.15)) 
}

########
# Area # 
########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

area_plot <- function(data.in, habitat, title) {
  
  ggplot() + 
    geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
    scale_fill_gradientn(name = paste0("Percent Area (%)"), 
                         colors = met.brewer(name = "VanGogh3", n = 100, type = "continuous"), 
                         guide = "colourbar", 
                         limits = c(0, 100)) +
    ggtitle(title) + 
    theme_light() +
    theme(axis.title = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          title = element_text(size=14), 
          legend.title = element_text(size=14),
          legend.text = element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.background = element_blank(),
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
                theme(axis.title = element_text(size=14))
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
    theme(axis.title = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          title = element_text(size=14), 
          legend.title = element_text(size=14),
          legend.text = element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.20, 0.15)) 
  
}
