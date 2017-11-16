# a function for exploring the data

# blank theme for plots
blank_theme <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 8))

# theme to remove axis
no_axis <- theme(axis.line = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title.y = element_blank())

plotRawImage <- function(image_df, angle){
  # Plots the raw images, given the AN-camera angle.
  #
  # Arguments: image_df, the dataframe with the image data
  # Returns: ggplot object with image
  
  gg_image <- ggplot(image_df, aes(x = x, y = y)) +
              geom_point(aes_string(color = angle)) +
              blank_theme + no_axis + xlab("")
  return(gg_image)
}

plotClasses <- function(image_df) {
  # Plots the expert classifications for a given image.
  # Arguments:
  #   image_df: the dataframe with the image data
  # Returns:
  #   ggplot object with image
  
  
  # Create plot object
  gg_image <- ggplot(image_df) +
              geom_point(aes(x = x, y = y, color = factor(label)),
                         alpha = 0.1) +
              scale_color_discrete(name = "Expert label",
                                   labels = c("Ice", "Unknown", "Cloud"),
                                   guide = guide_legend(override.aes = 
                                                  list(alpha = 0.8))) +
              blank_theme + no_axis + xlab("")
  
  return(gg_image)
}

plot_conditional_densities <- function(image_df, dimension) {
  # Plots the densities of ice/cloud/unknown densities based on
  # the values of the "dimension" parameter.
  # Arguments:
  #   image_df: the dataframe with the image data
  #   dimension: the value of the dataframe to condition on
  # Returns:
  #   ggplot object with image showing conditional densities
  
  gg_image <- ggplot(image_df, aes(group = factor(label),
                                 fill = factor(label))) + 
  geom_density(aes_string(x = dimension), alpha = 0.5) +
  scale_fill_discrete(name = "Expert label",
                      labels = c("Ice", "Unknown", "Cloud")) +
  blank_theme

return(gg_image)
}

plot_feature_vs_feature <- function(feature1, feature2) {
  # Plots a scatterplot of the values of one feature versus another
  # in the combined image data, colored by the expert label.
  # Arguments:
  #   feature1: the name of the first feature to plot
  #   feature2: the name of the first feature to plot
  # Returns:
  #   ggplot object with scatterplot of the two feature values
  
  # combine the dataset and then pass it into ggplot as a sampled df
  combined_data <- rbind(image1, image2, image3)
  
  # plot some combined data to confirm the relationship across images
  gg_image <- ggplot(combined_data[sample(nrow(combined_data), 1000),],
                     aes(color = factor(label))) +
    geom_point(aes_string(x = feature1, y = feature2), alpha=0.5) +
    # add legend descriptions
    scale_color_discrete(name = "Expert label",
                         labels = c("Ice", "Unknown", "Cloud")) +
    theme_bw() +
    no_axis +
    # make the title and axes labels smaller
    theme(panel.border = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size=7),
          legend.text = element_text(size=7),
          plot.title = element_text(size=7),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7))
  
  return(gg_image)
}


grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right")) {
  # Function to share a legend between multiple plots
  # using grid.arrange. Taken from:
  # https://github.com/tidyverse/ggplot2/wiki/
  # share-a-legend-between-two-ggplot2-graphs
  library(grid)
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc")
                                                             - lheight,
                                                             lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") -
                                                             lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}