library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

linegraph <- function(values, means, x_label, 
                     icon_interference_y, icon_blank_y,
                     icon_x,
                     color_scheme) {
  icon_x_left <- icon_x
  icon_x_right <- icon_x_left + icon_width
  ggplot(values, aes(x = rating)) +
    geom_smooth(aes(y = rate, ymin = rate - se, ymax = rate + se, 
                    group = mask_type, fill = mask_type, color = mask_type),
                stat = "identity", alpha = 0.4) +
    geom_point(aes(y = rate, shape = mask_type, color = mask_type, size = obs), 
               data = means) +
    scale_x_continuous(x_label, 
                       breaks = 0:3, labels = c("None", "Little", "Some", "A lot")) +
    scale_y_continuous("Error Rate", breaks = seq(0.0, 0.4, by = 0.05), labels = percent) +
    scale_color_manual(values = unlist(color_scheme)) +
    scale_fill_manual(values = unlist(color_scheme)) +
    scale_shape_manual(values = c(1,16)) +
    scale_size_continuous(range = c(2,4)) +
    coord_cartesian(xlim = c(-0.2, 3.6), ylim = c(0, 0.30)) +
    #facet_wrap(~ type, nrow = 2) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      plot.margin = unit(c(0.1, 2.5, 0.5, 0.5), "lines"),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank()
    ) + 
    annotation_custom(grob_icon("blank"), 
                      xmin = icon_x_left, 
                      xmax = icon_x_right, 
                      ymin = icon_blank_y) + 
    annotation_custom(grob_icon("interference"), 
                      xmin = icon_x_left, 
                      xmax = icon_x_right, 
                      ymin = icon_interference_y)  
}
