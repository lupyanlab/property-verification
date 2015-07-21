library(ggplot2)
library(scales)

errorbars <- function() {
  bar_dodge <- position_dodge(width = 0.6)
  
  error_plot <- ggplot(error_rates, aes(x = feat_c, y = rate, group = mask_type)) +
    geom_bar(aes(fill = fill), stat = "identity", 
             position = bar_dodge, width = 0.6, alpha = 0.4) +
    geom_linerange(aes(ymin = rate - se, ymax = rate + se), 
                   position = bar_dodge, color = error_bar_color) +
    scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("Nonvisual Question", "Visual Question")) +
    scale_y_continuous("Error Rate", breaks = seq(0, 0.14, by = 0.02), labels = percent) +
    scale_fill_manual(values = unlist(color_scheme)) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(0, 0.15)) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      #       legend.title.align = 0.5,
      plot.margin = unit(c(0.1, 0.1, 0.5, 0.5), "lines"),
      axis.ticks.x = element_blank(),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    ) +
    annotation_custom(grob_icon("blank"), 
                      xmin = left_pos$left_icon$xmin, 
                      xmax = left_pos$left_icon$xmax, 
                      ymin = icon_top
    ) +
    annotation_custom(grob_icon("interference"), 
                      xmin = left_pos$right_icon$xmin, 
                      xmax = left_pos$right_icon$xmax, 
                      ymin = icon_top
    ) +
    annotation_custom(grob_icon("blank"), 
                      xmin = right_pos$left_icon$xmin, 
                      xmax = right_pos$left_icon$xmax, 
                      ymin = icon_top
    ) +
    annotation_custom(grob_icon("interference"), 
                      xmin = right_pos$right_icon$xmin, 
                      xmax = right_pos$right_icon$xmax, 
                      ymin = icon_top
    )
  
  error_gt <- turn_off_clipping(error_plot)
  error_gt
}