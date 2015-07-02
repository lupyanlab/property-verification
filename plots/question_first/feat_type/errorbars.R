library(AICcmodavg)
library(lme4)
library(dplyr)

source("models/question_first/feat_type/accuracy.R")

dv_columns <- c("feat_type", "feat_c", "mask_type", "mask_c")
points <- question_first[, dv_columns] %>% unique()
error_rates <- predictSE(feat_type_error_mod, points, se.fit = TRUE, type = "response") %>%
  as.data.frame() %>% select(rate = fit, se = se.fit) %>% 
  cbind(points, .)

# ------------------------------------------------------------------------------
# Adjust for plot
error_rates <- error_rates %>% mutate(
  mask_type = factor(mask_type, levels = c("nomask", "mask")),
  fill = paste0(feat_type, mask_type)
)

# ------------------------------------------------------------------------------
# Create grobs from images to use as axis labels.

source("plots/axis-icons.R")

get_icon <- function(x) {
  icon_width <- 0.2
  list(xmin = x - icon_width/2, 
       xmax = x + icon_width/2)
}

get_pos <- function(x) {
  icon_gutter <- 0.15  # bar width
  
  list(left_icon = get_icon(x - icon_gutter),
       right_icon = get_icon(x + icon_gutter))
}

icon_top <- -0.13

left_pos_x <- -0.5
right_pos_x <- 0.5

left_pos <- get_pos(left_pos_x)
right_pos <- get_pos(right_pos_x)


# ------------------------------------------------------------------------------
# Load color scheme.
source("plots/colorscheme.R")

# ------------------------------------------------------------------------------

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

grid.draw(errorbars())

png("plots/question_first/feat_type/errorbars.png", width = 6, height = 6, units = "in", res = 200)
grid.draw(errorbars())
dev.off()
