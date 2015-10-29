source("models/feat_type__accuracy_for_plot.R")

# check global flag
if (!exists("SAVE_AS")) {
  SAVE_AS <- TRUE
}


library(AICcmodavg)
library(lme4)
library(dplyr)

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

icon_top <- -0.125

left_pos_x <- -0.5
right_pos_x <- 0.5

left_pos <- get_pos(left_pos_x)
right_pos <- get_pos(right_pos_x)


# ------------------------------------------------------------------------------
# Load color scheme.
source("plots/colorscheme.R")

# ------------------------------------------------------------------------------
# Load ggplot base call
source("plots/errorbar.R")

grid.draw(errorbars())

if (SAVE_AS == TRUE) {
  png("plots/question_first/feat_type__accuracy.png", width = 6, height = 6, units = "in", res = 200)
  grid.draw(errorbars())
  dev.off()
}