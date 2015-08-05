
# Subplot A: Trial structure
# --------------------------
library(gridExtra)
library(png)

png_to_grob <- function(png_image, alpha = 0.2) {
  img <- readPNG(png_image, info = TRUE)
  img_alpha <- matrix(rgb(img[,,1], img[,,2], img[,,3], alpha), nrow=dim(img)[1])
  img_grob <- rasterGrob(img_alpha, interpolate = TRUE)
  img_grob
}

trialstructure <- png_to_grob("plots/cue_first/trial_structure.png", alpha = 1.0)


# Subplot B: Feature type by mask
# -------------------------------
SAVE_AS <- FALSE
source("plots/cue_first/feat_type__accuracy.R")
error_gt <- errorbars()

# Subplots C,D: Effect of mask by amount of knowledge
# ---------------------------------------------------
SAVE_AS <- FALSE
source("plots/cue_first/amount_of_knowledge__accuracy.R")


# Figure 1
# --------
top_row <- arrangeGrob(trialstructure, error_gt, nrow = 1)
imagery_gg <- imagery_plot() +
  theme(
    plot.margin = unit(c(0.0, 0.7, 0.5, 0.3), units = "lines")
  )
factual_gg <- factual_plot() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = unit(c(0.0, 1.0, 0.5, 0.5), units = "lines")
  )

factual_gt <- turn_off_clipping(factual_gg)
imagery_gt <- turn_off_clipping(imagery_gg)
bottom_row <- arrangeGrob(imagery_gt, factual_gt, nrow = 1)

if (SAVE_AS == TRUE) {
  png("plots/cue_first/figS1.png", width = 8, height = 6, units = "in", res = 400)
  grid.arrange(top_row, bottom_row, nrow = 2, heights = c(0.55, 0.45))
  dev.off()
}
  
