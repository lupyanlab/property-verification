# ---- utils
library(grid)
library(png)

png_to_grob <- function(png_image, alpha = 0.2) {
  img <- readPNG(png_image, info = TRUE)
  img_alpha <- matrix(rgb(img[,,1], img[,,2], img[,,3], alpha), nrow=dim(img)[1])
  img_grob <- rasterGrob(img_alpha, interpolate = TRUE)
  img_grob
}

error_rate <- function(x) {
  to_pct <- function(y) y * 100
  mean(x, na.rm = TRUE) %>%
    to_pct %>%
    round(digits = 1) %>%
    paste0("%")
}