library(dplyr)

SAVE_AS <- FALSE

# ------------------------------------------------------------------------------
# Summarize the data.
feature <- read.csv("./feature-last/feature-last-final.csv", 
                    stringsAsFactors = FALSE)

feature <- filter(feature, subj_id != "MWPF214")

library(lme4)
imagery <- glmer(is_error ~ imagery_mean * mask_c + (1|subj_id), 
                 data = feature, family = binomial)
factual <- glmer(is_error ~ facts_mean * mask_c + (1|subj_id),
                 data = feature, family = binomial)

# ------------------------------------------------------------------------------
# Get the model estimates
library(AICcmodavg)

# get the x predictions
expand_points <- function(rating_range, name) {
  points <- expand.grid(
    tmp = seq(rating_range[1], rating_range[2], by = 0.1),
    mask_c = c(-0.5, 0.5)
  )
  mask_map <- data.frame(mask_type = c("nomask", "mask"), mask_c = c(-0.5, 0.5))
  points <- left_join(points, mask_map)
  plyr::rename(points, c("tmp" = name))
}

# get the y predictions (and merge with x)
predict_and_merge <- function(mod, newdata, type) {
  yvals <- predictSE(mod, newdata, se.fit = TRUE) %>%
    as.data.frame() %>% select(rate = fit, se = se.fit)

  common_name <- c("rating")
  names(common_name) <- type
  plyr::rename(newdata, common_name) %>%
    cbind(., yvals) %>%
    mutate(type = type)
}

imagery_range <- range(feature$imagery_mean)
imagery_points <- expand_points(imagery_range, "imagery_mean")
imagery_values <- predict_and_merge(imagery, imagery_points, "imagery_mean")

facts_range <- range(feature$facts_mean)
facts_points <- expand_points(facts_range, "facts_mean")
facts_values <- predict_and_merge(factual, facts_points, "facts_mean")

facet_values <- rbind_list(imagery_values, facts_values)

# ------------------------------------------------------------------------------
# Summarize ratings by question

imagery_means <- feature %>% 
  group_by(imagery_mean = cut(imagery_mean, breaks = seq(0, 3.5, by = 0.25),
                              labels = seq(0.125, 3.375, by = 0.25), right = FALSE), 
           mask_type) %>%
  summarize(
    obs = n(),
    rate = mean(is_error, na.rm = TRUE)
  ) %>% ungroup() %>%
  mutate(
    rating = as.numeric(levels(imagery_mean))[imagery_mean],
    mask_type = factor(mask_type, levels = c("nomask", "mask")),
    type = "imagery_mean"
  ) %>% select(-imagery_mean)

facts_means <- feature %>% 
  group_by(facts_mean = cut(facts_mean, breaks = seq(0, 3, by = 0.25),
                            labels = seq(0.125, 2.875, by = 0.25), right = FALSE), 
           mask_type) %>%
  summarize(
    obs = n(),
    rate = mean(is_error, na.rm = TRUE)
  ) %>% ungroup() %>%
  mutate(
    rating = as.numeric(levels(facts_mean))[facts_mean],
    mask_type = factor(mask_type, levels = c("nomask", "mask")),
    type = "facts_mean"
  ) %>% select(-facts_mean)

# hack in missing values
facts_means[is.na(facts_means$rating), "rating"] <- 2.875

# question_means <- rbind_list(imagery_means, facts_means)

# ------------------------------------------------------------------------------
# Get factors in right order

facet_values$type <- factor(facet_values$type, levels = c("imagery_mean", "facts_mean"))
# question_means$type <- factor(question_means$type, levels = c("imagery_mean", "facts_mean"))

# ------------------------------------------------------------------------------
# Prepare icons
source("./new-graphs/axis-icons.R")

icon_width <- 0.25

# ------------------------------------------------------------------------------
# Load color scheme.
source("./new-graphs/colorscheme.R")

# ------------------------------------------------------------------------------
# Generate the plot.
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

generate_plot <- function(values, means, x_label, 
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

# ------------------------------------------------------------------------------
# Draw plots
factual_plot <- function() {
  generate_plot(facts_values, facts_means,
                "Amount of nonvisual knowledge required",
                icon_interference_y = 0.0,
                icon_blank_y = 0.08,
                icon_x = 3.2,
                color_scheme = facts_color_scheme)
}

imagery_plot <- function() {
  generate_plot(imagery_values, imagery_means,
                "Amount of visual knowledge required",
                icon_interference_y = 0.07,
                icon_blank_y = -0.11,
                icon_x = 3.5,
                color_scheme = imagery_color_scheme)
}

factual_gg <- factual_plot()
factual_gt <- turn_off_clipping(factual_gg)

imagery_gg <- imagery_plot()
imagery_gt <- turn_off_clipping(imagery_gg)

grid.draw(factual_gt)
grid.draw(imagery_gt)


# ------------------------------------------------------------------------------
# png
if (SAVE_AS == "png") {
  png("./new-graphs/amounts-imagery.png", width = 6, height = 6, units = "in", res = 200)
  grid.draw(imagery_gt)
  dev.off()
  
  png("./new-graphs/amounts-facts.png", width = 6, height = 6, units = "in", res = 200)
  grid.draw(factual_gt)
  dev.off()
  
  png("./new-graphs/amounts.png", width = 6, height = 12, units = "in", res = 200)
  grid.arrange(imagery_gt, factual_gt, ncol = 1)
  dev.off()
}
