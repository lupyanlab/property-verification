source("models/amount_of_knowledge__accuracy_for_plot.R")

# check global flag
if (!exists("SAVE_AS")) {
  SAVE_AS <- TRUE
}

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

imagery_range <- range(property_verification$imagery_mean)
imagery_points <- expand_points(imagery_range, "imagery_mean")
imagery_values <- predict_and_merge(imagery_error_mod, imagery_points, "imagery_mean")

facts_range <- range(property_verification$facts_mean)
facts_points <- expand_points(facts_range, "facts_mean")
facts_values <- predict_and_merge(facts_error_mod, facts_points, "facts_mean")

facet_values <- rbind_list(imagery_values, facts_values)

# ------------------------------------------------------------------------------
# Summarize ratings by question

imagery_means <- property_verification %>% 
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

facts_means <- property_verification %>% 
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
source("plots/axis-icons.R")

icon_width <- 0.25

# ------------------------------------------------------------------------------
# Load color scheme.
source("plots/colorscheme.R")

# ------------------------------------------------------------------------------
# Get the function that will make the plot
source("plots/linegraph.R")

# ------------------------------------------------------------------------------
# Draw plots
factual_plot <- function() {
  linegraph(facts_values, facts_means,
            "Amount of encyclopedic knowledge required",
            icon_interference_y = 0.0,
            icon_blank_y = -0.03,
            icon_x = 3.2,
            color_scheme = facts_color_scheme)
}

imagery_plot <- function() {
  linegraph(imagery_values, imagery_means,
            "Amount of visual knowledge required",
            icon_interference_y = -0.01,
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

if (SAVE_AS == TRUE) {
  png("plots/amount_of_knowledge__accuracy_imagery.png",
      width = 6, height = 6, units = "in", res = 200)
  grid.draw(imagery_gt)
  dev.off()
  
  png("plots/amount_of_knowledge__accuracy_encyclopedic.png",
      width = 6, height = 6, units = "in", res = 200)
  grid.draw(factual_gt)
  dev.off()
  
  png("plots/amount_of_knowledge__accuracy.png",
      width = 12, height = 6, units = "in", res = 200)
  grid.arrange(imagery_gt, factual_gt, nrow = 1)
  dev.off()
}