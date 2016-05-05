# ---- setup
library(dplyr)
library(lme4)
library(broom)
library(tidyr)
library(ggplot2)
library(grid)
library(scales)

plot_interaction <- function(frame, y_var, scale_y, coord_ylim) {
  scale_x_mask <- scale_x_continuous("Interference",
                                     breaks = c(-0.5, 0.5),
                                     labels = c("None", "Mask"))
  scale_fill_mask <- scale_fill_discrete("Interference",
                                         labels = c("None", "Mask"))
  base_theme <- theme_minimal(base_size = 14) +
    theme(axis.ticks = element_blank(),
          legend.position = "none")
  
  ggplot(frame, aes_string(x = "mask_c", y = y_var, fill = "mask_f")) +
    geom_bar(aes(width = 1.0), stat = "summary", fun.y = "mean") +
    facet_wrap("feat_label") +
    scale_x_mask +
    scale_y +
    scale_fill_mask +
    coord_cartesian(ylim = coord_ylim) +
    base_theme
}

plot_rt <- function(frame) {
  scale_y_rt <- scale_y_continuous("Reaction time (ms)")
  coord_ylim_rt <- c(200, 750)
  plot_interaction(frame, "rt",
                   scale_y = scale_y_rt,
                   coord_ylim = coord_ylim_rt)
}

plot_error <- function(frame) {
  scale_y_error <- scale_y_continuous("Error rate", labels = percent)
  coord_ylim_error <- c(0, 0.09)
  plot_interaction(frame, "is_error",
                   scale_y = scale_y_error,
                   coord_ylim = coord_ylim_error)
}

# ---- data
library(propertyverificationdata)
data(question_first)
question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_outlier == 0)


# ---- error-mods
error_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                   family = "binomial", data = question_first)

feat_type_mod <- glmer(is_error ~ feat_c + (1|subj_id),
                       family = "binomial",
                       data = filter(question_first, mask_type == "nomask"))

vis_mask_mod <- glmer(is_error ~ mask_c + (1|subj_id),
                      family = "binomial",
                      data = filter(question_first, feat_type == "visual"))

non_mask_mod <- glmer(is_error ~ mask_c + (1|subj_id),
                      family = "binomial",
                      data = filter(question_first, feat_type == "nonvisual"))

# ---- error-plot
plot_error(question_first)

# ---- error-by-exp
base_plot <- ggplot(question_first, aes(x = mask_c)) +
  facet_wrap("feat_label") +
  scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("No mask", "Mask")) +
  scale_color_discrete("") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    axis.ticks = element_blank(),
    panel.margin = unit(3, "lines")
  )

base_plot +
  geom_line(aes(y = is_error, color = exp_run_label),
            stat = "summary", fun.y = "mean",
            size = 1.5) +
  scale_y_continuous("Error rate", labels = percent)

# ---- rt-mod
rt_mod <- lmer(rt ~ feat_c * mask_c + (feat_c * mask_c|subj_id),
               data = question_first)

# ---- rt-plot
plot_rt(question_first)
