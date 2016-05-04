# ---- setup
library(dplyr)
library(lme4)
library(broom)
library(ggplot2)
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
  coord_ylim_rt <- c(500, 1500)
  plot_interaction(frame, "rt",
                   scale_y = scale_y_rt,
                   coord_ylim = coord_ylim_rt)
}

plot_error <- function(frame) {
  scale_y_error <- scale_y_continuous("Error rate", labels = percent)
  coord_ylim_error <- c(0, 0.14)
  plot_interaction(frame, "is_error",
                   scale_y = scale_y_error,
                   coord_ylim = coord_ylim_error)
}

# ---- data
library(propertyverificationdata)
data(question_first)
question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_feat_type %>%
  recode_mask_type %>%
  filter(exp_run == 4)

# ---- rt
rt_mod <- lmer(rt ~ feat_c * mask_c + (feat_c * mask_c|subj_id),
               data = question_first)
summary(rt_mod)
plot_rt(question_first)

# ---- error
error_mod <- glmer(is_error ~ feat_c * mask_c + (feat_c * mask_c|subj_id),
                   family = "binomial", data = question_first)
summary(error_mod)
plot_error(question_first)
