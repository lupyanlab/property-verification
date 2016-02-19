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
property_verification <- compile("experiment/data/", regex_key="PV*") %>%
    filter(block_type != "practice") %>%
    mutate(
      # only measure errors of comission
      is_correct = ifelse(response == "timeout", NA, is_correct),

      # only measure rt on correct responses
      rt = ifelse(is_correct == 1, rt, NA),

      # invert correctness to get error
      is_error = 1 - is_correct
    ) %>%
    # create new columns for modeling and graphing
    recode_feat_type %>%
    recode_mask_type

# ---- outliers
Z_SCORE_CUTOFF <- 2.0

subj_mods <- property_verification %>%
  group_by(subj_id) %>%
  do(mod = glm(is_error ~ feat_c * mask_c, family = "binomial", data = .))

subj_effects <- subj_mods %>%
  tidy(mod) %>%
  filter(term == "feat_c:mask_c")

z_score <- function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
subj_effects$estimate_z <- z_score(subj_effects$estimate)
subj_effects$is_outlier <- ifelse(abs(subj_effects$estimate_z) > Z_SCORE_CUTOFF, 1, 0)
outlier_labels <- select(subj_effects, subj_id, is_outlier)

# drop any outlier subjects
property_verification <- left_join(property_verification, outlier_labels) %>%
  filter(is_outlier != 1)

# ---- rt
rt_mod <- lmer(rt ~ feat_c * mask_c + (feat_c * mask_c|subj_id) + (mask_c|proposition_id),
               data = property_verification)
summary(rt_mod)
plot_rt(property_verification)

# ---- error
error_mod <- lmer(is_error ~ feat_c * mask_c + (feat_c * mask_c|subj_id) + (mask_c|proposition_id),
                  family = "binomial", data = property_verification)
summary(error_mod)
plot_error(property_verification)
