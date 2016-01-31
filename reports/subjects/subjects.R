# ---- setup
library(dplyr)
library(broom)
library(ggplot2)
library(lme4)
library(stringr)

scale_color_exp_run <- scale_color_discrete("")

subjects_theme <- theme_minimal(base_size = 14) +
  theme(axis.ticks = element_blank())

# ---- data
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverification")
library(propertyverification)
data(question_first)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_mask_type %>%
  recode_feat_type %>%
  recode_exp_run

# ---- subj-effects
subj_mods <- question_first %>%
  group_by(exp_run, subj_id) %>%
  do(error_mod = glm(is_error ~ feat_c * mask_c, family = binomial, data = .))

subj_effects <- subj_mods %>%
  tidy(error_mod) %>%
  filter(term == "feat_c:mask_c") %>%
  select(-term)

# recode exp run to get exp run label column
subj_effects <- subj_effects %>% recode_exp_run

# add columns for rank
rank_effects <- function(estimate) rank(estimate)
subj_effects <- subj_effects %>%
  # overall rank
  ungroup %>% mutate(overall_rank = rank_effects(estimate)) %>%
  # rank in exp run
  group_by(exp_run) %>% mutate(rank_in_exp_run = rank_effects(estimate)) %>% ungroup

plot_overall_effect <- function(frame) {
  ggplot(frame, aes(x = estimate)) +
    geom_density(fill = "gray", alpha = 0.5, size = 0.0) +
    geom_density(aes(color = exp_run_label)) +
    scale_x_continuous("knowledge type x interference interaction") +
    scale_y_continuous("subject density") +
    scale_color_exp_run +
    subjects_theme +
    coord_cartesian(ylim = c(0.0, 0.5))
}

# ---- subj-effects-plots
plot_overall_effect(subj_effects) +
  ggtitle("Density of by-subject effects (all subjects)")

outliers <- abs(subj_effects$estimate) > 10
plot_overall_effect(subj_effects[!outliers, ]) +
  ggtitle("Density of by-subject effects (no outliers)")

# data for annotations
overall_mean <- mean(subj_effects$estimate)
overall_mean_without_outliers <- mean(subj_effects$estimate[!outliers])
overall_effects <- data_frame(
  label = c("with outliers", ""),
  mean_effect = c(overall_mean, overall_mean_without_outliers),
  height = 0.08,
  sample = c("with_outliers", "no_outliers")
)

exp_run_means <- subj_effects %>%
  filter(!outliers) %>%
  group_by(exp_run) %>%
  summarize(mean_effect = mean(estimate)) %>%
  recode_exp_run %>%
  mutate(height = 0.03, sample = "no_outliers")

effect_summary <- rbind_list(overall_effects, exp_run_means)

plot_overall_effect(subj_effects[!outliers, ]) +
  geom_segment(aes(x = mean_effect, xend = mean_effect, y = 0, yend = height,
                   color = exp_run_label, lty = sample), data = effect_summary) +
  scale_linetype_manual(values = c(1, 2)) +
  guides(lty = "none") +
  geom_text(aes(x = mean_effect, y = height, label = label), data = effect_summary,
            hjust = -0.02, angle = 45, size = 3) +
  ggtitle("Density of by-subject effects")
