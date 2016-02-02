# ---- setup
library(dplyr)
library(broom)
library(ggplot2)
library(grid)
library(lme4)
library(stringr)

scale_color_exp_run <- scale_color_discrete("")

subjects_theme <- theme_minimal(base_size = 14) +
  theme(axis.ticks = element_blank())

# density plot
plot_overall_effect <- function(frame) {
  ggplot(frame, aes(x = estimate)) +
    geom_density(fill = "gray", alpha = 0.5, size = 0.0) +
    geom_density(aes(color = exp_run_label)) +
    scale_x_continuous("knowledge type x interference interaction") +
    scale_y_continuous("number of subjects (density)") +
    scale_color_exp_run +
    subjects_theme
}

recode_parameter <- function(frame) {
  terms <- c("(Intercept)", "mask_c", "feat_c", "feat_c:mask_c")
  labels <- c("overall error rate (intercept)", "effect of mask (mask_c)", "difference between knowledge types (feat_c)", "interaction (feat_c:mask_c)")
  parameter_map <- data_frame(
    term = terms,
    term_label = factor(terms, levels = terms, labels = labels)
  )
  frame %>% left_join(parameter_map)
}

# ---- data
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverificationdata")
library(propertyverificationdata)
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
  ungroup

# recode and label
subj_effects <- subj_effects %>%
  recode_exp_run %>%
  recode_parameter %>%
  label_outlier_subjects

# ---- subj-effects-plot
plot_overall_effect(subj_effects) +
  facet_wrap("term_label", ncol = 1, scales = "free_y") +
  theme(panel.margin = unit(3, "lines")) +
  geom_vline(aes(xintercept = xintercept),
             data = data_frame(xintercept = c(-5, 5)),
             lty = 2, size = 0.2, alpha = 0.4)

# ---- subj-interactions-plot
subj_interactions <- subj_effects %>%
  filter(term == "feat_c:mask_c")

# data for annotations
overall_mean <- mean(subj_interactions$estimate)
overall_mean_without_outliers <- mean(subj_interactions$estimate[!subj_interactions$outlier])
overall_effects <- data_frame(
  label = c("with outliers", ""),
  mean_effect = c(overall_mean, overall_mean_without_outliers),
  height = 0.08,
  sample = c("with_outliers", "no_outliers")
)

exp_run_means <- subj_interactions %>%
  filter(outlier == FALSE) %>%
  group_by(exp_run) %>%
  summarize(mean_effect = mean(estimate)) %>%
  recode_exp_run %>%
  mutate(height = 0.03, sample = "no_outliers")

effect_summary <- rbind_list(overall_effects, exp_run_means)

plot_overall_effect(subj_interactions[!subj_interactions$outlier, ]) +
  geom_segment(aes(x = mean_effect, xend = mean_effect, y = 0, yend = height,
                   color = exp_run_label, lty = sample), data = effect_summary) +
  scale_linetype_manual(values = c(1, 2)) +
  guides(lty = "none") +
  geom_text(aes(x = mean_effect, y = height, label = label), data = effect_summary,
            hjust = -0.02, angle = 45, size = 3) +
  ggtitle("Distribution of by-subject effects")
