# ---- setup
library(dplyr)
library(broom)
library(ggplot2)
library(grid)
library(scales)
library(lme4)
library(stringr)

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

# Label model parameters (terms)
label_term <- function(frame) {
  terms <- c("(Intercept)", "mask_c", "feat_c", "feat_c:mask_c")
  labels <- c("overall error", "effect of mask", "effect of feat", "interaction")
  key <- data_frame(
    term = terms,
    term_label = factor(terms, levels = terms, labels = labels)
  )
  frame %>% left_join(key)
}

# Error function
calculate_pointrange <- function(estimate) {
  data_frame(
    y = mean(estimate),
    ymin = mean(estimate) - (2 * sd(estimate)),
    ymax = mean(estimate) + (2 * sd(estimate))
  )
}

# Theme
scale_y_estimate <- scale_y_continuous("Estimate")
scale_y_estimate_z <- scale_y_continuous("Estimate (z-score)",
                                         breaks = -3:3)
scale_y_estimate_abs_z <- scale_y_continuous("Estimate (abs z-score)",
                                             breaks = 0:3)

scale_color_exp_run <- scale_color_discrete("")
scale_x_strategy <- scale_x_discrete("coded strategy")

subjects_theme <- theme_minimal(base_size = 14) +
  theme(axis.ticks = element_blank())

# ---- data
library(propertyverificationdata)
data(question_first)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_mask_type %>%
  recode_feat_type %>%
  recode_exp_run

# ---- sample
question_first %>%
  group_by(exp_run_label) %>%
  summarize(num_subjects = length(unique(subj_id)))

# ---- subj-effects-mod
subj_effects_mod <- glmer(is_error ~ 1 + (feat_c * mask_c|subj_id),
                          family = "binomial", data = question_first)

# ---- subj-effects
subj_effects <- tidy(subj_effects_mod, effects = "random") %>%
  filter(group == "subj_id") %>%
  select(subj_id = level, term, estimate)

# Convert estimates to z-scores and abs z-scores
z_score <- function(x) (x - mean(x))/sd(x)
subj_effects <- group_by(subj_effects, term) %>%
  mutate(
    estimate_z = z_score(estimate),
    estimate_abs_z = abs(estimate_z)
  ) %>% ungroup

# Label the computer and experiment the subject was in
exp_map <- unique(question_first[, c("exp_run", "computer", "subj_id")])
subj_effects <- left_join(subj_effects, exp_map) %>% recode_exp_run
subj_effects$computer <- str_to_lower(subj_effects$computer)

# Label the model terms 
subj_effects <- label_term(subj_effects)

# Mean effect across glmer random effects
# subj_effects %>%
#   group_by(term) %>%
#   summarize(effect = mean(estimate))

# Mean effect across by-subject models
# question_first %>%
#   group_by(subj_id) %>%
#   do(mod = glm(is_error ~ feat_c * mask_c, family = "binomial", data = .)) %>%
#   tidy(mod) %>%
#   group_by(term) %>%
#   summarize(effect = mean(estimate))

# ---- subj-effects-plot
ggplot(subj_effects, aes(x = term_label, y = estimate)) +
  geom_point(shape = 1, position = position_jitter(width = 0.2, height = 0.0),
             alpha = 0.2) +
  geom_pointrange(stat = "summary", fun.data = calculate_pointrange) +
  scale_y_estimate +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subj_effects, aes(x = term_label, y = estimate_z)) +
  geom_point(shape = 1, position = position_jitter(width = 0.2, height = 0.0),
             alpha = 0.2) +
  geom_pointrange(stat = "summary", fun.data = calculate_pointrange) +
  scale_y_estimate_z +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subj_effects, aes(x = term_label, y = estimate_z)) +
  geom_line(aes(group = subj_id), alpha = 0.2) +
  geom_hline(aes(yintercept = c(-2, 2)), lty = 2, alpha = 0.4) +
  scale_y_estimate_z +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- exp-effects-plot
ggplot(subj_effects, aes(x = term_label, y = estimate_z)) +
  geom_point(aes(color = exp_run_label), shape = 1, position = position_jitter(width = 0.2, height = 0.0),
             alpha = 0.8) +
  geom_line(aes(group = exp_run_label, color = exp_run_label), shape = 1,
            alpha = 0.8, stat = "summary", fun.y = "mean") +
  scale_y_estimate_z +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- computer-effects-plot
ggplot(subj_effects, aes(x = term_label, y = estimate_z)) +
  geom_point(aes(color = computer), shape = 1, position = position_jitter(width = 0.2, height = 0.0),
             alpha = 0.8) +
  geom_line(aes(group = computer, color = computer), shape = 1,
            alpha = 0.8, stat = "summary", fun.y = "mean") +
  scale_y_estimate_z +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- computer-effects-plot-by-exp
ggplot(subj_effects, aes(x = term_label, y = estimate_z)) +
  geom_point(aes(color = computer), shape = 1, position = position_jitter(width = 0.2, height = 0.0),
             alpha = 0.8) +
  geom_line(aes(group = computer, color = computer), shape = 1,
            alpha = 0.8, stat = "summary", fun.y = "mean") +
  scale_y_estimate_z +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap("exp_run_label", nrow = 1)

# ---- exp-means
question_first <- question_first %>%
  group_by(subj_id) %>%
  mutate(rt_c = rt - mean(rt, na.rm = TRUE)) %>%
  ungroup()

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
  geom_line(aes(y = is_error, color = factor(exp_run)),
            stat = "summary", fun.y = "mean",
            size = 1.5) +
  scale_y_continuous("Error rate", labels = percent)

base_plot +
  geom_line(aes(y = rt_c, color = factor(exp_run)),
            stat = "summary", fun.y = "mean",
            size = 1.5) +
  scale_y_continuous("Mean-centered RT (ms)")
