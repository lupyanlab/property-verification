# ---- setup
library(dplyr)
library(tidyr)

library(ggplot2)
library(scales)

library(lme4)
library(broom)

# ---- theme
base_theme <- theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.ticks = element_blank()
  )

# ---- data
library(propertyverificationdata)
data(question_first)
data(norms)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_outlier == 0)

# ---- effect-by-diff-bin
bin_diffs <- function(diffs) {
  cut(diffs, breaks = quantile(diffs, probs = seq(0, 1, by = 0.25)),
      labels = c("0-25", "25-50", "50-75", "75-100"),
      include.lowest = TRUE)
}

norms <- mutate(norms, diff_bin = bin_diffs(difficulty_z))

question_first <- left_join(question_first, norms)

summarize_effect <- function(frame, formula, param) {
  frame %>%
    do(mod = glm(formula, data = .)) %>%
    tidy(mod) %>%
    ungroup() %>%
    filter(term == param)
}

effects_by_bin <- question_first %>%
  group_by(diff_bin) %>%
  summarize_effect(is_error ~ feat_c * mask_c, "feat_c:mask_c")

ggplot(effects_by_bin, aes(x = diff_bin, y = estimate)) +
  geom_line(aes(group = 1)) +
  geom_linerange(aes(ymin = estimate - std.error, ymax = estimate + std.error))

# ---- effect-by-diff-mod
effect_by_diff_mod <- glmer(is_error ~ feat_c * mask_c * difficulty_z +
                              (1|subj_id),
                            family = "binomial", data = question_first)
