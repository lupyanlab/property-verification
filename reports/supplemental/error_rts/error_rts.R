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

# Select observations
question_first <- question_first %>%
  # Drop practice trials
  filter(block_type != "practice") %>%
  # Exclude RTs on timeout trials
  mutate(rt = ifelse(response == "timeout", NA, rt)) %>%
  # Exclude weirdly long RTs
  mutate(rt = ifelse(rt > 2000.0, NA, rt)) %>%
  # Drop any missing RTs
  filter(!is.na(rt)) %>%
  label_outliers %>%
  filter(is_outlier == 0)

# Recode variables
question_first <- question_first %>%
  mutate(is_error = 1 - is_correct) %>%
  recode_property_verification_data

# ---- rt-hist
gg_rt_hist <- ggplot(question_first, aes(x = rt)) +
  base_theme

gg_rt_hist +
  geom_density() +
  ggtitle("All RTs")

gg_rt_hist +
  geom_density(aes(color = correct_response)) +
  ggtitle("RTs by correct response\nto proposition")

gg_rt_hist +
  geom_density(aes(color = factor(is_correct))) +
  ggtitle("RTs by response accuracy")

gg_rt_hist +
  geom_density(aes(color = factor(is_correct))) +
  facet_wrap("correct_response") +
  ggtitle("RTs by actual and expected\nresponse")

# ---- rt-hist-by-exp
gg_rt_hist +
  geom_density() +
  facet_wrap("exp_run", ncol = 1) +
  ggtitle("RTs by experiment")

gg_rt_hist +
  geom_density(aes(color = factor(is_correct))) +
  facet_grid(exp_run ~ correct_response) +
  ggtitle("RTs by actual and expected\nresponse for each experiment")

# ---- rt-z-hist
question_first <- question_first %>%
  group_by(subj_id) %>%
  mutate(
    rt_c = rt - mean(rt),
    rt_z = rt_c/sd(rt)
  ) %>%
  ungroup()

ggplot(question_first, aes(x = rt_c)) +
  geom_density(aes(color = factor(exp_run))) +
  base_theme +
  ggtitle("Normalized RTs by experiment")

# ---- rt-bins
bin_rts <- function(rts) {
  cut(rts, breaks = quantile(rts, probs = seq(0, 1, by = 0.25)),
      labels = c("0-25", "25-50", "50-75", "75-100"),
      include.lowest = TRUE)
}

question_first <- question_first %>%
  mutate(rt_bin = bin_rts(rt_c))

ggplot(question_first, aes(x = rt_bin)) +
  geom_histogram() +
  base_theme +
  ggtitle("RTs were binned by quantile")

# ---- error-by-bin
gg_error_by_bin <- ggplot(question_first, aes(x = rt_bin, y = is_error)) +
  base_theme +
  ggtitle("Error by RT bin")

gg_error_by_bin +
  geom_bar(stat = "summary", fun.y = "mean")

gg_error_by_bin +
  geom_bar(aes(fill = factor(exp_run)), position = "dodge",
           stat = "summary", fun.y = "mean")

# ---- effect-by-bin
summarize_effect <- function(frame, formula, param) {
  frame %>%
    do(mod = glm(formula, data = .)) %>%
    tidy(mod) %>%
    ungroup() %>%
    filter(term == param)
}

gg_effect_by_bin <- function(frame) {
  ggplot(frame, aes(x = rt_bin, y = estimate)) +
    geom_hline(yintercept = 0, size = 0.5, color = "gray") +
    base_theme
}


effects_by_bin <- question_first %>%
  group_by(rt_bin) %>%
  summarize_effect(is_error ~ feat_c * mask_c, "feat_c:mask_c")
gg_effect_by_bin(effects_by_bin) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  ggtitle("Effect by RT bin\nacross experiments")


exp_effects_by_bin <- question_first %>%
  group_by(exp_run, rt_bin) %>%
  summarize_effect(is_error ~ feat_c * mask_c, "feat_c:mask_c")
gg_effect_by_bin(exp_effects_by_bin) +
  geom_bar(aes(fill = factor(exp_run)), stat = "identity", position = "dodge") +
  ggtitle("Effect by RT bin\nin each experiment")

# ---- mask-by-bin
effects_by_mask <- question_first %>%
  group_by(rt_bin) %>%
  summarize_effect(is_error ~ mask_c, "mask_c")
gg_effect_by_bin(effects_by_mask) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  ggtitle("Effect of mask by RT bin\nacross experiments")


exp_effects_by_mask <- question_first %>%
  group_by(rt_bin, exp_run) %>%
  summarize_effect(is_error ~ mask_c, "mask_c")
gg_effect_by_bin(exp_effects_by_mask) +
  geom_bar(aes(fill = factor(exp_run)), stat = "identity", position = "dodge") +
  ggtitle("Effect of mask by RT bin\nin each experiment")


# ---- feat-by-bin
effects_by_feat <- question_first %>%
  group_by(rt_bin) %>%
  summarize_effect(is_error ~ feat_c, "feat_c")
gg_effect_by_bin(effects_by_feat) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  ggtitle("Diff between knowledge types\nby RT bin")


exp_effects_by_feat <- question_first %>%
  group_by(rt_bin, exp_run) %>%
  summarize_effect(is_error ~ feat_c, "feat_c")
gg_effect_by_bin(exp_effects_by_feat) +
  geom_bar(aes(fill = factor(exp_run)), stat = "identity", position = "dodge") +
  ggtitle("Diff between knowledge types\nin each experiment")


# ---- last-bin-means
last_bin <- rev(levels(question_first$rt_bin))[1]
last_bin_data <- filter(question_first, rt_bin == last_bin)

gg_interaction <- ggplot(last_bin_data, aes(x = mask_c, fill = feat_type, alpha = mask_type, width = 1.0)) +
  facet_wrap("feat_label") +
  scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("nomask", "mask")) +
  scale_alpha_manual(values = c(0.9, 0.5)) +
  base_theme +
  theme(legend.position = "none")

gg_interaction_error <- gg_interaction +
  geom_bar(aes(y = is_error), stat = "summary", fun.y = "mean") +
  ggtitle("Last bin: Error rate")
gg_interaction_error

gg_interaction_rt <- gg_interaction +
  geom_bar(aes(y = rt_c), stat = "summary", fun.y = "mean") +
  ggtitle("Last bin: RTs")
gg_interaction_rt

# ---- last-bin-means-by-exp
gg_interaction_error +
  facet_grid(exp_run ~ feat_label)

gg_interaction_rt +
  facet_grid(exp_run ~ feat_label)