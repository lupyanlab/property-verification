# ---- setup
library(dplyr)

library(ggplot2)
library(scales)

library(lme4)
library(broom)
library(AICcmodavg)

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
  filter(!is.na(rt))

# Recode variables
question_first <- question_first %>%
  mutate(is_error = 1 - is_correct) %>%
  recode_property_verification_data

# Label exposure order
question_first <- label_exposure_order(question_first) %>%
  label_outliers %>%
  filter(is_outlier == 0)

# ---- first-question
first_question_plot <- ggplot(filter(question_first, exposure_order == 1),
       aes(x = mask_c, y = is_error)) +
  geom_bar(aes(fill = feat_type, alpha = mask_type, width = 1.0),
           stat = "summary", fun.y = "mean") +
  facet_wrap("feat_label") +
  scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("No mask", "Mask")) +
  scale_y_continuous("Error rate", labels = percent) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  ggtitle("Error rates on first exposure to question")
first_question_plot

# ---- exposure-model
exposure_mod <- glmer(is_error ~ feat_c * mask_c * exposure_c_first + (1|subj_id),
                      family = "binomial", data = question_first)
tidy(exposure_mod, effects = "fixed")

# ---- exposure-models
interactions <- question_first %>%
  group_by(exposure_order) %>%
  do(mod = glm(is_error ~ feat_c * mask_c, family = "binomial", data = .)) %>%
  tidy(mod) %>%
  ungroup() %>%
  filter(term == "feat_c:mask_c")

ggplot(filter(interactions, exposure_order < 6), aes(x = exposure_order, y = estimate)) +
  geom_line() +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  scale_y_continuous("feat x mask interaction term")

# ---- first-five-generations
first_five_plot <- first_question_plot %+% filter(question_first, exposure_order < 6)
first_five_plot + facet_grid(exposure_order ~ feat_label) +
  ggtitle("Error rates on questions\nby exposure order")
