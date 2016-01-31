# ---- setup
library(dplyr)
library(broom)
library(ggplot2)
library(lme4)

library(propertyverification)
data(question_first)
question_first <- tidy_property_verification_data(question_first) %>%
  recode_mask_type %>%
  recode_feat_type %>%
  recode_exp_run

subj_mods <- question_first %>%
  group_by(exp_run, subj_id) %>%
  do(error_mod = glm(is_error ~ feat_c * mask_c, family = binomial, data = .))

subj_effects <- subj_mods %>%
  tidy(error_mod) %>%
  filter(term == "feat_c:mask_c") %>%
  select(-term)

ggplot(subj_effects, aes(x = exp_run, y = estimate, color = factor(exp_run))) +
  geom_point(position = position_jitter(width = 0.2, height = 0.0),
             shape = 1, alpha = 0.4) +
  geom_point(stat = "summary", fun.y = "mean", size = 5)

error_mod <- glmer(is_error ~ feat_c * mask_c + (feat_c * mask_c|subj_id) + (mask_c|proposition_id),
                   family = binomial, data = question_first)
summary(error_mod)