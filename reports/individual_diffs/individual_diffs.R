# ---- setup
library(dplyr)
library(ggplot2)
library(lme4)
library(broom)

options(stringsAsFactors = FALSE)

# ---- data
library(propertyverificationdata)
data(question_first)
data(individual_diffs)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_feat_type %>%
  recode_mask_type %>%
  left_join(individual_diffs)

# ---- individual-diffs-mod
individual_diffs_mod <- glmer(is_error ~ imagery * mask_c + (imagery * mask_c|subj_id),
                              family = "binomial", data = question_first)
summary(individual_diffs_mod)

# ---- predict-mask-effect
proposition_mods <- question_first %>%
  group_by(proposition_id) %>%
  do(mod = glm(is_error ~ mask_c, family = "binomial", data = .))

proposition_effects <- proposition_mods %>%
  tidy(mod) %>%
  filter(term == "mask_c") %>%
  select(proposition_id, effect_of_mask = estimate)

individual_diffs <- individual_diffs %>%
  left_join(proposition_effects)

ggplot(individual_diffs, aes(x = imagery, y = effect_of_mask)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(aes(group = 1), method = "lm")

# ---- predict-mask-effect-by-subj
ggplot(individual_diffs, aes(x = imagery, y = effect_of_mask)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(aes(group = subj_id), method = "lm") +
  facet_wrap("subj_id")