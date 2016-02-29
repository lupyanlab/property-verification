# ---- setup
library(dplyr)
library(ggplot2)
library(lme4)
library(broom)

options(stringsAsFactors = FALSE)

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

individual_diffs <- read.csv("individual_diffs/imagery.csv") %>%
  select(subj_id, proposition_id, imagery)

# ----
property_verification <- property_verification %>%
  left_join(individual_diffs)

individual_diffs_mod <- glmer(is_error ~ imagery * mask_c + (imagery * mask_c|subj_id) + (mask_c|proposition_id),
                              family = "binomial", data = property_verification)
summary(individual_diffs_mod)

proposition_mods <- property_verification %>%
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
  geom_smooth(aes(group = subj_id), method = "lm") +
  facet_wrap("subj_id")