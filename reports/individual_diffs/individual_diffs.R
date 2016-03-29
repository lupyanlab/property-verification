# ---- setup
library(dplyr)
library(ggplot2)
library(lme4)
library(broom)
library(stringr)

options(stringsAsFactors = FALSE)

# ---- data
library(propertyverificationdata)
data(question_first)
data(individual_diffs)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_feat_type %>%
  recode_mask_type %>%
  filter(exp_run == 4) %>%
  left_join(individual_diffs)

# ---- imagery-subjs
subj_mod <- glmer(is_error ~ 1 + (feat_c * mask_c|subj_id),
                  family = "binomial", data = question_first)

subj_effects <- subj_mod %>%
  tidy(effects = "random") %>%
  filter(term == "feat_c:mask_c") %>%
  select(subj_id = level, interaction = estimate)

subj_imagery <- individual_diffs %>%
  group_by(subj_id) %>%
  summarize(imagery = mean(imagery, na.rm = TRUE))

subjs <- left_join(subj_effects, subj_imagery)

ggplot(subjs, aes(x = imagery, y = interaction)) +
  geom_point()

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

# ---- question-text
extract_question <- function(proposition_ids) {
  str_split_fixed(proposition_ids, ":", 2)[, 1]
}

# Identify the proposition by question only (not including cue)
data(individual_diffs)
individual_diffs$question <- extract_question(individual_diffs$proposition_id)
individual_diffs <- select(individual_diffs, subj_id, question, imagery)

# Center imagery score for each subject
center <- function(x) x - mean(x, na.rm = TRUE)
individual_diffs <- individual_diffs %>%
  group_by(subj_id) %>%
  mutate(imagery_c = center(imagery))

# Merge imagery scores by question only
data(question_first)
question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_feat_type %>%
  recode_mask_type %>%
  filter(exp_run == 4) %>%
  mutate(question = extract_question(proposition_id)) %>%
  left_join(individual_diffs)

# Predict effect of mask from individual measures of proposition imagery
imagery_mod <- glmer(is_error ~ mask_c * imagery + (1|subj_id),
                     family = "binomial", data = question_first)
summary(imagery_mod)

# Predict effect of mask from centered measures of proposition imagery
imagery_c_mod <- glmer(is_error ~ mask_c * imagery_c + (1|subj_id),
                       family = "binomial", data = question_first)
summary(imagery_c_mod)

ggplot(question_first, aes(x = imagery_c, y = is_error, color = mask_type)) +
  geom_smooth(method = "lm")

ggplot(question_first, aes(x = feat_type, y = imagery)) +
  geom_point(stat = "summary", fun.y = "mean")

data(norms)
question_first <- left_join(question_first, norms)
imagery_compare <- question_first %>%
  group_by(proposition_id) %>%
  summarize(imagery_indiv = mean(imagery, na.rm = TRUE),
            imagery_norm = mean(imagery_z, na.rm = TRUE)) %>%
  filter(!is.na(imagery_indiv), !is.na(imagery_norm))
  
ggplot(imagery_compare, aes(x = imagery_indiv, y = imagery_norm)) +
  geom_point()
