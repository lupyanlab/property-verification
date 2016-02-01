# ---- setup
library(dplyr)
library(ggplot2)
library(lme4)

# ---- data
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverificationdata")
library(propertyverificationdata)
data(question_first)
data(norms)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_mask_type %>%
  recode_feat_type %>%
  recode_exp_run %>%
  recode_correct_response %>%
  left_join(norms)

# ---- filters
question_first <- question_first %>%
  label_ambiguous_propositions %>%
  filter(agreement != "ambiguous")

# ---- feat-type-mod
feat_type_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                       family = binomial, data = question_first)
summary(feat_type_mod)

# ---- imagery-mod
mask_effect_mod <- glmer(is_error ~ imagery_z * mask_c + (mask_c|proposition_id) + (1|subj_id),
                         family = binomial, data = question_first)
summary(mask_effect_mod)

mask_effect_response_mod <- glmer(
  is_error ~ imagery_z + mask_c + correct_response_c + (mask_c|proposition_id),
  family = binomial, data = question_first
)
summary(mask_effect_response_mod)

mask_effect_size_mod <- glmer(
  is_error ~ imagery_z * mask_c + (mask_c|proposition_id),
  family = binomial, data = question_first
)
summary(mask_effect_size_mod)

mask_effect_by_response_mod <- glmer(
  is_error ~ mask_c * correct_response_c + (mask_c|proposition_id),
  family = binomial, data = question_first
)
summary(mask_effect_by_response_mod)


