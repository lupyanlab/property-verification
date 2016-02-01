#' @importFrom magrittr %>%
recode_correct_response <- function(frame) {
  correct_response_map <- dplyr::data_frame(
    correct_response = c("no", "yes"),
    correct_response_c = c(-0.5, 0.5)
  )
  frame %>% left_join(correct_response_map)
}

# ---- setup
library(dplyr)
library(ggplot2)
library(lme4)

# ---- data
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverification")
library(propertyverification)
data(question_first)
data(norms)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_mask_type %>%
  recode_feat_type %>%
  recode_exp_run %>%
  recode_correct_response %>%
  left_join(norms)

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


