# ---- setup
library(dplyr)
library(ggplot2)
library(scales)
library(lme4)

base_theme <- theme_minimal(base_size = 16) +
  theme(axis.ticks = element_blank())

plot_feat_type <- function(frame, dv) {

  # switch on dv
  scale_y_correct <- scale_y_continuous("Error Rate", labels = percent)
  scale_y_rt <- scale_y_continuous("Reaction Time (msec)")
  
  scale_y_options <- list(
    is_correct = scale_y_correct,
    rt = scale_y_rt
  )
  scale_y <- scale_y_options[dv]

  # use aes_string so dv can be provided
  ggplot(frame, aes_string(x = "mask_c", y = dv)) +
    geom_bar(stat = "summary", fun.y = "mean",
             # hack to adjust bar width when using summary functions
             mapping = aes(width = 0.1)) +
    scale_x_mask +
    scale_y +
    base_theme
}

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
  label_ambiguous_propositions %>%
  label_outlier_subjects %>%
  left_join(norms)

# ---- filters
question_first <- question_first %>%
  filter(
    agreement != "ambiguous",
    outlier == FALSE
  )

# ---- feat-type-mod
feat_type_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                       family = binomial, data = question_first)
summary(feat_type_mod)

# ---- imagery-mod
mask_effect_mod <- glmer(is_error ~ imagery_z * mask_c + (mask_c|proposition_id),
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


