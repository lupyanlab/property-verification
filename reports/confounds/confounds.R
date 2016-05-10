# ---- setup
library(dplyr)
library(lme4)

library(propertyverificationdata)
data("question_first")
data("norms")

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_outlier == 0)

# Summarize measures of difficulty for each of the propositions
difficulty_measures <- question_first %>%
  filter(mask_type == "nomask") %>%
  group_by(proposition_id) %>%
  summarize(
    num_trials = n(),
    num_errors = sum(is_error, na.rm = TRUE),
    difficulty_exp_error = mean(is_error, na.rm = TRUE),
    difficulty_exp_rt = mean(raw_rt[response != "timeout"], na.rm = TRUE)
  ) %>%
  # add in norming difficulty
  left_join(norms) %>%
  select(
    proposition_id,
    n_norms = difficulty_count,
    difficulty_norms = difficulty_z,
    n_exp = num_trials,
    difficulty_exp_error,
    difficulty_exp_rt,
    imagery_z,
    facts_z,
    senses_z
  )

question_first <- left_join(question_first, difficulty_measures)

# ---- cor
prop_measures <- c(
  "difficulty_norms",
  "difficulty_exp_rt",
  "difficulty_exp_error",
  "imagery_z",
  "facts_z",
  "senses_z"
)
cor(difficulty_measures[, prop_measures], use = "pairwise")

# ---- norms
norms_diff <- glmer(is_error ~ mask_c * difficulty_norms + (1|subj_id),
                    family = "binomial", data = question_first)
summary(norms_diff)

# ---- rt
rt_diff <- glmer(is_error ~ mask_c * difficulty_exp_rt + (1|subj_id),
                 family = "binomial", data = question_first)
summary(rt_diff)
report_glmer_effect(rt_diff, "mask_c:difficulty_exp_rt")

# ---- error
error_diff <- glmer(is_error ~ mask_c * difficulty_exp_error + (1|subj_id),
                    family = "binomial", data = question_first)
summary(error_diff)
report_glmer_effect(error_diff, "mask_c:difficulty_exp_error")

# ---- covariates
norms_diff_cov <- glmer(is_error ~ mask_c * feat_c + difficulty_norms + (1|subj_id),
                        family = "binomial", data = question_first)
summary(norms_diff_cov)

exp_rt_cov <- glmer(is_error ~ mask_c * feat_c + difficulty_exp_rt + (1|subj_id),
                    family = "binomial", data = question_first)
summary(exp_rt_cov)

exp_error_cov <- glmer(is_error ~ mask_c * feat_c + difficulty_exp_error + (1|subj_id),
                        family = "binomial", data = question_first)
summary(exp_error_cov)
report_glmer_effect(exp_error_cov, "mask_c:feat_c")

# ---- abstract
abstract_mod <- glmer(is_error ~ mask_c * senses_z + (1|subj_id),
                      family = "binomial", data = question_first)
summary(abstract_mod)
