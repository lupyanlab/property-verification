source("reports/propositions/setup.R")

# ---- subset
only_best_propositions <- question_first %>%
  label_bad_baseline_difficulty
label_ambiguous_propositions %>%
  label_outlier_subjects %>%
  filter(outlier == FALSE, baseline_difficulty == "easy", agreement == "agree")

feat_mod <- glmer(is_error ~ feat_c * mask_c + (feat_c * mask_c|subj_id),
                  family = "binomial", data = only_best_propositions)
summary(feat_mod)