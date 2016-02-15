source("reports/propositions/setup.R")

# ---- subset
best_propositions <- norms %>%
  label_ambiguous_propositions %>%
  label_bad_baseline_performance %>%
  filter(agreement == "agree", baseline_difficulty == "easy")
