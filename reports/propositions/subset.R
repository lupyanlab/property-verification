source("reports/propositions/setup.R")

# ---- subset
best_propositions <- norms %>%
  label_ambiguous_propositions %>%
  label_bad_baseline_performance(question_first) %>%
  filter(agreement == "agree", baseline_difficulty == "easy")

cat("number of remaining propositions")
nrow(best_propositions)

cat("distribution of propositions by type")
table(best_propositions[, c("feat_type", "correct_response")])

best_proposition_ids <- best_propositions$proposition_id

# Summarize baseline task performance for each proposition
proposition_difficulty <- question_first %>%
  recode_property_verification_data %>%
  filter(mask_type == "nomask", proposition_id %in% best_proposition_ids) %>%
  group_by(proposition_id) %>%
  summarize(
    num_trials = n(),
    num_errors = sum(is_error, na.rm = TRUE),
    difficulty_exp_error = mean(is_error, na.rm = TRUE),
    difficulty_exp_rt = mean(rt[is_correct == 1])
  ) %>%
  left_join(norms) %>%
  select(
    proposition_id,
    correct_response,
    feat_type,
    n_norms = difficulty_count,
    difficulty_norms = difficulty_z,
    n_exp = num_trials,
    difficulty_exp_error,
    difficulty_exp_rt
  )

lm(difficulty_norms ~ feat_type, data = proposition_difficulty) %>% tidy
lm(difficulty_exp_error ~ feat_type, data = proposition_difficulty) %>% tidy
lm(difficulty_exp_rt ~ feat_type, data = proposition_difficulty) %>% tidy