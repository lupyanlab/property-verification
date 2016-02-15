source("reports/propositions/setup.R")

# ---- subset
best_propositions <- norms %>%
  label_ambiguous_propositions %>%
  label_bad_baseline_performance(question_first) %>%
  filter(agreement == "agree", baseline_difficulty == "easy") %>%
  select(proposition_id, feat_type, correct_response)

cat("number of remaining propositions")
nrow(best_propositions)

cat("distribution of propositions by type")
table(best_propositions[, c("feat_type", "correct_response")])

# Summarize baseline task performance for each proposition
proposition_difficulty <- question_first %>%
  filter(mask_type == "nomask") %>%
  group_by(proposition_id) %>%
  summarize(
    num_trials = n(),
    num_errors = sum(is_error, na.rm = TRUE),
    difficulty_exp_error = mean(is_error, na.rm = TRUE),
    difficulty_exp_rt = mean(rt[is_correct == 1], na.rm = TRUE)
  ) %>%
  left_join(norms) %>%
  select(
    proposition_id,
    n_norms = difficulty_count,
    difficulty_norms = difficulty_z,
    n_exp = num_trials,
    difficulty_exp_error,
    difficulty_exp_rt
  )

best_propositions <- best_propositions %>% left_join(proposition_difficulty)

for(seed in 1:10000) {
  set.seed(seed)
  
  subset_proposition_ids <- best_propositions %>%
    group_by(feat_type, correct_response) %>%
    sample_n(50) %>%
    .$proposition_id
  
  # see if the randomly selected propositions differ in norming difficulty
  norm_diff <- filter(norms, proposition_id %in% subset_proposition_ids) %>%
    lm(difficulty_z ~ feat_type, data = .) %>%
    tidy %>%
    filter(term == "feat_typevisual") %>%
    .$p.value < 0.05
  
  # see if the randomly selected propositions differ in exp difficulty (error)
  exp_error_diff <- filter(question_first, proposition_id %in% subset_proposition_ids) %>%
    glm(is_error ~ feat_type, data = .) %>%
    tidy %>%
    filter(term == "feat_typevisual") %>%
    .$p.value < 0.05
  
  # see if the randomly selected propositions differ in exp difficulty (rt)
  exp_rt_diff <- filter(question_first, proposition_id %in% subset_proposition_ids) %>%
    lm(rt ~ feat_type, data = .) %>%
    tidy %>%
    filter(term == "feat_typevisual") %>%
    .$p.value < 0.05
  
  if(!any(norm_diff, exp_error_diff, exp_rt_diff)) {
    cat("found one!", seed)
    break
  }
}

# verify
selected_propositions <- norms %>%
  filter(proposition_id %in% subset_proposition_ids) %>%
  left_join(proposition_difficulty)

table(selected_propositions[, c("feat_type", "correct_response")])

ggplot(selected_propositions, aes(x = feat_type, y = difficulty_z)) +
  geom_point(aes(color = feat_type), shape = 1, position = position_jitter(width = 0.1)) +
  geom_point(stat = "summary", fun.y = "mean", size = 4) +
  facet_wrap("correct_response")

ggplot(selected_propositions, aes(x = feat_type, y = difficulty_exp_error)) +
  geom_point(aes(color = feat_type), shape = 1, position = position_jitter(width = 0.1)) +
  geom_point(stat = "summary", fun.y = "mean", size = 4) +
  facet_wrap("correct_response")

ggplot(selected_propositions, aes(x = feat_type, y = difficulty_exp_rt)) +
  geom_point(aes(color = feat_type), shape = 1, position = position_jitter(width = 0.1)) +
  geom_point(stat = "summary", fun.y = "mean", size = 4) +
  facet_wrap("correct_response")

# ---- export-subset
selected_propositions %>%
  select(proposition_id) %>%
  write.csv("balanced-propositions.csv", row.names = FALSE)
