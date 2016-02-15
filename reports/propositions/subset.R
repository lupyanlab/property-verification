source("reports/propositions/setup.R")

# ---- subset
cat("total number of propositions in the original stimuli set")
nrow(norms)

best_propositions <- norms %>%
  label_ambiguous_propositions %>%
  label_bad_baseline_performance(question_first) %>%
  filter(agreement == "agree", baseline_difficulty == "easy") %>%
  select(proposition_id, feat_type, correct_response)

cat("number of remaining propositions (no ambiguous propositions or propositions that were too difficult")
nrow(best_propositions)

cat("distribution of propositions by type")
table(best_propositions[, c("feat_type", "correct_response")])

best_propositions <- best_propositions %>%
  left_join(difficulty_measures) %>%
  filter(
    # additional cutoffs from eyeballing distributions
    difficulty_exp_error < 0.2,
    difficulty_exp_rt < 600
  )

cat("entering loop to randomly select balanced propositions")

for(seed in 1:1000) {
  set.seed(seed)
  
  subset_proposition_ids <- best_propositions %>%
    group_by(feat_type, correct_response) %>%
    sample_n(50) %>%
    .$proposition_id
  
  # see if the randomly selected propositions differ in norming difficulty
  norm_diff <- filter(norms, proposition_id %in% subset_proposition_ids) %>%
    lm(difficulty_z ~ feat_type + correct_response, data = .) %>%
    tidy %>%
    filter(term == "feat_typevisual") %>%
    .$p.value < 0.05
  
  # see if the randomly selected propositions differ in exp difficulty (error)
  exp_error_diff <- filter(question_first, proposition_id %in% subset_proposition_ids) %>%
    glm(is_error ~ feat_type + correct_response, data = .) %>%
    tidy %>%
    filter(term == "feat_typevisual") %>%
    .$p.value < 0.05
  
  # see if the randomly selected propositions differ in exp difficulty (rt)
  exp_rt_diff <- filter(question_first, proposition_id %in% subset_proposition_ids) %>%
    lm(rt ~ feat_type + correct_response, data = .) %>%
    tidy %>%
    filter(term == "feat_typevisual") %>%
    .$p.value < 0.05
  
  if(!any(norm_diff, exp_error_diff, exp_rt_diff)) {
    cat("found a set of propositions that are balanced!")
    break
  }
}

cat("verify the final selected propositions")

# verify
selected_propositions <- norms %>%
  filter(proposition_id %in% subset_proposition_ids) %>%
  left_join(difficulty_measures)


table(selected_propositions[, c("feat_type", "correct_response")])

ggplot(selected_propositions, aes(x = feat_type, y = difficulty_z)) +
  geom_point(aes(color = feat_type), shape = 1, position = position_jitter(width = 0.1)) +
  geom_point(stat = "summary", fun.y = "mean", size = 4) +
  facet_wrap("correct_response") +
  base_theme

ggplot(selected_propositions, aes(x = feat_type, y = difficulty_exp_error)) +
  geom_point(aes(color = feat_type), shape = 1, position = position_jitter(width = 0.1)) +
  geom_point(stat = "summary", fun.y = "mean", size = 4) +
  facet_wrap("correct_response") +
  base_theme

ggplot(selected_propositions, aes(x = feat_type, y = difficulty_exp_rt)) +
  geom_point(aes(color = feat_type), shape = 1, position = position_jitter(width = 0.1)) +
  geom_point(stat = "summary", fun.y = "mean", size = 4) +
  facet_wrap("correct_response") +
  base_theme

# ---- export-subset
selected_propositions %>%
  select(proposition_id) %>%
  write.csv("balanced_propositions.csv", row.names = FALSE)
