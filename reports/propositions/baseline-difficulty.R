source("reports/propositions/setup.R")

# ---- baseline-difficulty
baseline_performance <- question_first %>%
  filter(mask_type == "nomask")

trials_per_proposition <- baseline_performance %>%
  group_by(correct_response, proposition_id) %>%
  summarize(num_trials = n())

ggplot(trials_per_proposition, aes(x = num_trials)) +
  geom_histogram(aes(fill = correct_response)) +
  base_theme

baseline_error_mods <- baseline_performance %>%
  group_by(proposition_id) %>%
  do(mod = glm(is_error ~ 1, family = "binomial", data = .))

prop_error_coefs <- baseline_error_mods %>%
  tidy(mod) %>%
  mutate(baseline_difficulty = ifelse(
    (estimate > 0) | (p.value > 0.05),
    "too_hard", "easy")) %>%
  label_ambiguous_propositions

# Propositions for which all responses were correct have a very negative
# estimate, but a p.value of 1.00, should be labeled as "easy"
prop_error_coefs[prop_error_coefs$estimate < -10, "baseline_difficulty"] <- "easy"

ggplot(prop_error_coefs, aes(x = estimate, y = p.value)) +
  geom_point(aes(color = agreement, shape = baseline_difficulty),
             position = position_jitter(height = 0.01, width = 0.5)) +
  geom_hline(yintercept = 0.05, lty = 2, alpha = 0.4) +
  scale_shape_manual(values = c(16, 1)) +
  base_theme

# ---- export-difficult
difficult_propositions <- prop_error_coefs %>%
  filter(baseline_difficulty == "too_hard") %>%
  select(proposition_id, baseline_difficulty)
write.csv(difficult_propositions, "difficult_propositions.csv", row.names = FALSE)