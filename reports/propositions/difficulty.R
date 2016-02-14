source("reports/propositions/setup.R")

# ---- difficulty
baseline_performance <- question_first %>%
  filter(mask_type == "nomask")

prop_error_coefs <- baseline_performance %>%
  group_by(proposition_id) %>%
  do(mod = glm(is_error ~ 1, family = "binomial", data = .)) %>%
  tidy(mod) %>%
  mutate(baseline_difficulty = ifelse(
    (estimate > 0) | (p.value > 0.05),
    "too_hard", "easy")) %>%
  label_ambiguous_propositions

ggplot(prop_error_coefs, aes(x = estimate, y = p.value)) +
  geom_point(aes(color = agreement, shape = baseline_difficulty)) +
  geom_hline(yintercept = 0.05, lty = 2, alpha = 0.4) +
  scale_shape_manual(values = c(16, 1)) +
  base_theme

baseline_difficulty <- prop_error_coefs %>%
  select(proposition_id, baseline_difficulty)