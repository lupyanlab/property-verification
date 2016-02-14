source("reports/propositions/setup.R")

# ---- ambiguous
# For each proposition, calculate whether or not the normative truth value
# is significantly different from 0, which would mean that the proposition
# does indeed have a normatively correct response.

norms_mods <- norms_responses %>%
  # select only the truth responses and label the value column appropriately
  filter(measure == "truth") %>%
  select(-measure) %>%
  rename(truth = value) %>%
  # fit separate models to each part
  group_by(proposition_id) %>%
  do(diff_mod = lm(truth ~ 1, data = .))

# classify the results of the models based on whether people agree
# with the classification or they find the proposition ambiguous
proposition_classification <- norms_mods %>%
  tidy(diff_mod) %>%
  ungroup %>%
  mutate(
    agreement = ifelse(p.value < 0.05, "agree", "ambiguous"),
    norm_response = ifelse(agreement == "ambiguous", NA,
                           ifelse(estimate > 0, "yes", "no"))
  ) %>%
  select(-term) %>%
  left_join(norms)

scale_x_truth <- scale_x_continuous(
  "Truth of proposition",
  breaks = -2:2,
  labels = c("Definitely no", "Probably no", "Maybe", "Probably yes", "Definitely yes")
)

ggplot(proposition_classification, aes(x = estimate, y = p.value)) +
  geom_point(aes(color = agreement), shape = 1) +
  geom_hline(yintercept = 0.05, lty = 2, alpha = 0.4) +
  scale_x_truth +
  base_theme +
  ggtitle("Which propositions were ambiguous?")

# ---- save-ambiguous
ambiguous_propositions <- filter(proposition_classification, agreement == "ambiguous")
write.csv(ambiguous_propositions, "ambiguous_propositions.csv", row.names = FALSE)