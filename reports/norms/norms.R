# ---- norms
library(dplyr)
library(broom)

library(propertyverificationdata)
data("norms")

norms <- norms %>%
  recode_feat_type %>%
  recode_norms

norms %>%
  group_by(feat_type) %>%
  summarize(
    imagery_mean = mean(imagery_mean),
    facts_mean = mean(facts_mean),
    difficulty_mean = mean(difficulty_mean),
    truth_mean = mean(truth_mean),
    senses_mean = mean(senses_mean, na.rm = TRUE)
  )

imagery_mod <- lm(imagery_mean ~ feat_c, data = norms)
tidy(imagery_mod)

facts_mod <- lm(facts_mean ~ feat_c, data = norms)
tidy(facts_mod)

difficulty_mod <- lm(difficulty_mean ~ feat_c, data = norms)
tidy(difficulty_mod)

truth_mod <- lm(truth_mean ~ feat_c, data = norms)
tidy(truth_mod)

senses_mod <- lm(senses_mean ~ feat_c, data = norms)
tidy(senses_mod)
cor.test(norms$senses_mean, norms$imagery_mean) %>% tidy
