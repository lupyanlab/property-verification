# ---- setup
library(dplyr)
library(broom)
library(ggplot2)

# ---- data
library(propertyverificationdata)
data(question_first)
question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_feat_type %>%
  recode_mask_type

# ---- subj-effects
subj_mods <- question_first %>%
  group_by(exp_run, computer, subj_id) %>%
  do(mod = glm(is_error ~ feat_c * mask_c, family = "binomial", data = .))

subj_effects <- subj_mods %>%
  tidy(mod) %>%
  filter(term == "feat_c:mask_c")

ggplot(subj_effects, aes(x = estimate)) +
  geom_density() +
  coord_cartesian(xlim = c(-50, 50))

# ---- subj-effects-by-computer
ggplot(subj_effects, aes(x = estimate, color = computer)) +
  geom_density() +
  coord_cartesian(xlim = c(-50, 50))

ggplot(subj_effects, aes(x = computer, y = estimate, color = computer)) +
  geom_point(stat = "summary", fun.y = "mean")