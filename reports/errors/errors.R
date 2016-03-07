# ---- setup
library(dplyr)
library(ggplot2)
library(lme4)
library(scales)
library(broom)

plot_effect_density <- function(frame) {
  ggplot(frame, aes(x = estimate)) +
    geom_density() +
    coord_cartesian(xlim = c(-40, 40))
}

# ---- data
library(propertyverificationdata)
data(question_first)
data(norms)
question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_feat_type %>%
  recode_mask_type %>%
  left_join(norms)

# ---- subj-mods
subj_mods <- question_first %>%
  group_by(exp_run, subj_id) %>%
  do(mod = glm(is_error ~ feat_c * mask_c, family = "binomial", data = .))

subj_effects <- subj_mods %>%
  tidy(mod) %>%
  filter(term == "feat_c:mask_c")

plot_effect_density(subj_effects) +
  ggtitle("Distribution of per-subject\nfeat:mask interaction term")

# ---- prop-mods
prop_mods <- question_first %>%
  group_by(proposition_id) %>%
  do(mod = glm(is_error ~ mask_c, family = "binomial", data = .))

prop_effects <- prop_mods %>%
  tidy(mod) %>%
  filter(term == "mask_c")

plot_effect_density(prop_effects) +
  ggtitle("Distribution of per-proposition\neffect of mask")

# ---- group-subjects

# group subjects by estimate
subj_groups <- subj_effects %>%
  mutate(group = cut(estimate, breaks = c(-Inf, -30, -10, 10, 30, Inf), labels = c("most_negative", "negative", "null", "positive", "most_positive"))) %>%
  select(subj_id, exp_run, group)

table(subj_groups$group)
table(subj_groups[, c("exp_run", "group")])

question_first <- question_first %>%
  left_join(subj_groups)

# ----

ggplot(question_first, aes(x = group, y = is_error)) +
  geom_point(aes(group = subj_id), stat = "summary", fun.y = "mean",
             position = position_jitter(width = 0.2, height = 0.0),
             shape = 1) +
  ggtitle("Mean error per subject\nin each group")

error_counts_by_feat <- question_first %>%
  filter(is_correct == 0) %>%
  count(group, subj_id, feat_type) %>%
  rename(num_errors = n)

ggplot(error_counts_by_feat, aes(x = group, y = num_errors, fill = feat_type)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge")

error_counts_by_mask <- question_first %>%
  filter(is_correct == 0) %>%
  count(group, subj_id, mask_type) %>%
  rename(num_errors = n)

ggplot(error_counts_by_mask, aes(x = group, y = num_errors, fill = mask_type)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge")

# ---- error-counts
error_counts <- question_first %>%
  filter(is_correct == 0) %>%
  count(group, subj_id, feat_type, mask_type) %>%
  rename(num_errors = n)

ggplot(error_counts, aes(x = feat_type, y = num_errors, fill = mask_type)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  facet_wrap("group", nrow = 1)

# ---- glmer-mod
error_mod <- glmer(is_error ~ feat_c * mask_c + (feat_c * mask_c|subj_id),
                   family = "binomial", data = question_first)
summary(error_mod)

# ---- glmer-density
glmer_effects <- tidy(error_mod, effects = "random") %>%
  filter(term == "feat_c:mask_c")
plot_effect_density(glmer_effects)

# ---- glmer-prop-mod
prop_error_mod <- glmer(is_error ~ imagery_z * mask_c + (mask_c|proposition_id),
                        family = "binomial", data = question_first)
summary(prop_error_mod)

# ---- glmer-prop-density
glmer_prop_effects <- tidy(prop_error_mod, effects = "random") %>%
  filter(term == "mask_c")
plot_effect_density(glmer_prop_effects)