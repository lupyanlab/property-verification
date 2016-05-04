# ---- setup
library(dplyr)
library(ggplot2)
library(scales)
library(lme4)
library(broom)

plot_amount_of_knowledge <- function(frame, knowledge_type) {
  ggplot(frame, aes_string(x = knowledge_type, y = "is_error", color = "mask_type")) +
    geom_smooth(method = "glm", se = FALSE)
}

# ---- data
library(propertyverificationdata)
data(question_first)
data(norms)

all_props <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_subj_outlier == 0) %>%
  left_join(norms)

question_first <- filter(all_props, is_prop_outlier == 0)

# ---- imagery-mod
imagery_mod <- glmer(is_error ~ imagery_z * mask_c + (1|subj_id),
                     family = "binomial", data = question_first)

# ---- imagery-mod-all
imagery_mod_all <- glmer(is_error ~ imagery_z * mask_c + (1|subj_id),
                         family = "binomial", data = all_props)

# ---- amount-of-visual
plot_amount_of_knowledge(question_first, knowledge_type = "imagery_z") +
  ggtitle("All runs")

# ---- amount-of-visual-by-exp
plot_amount_of_knowledge(question_first, knowledge_type = "imagery_z") +
  facet_wrap("exp_run_label", ncol = 1)

# ---- amount-of-visual-all-props
plot_amount_of_knowledge(all_props, knowledge_type = "imagery_z") +
  ggtitle("All runs, all propositions")

# --- amount-of-visual-all-props-by-exp
plot_amount_of_knowledge(all_props, knowledge_type = "imagery_z") +
  facet_wrap("exp_run_label", ncol = 1)

# ---- facts-mod
facts_mod <- glmer(is_error ~ facts_z * mask_c + (1|subj_id),
                   family = "binomial", data = question_first)

# ---- amount-of-nonvisual
plot_amount_of_knowledge(question_first, knowledge_type = "facts_z") +
  ggtitle("All runs")

# ---- amount-of-nonvisual-by-exp
plot_amount_of_knowledge(question_first, knowledge_type = "facts_z") +
  facet_wrap("exp_run_label", ncol = 1)

# ---- cor
props <- create_prop_map()
norms <- left_join(norms, props)

ggplot(norms, aes(x = feat_type, y = imagery_z, color = factor(is_prop_outlier))) +
  geom_point(stat = "summary", fun.y = "mean", size = 4) +
  geom_point(aes(group = proposition_id, color = factor(is_prop_outlier)), stat = "summary", fun.y = "mean",
             position = position_jitter(0.1, 0),
             shape = 1, size = 1, alpha = 0.5)