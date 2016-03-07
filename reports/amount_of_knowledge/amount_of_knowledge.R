# ---- setup
library(dplyr)
library(ggplot2)
library(scales)
library(lme4)

plot_amount_of_knowledge <- function(frame, knowledge_type) {
  ggplot(frame, aes_string(x = knowledge_type, y = "is_error", color = "mask_type")) +
    geom_smooth(method = "glm", se = FALSE)
}

# ---- data
library(propertyverificationdata)
data(question_first)
data(norms)
question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_mask_type %>%
  recode_exp_run %>%
  left_join(norms)

# ---- amount-of-visual
plot_amount_of_knowledge(question_first, knowledge_type = "imagery_z") +
  ggtitle("All runs")

# ---- amount-of-visual-by-exp
plot_amount_of_knowledge(question_first, knowledge_type = "imagery_z") +
  facet_wrap("exp_run_label", ncol = 1)

# ---- amount-of-nonvisual
plot_amount_of_knowledge(question_first, knowledge_type = "facts_z") +
  ggtitle("All runs")

# ---- amount-of-nonvisual-by-exp
plot_amount_of_knowledge(question_first, knowledge_type = "facts_z") +
  facet_wrap("exp_run_label", ncol = 1)