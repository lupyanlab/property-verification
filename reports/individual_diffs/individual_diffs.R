# ---- setup
library(dplyr)
library(ggplot2)
library(lme4)
library(broom)
library(stringr)

#' Extract question id from proposition_id column
label_question_id <- function(frame) {
  proposition_ids <- str_split_fixed(frame$proposition_id,
                                     pattern = ":", n = 2)
  frame$question_id <- proposition_ids[, 1]
  frame
}

# ---- data
library(propertyverificationdata)
data(question_first)
data(individual_diffs)
data(norms)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_outlier == 0)


# Create imagery column from propositions
proposition_imagery <- left_join(question_first, individual_diffs)


# Create imagery column from questions
individual_diffs_questions <- label_question_id(individual_diffs) %>%
  group_by(subj_id, question_id) %>%
  summarize(
    imagery = mean(imagery),
    n = n()
  )

question_imagery <- label_question_id(question_first) %>%
  left_join(individual_diffs_questions)

# ---- proposition-imagery-mod
proposition_imagery_mod <- glmer(
  is_error ~ imagery * mask_c + (1|subj_id),
  family = "binomial", data = proposition_imagery
)

# ----- question-imagery-mod
question_imagery_mod <- glmer(
  is_error ~ imagery * mask_c + (1|subj_id),
  family = "binomial", data = question_imagery
)

# ---- cor
imagery_norms <- norms %>%
  select(feat_type, proposition_id, imagery_norms = imagery_z)

imagery_individ <- individual_diffs %>%
  group_by(proposition_id) %>%
  summarize(imagery_individ = mean(imagery, na.rm = TRUE))

imagery_cor <- left_join(imagery_norms, imagery_individ)

set.seed(153)
ggplot(imagery_cor, aes(x = imagery_norms, y = imagery_individ)) +
  geom_point(aes(color = feat_type),
             position = position_jitter(0.01, 0.01)) +
  geom_smooth(method = "lm", se = FALSE,
              color = "black")

# ---- cor-prop-quest
prop_diffs <- individual_diffs %>%
  group_by(proposition_id) %>%
  summarize(prop_imagery = mean(imagery, na.rm = TRUE)) %>%
  label_question_id

quest_diffs <- individual_diffs %>%
  label_question_id %>%
  group_by(question_id) %>%
  summarize(quest_imagery = mean(imagery, na.rm = TRUE))

compare <- left_join(prop_diffs, quest_diffs)

set.seed(642)
ggplot(compare, aes(prop_imagery, quest_imagery)) +
  geom_point(position = position_jitter(0.01, 0.01))
