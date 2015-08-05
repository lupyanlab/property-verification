source("scripts/question_first_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")

# Create contrast variables
# -------------------------
question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

# Drop outlier subjects
# ---------------------
question_first <- filter(question_first, subj_id != question_first_outliers)

# Correlation between RTs and accuracies
summarized <- question_first %>% group_by(subj_id, mask_type) %>%
  summarize(rt = mean(rt, na.rm = TRUE),
            accuracy = mean(is_correct, na.rm = TRUE))
cor(summarized$rt, summarized$accuracy)
