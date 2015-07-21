source("scripts/all_data.R")

difficulty <- property_verification %>%
  group_by(question_id) %>%
  summarize(
    norm_diff = mean(diff_z),
    exp_diff = mean(is_error[mask_type == "nomask"], na.rm = TRUE))

cor(difficulty[,c("norm_diff", "exp_diff")])

property_verification <- merge(property_verification, difficulty[,c("question_id", "exp_diff")])

library(lme4)
source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")
property_verification <- recode_mask_type(property_verification)
amount_of_knowledge <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id), 
                             family = binomial,
                             data = filter(property_verification, exp == "question_first"))
summary(amount_of_knowledge)