devtools::load_all("propertyverification")
data(property_verification)

library(lme4)
library(dplyr)


difficulty <- property_verification %>%
  group_by(question_id) %>%
  summarize(
    norm_diff = mean(diff_z),
    exp_diff = mean(is_error[mask_type == "nomask"], na.rm = TRUE),
    
    # save imagery and facts
    imagery_z = mean(imagery_z),
    facts_z = mean(facts_z))

# Correlation between norming experiment difficulty and experiment difficulty
cor(difficulty[,c("norm_diff", "exp_diff")])

# Correlation between imagery, facts, and difficulty
cor(difficulty[,c("norm_diff", "imagery_z", "facts_z")])

property_verification <- merge(property_verification, difficulty[,c("question_id", "exp_diff")])

library(lme4)
source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")

property_verification <- recode_mask_type(property_verification)

# Norming-context difficulty
property_verification <- filter(property_verification,
                                subj_id %nin% question_first_outliers,
                                subj_id %nin% cue_first_outliers)

amount_of_knowledge_norm_diff <- glmer(is_error ~ mask_c * imagery_z + diff_z + (1|subj_id), 
                                       family = binomial,
                                       data = filter(property_verification, exp == "question_first"))
summary(amount_of_knowledge_norm_diff)

# Experiment-context difficulty
amount_of_knowledge_exp_diff <- glmer(is_error ~ mask_c * imagery_z + exp_diff + (1|subj_id), 
                                      family = binomial,
                                      data = filter(property_verification, exp == "question_first"))
summary(amount_of_knowledge_exp_diff)
report_glmer_effect(amount_of_knowledge_exp_diff, "mask_c:imagery_z")


# Experiment-context difficulty by mask
exp_diff_by_mask <- glmer(is_error ~ mask_c * (imagery_z + exp_diff) + (1|subj_id),
                          family = binomial,
                          data = filter(property_verification, exp == "question_first"))
summary(exp_diff_by_mask)
report_glmer_effect(exp_diff_by_mask, "mask_c:exp_diff")
