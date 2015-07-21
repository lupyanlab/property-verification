source("scripts/question_first_data.R")

library(lme4)

source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")

question_first <- recode_mask_type(question_first)

# Drop outlier subjects
# ---------------------
question_first <- filter(question_first, subj_id %nin% question_first_outliers)

# Amount of visual knowledge (imagery)
# ------------------------------------
imagery_error_mod <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id),
                           data = question_first, family = binomial)
summary(imagery_error_mod)

# Amount of visual knowledge (imagery)
# controlling for: diff_z
# ------------------------------------
imagery_error_mod_diff <- glmer(is_error ~ mask_c * imagery_z + diff_z + (1|subj_id),
                                data = question_first, family = binomial)
summary(imagery_error_mod_diff)


# Amount of nonvisual knowledge (facts)
# -------------------------------------
facts_error_mod <- glmer(is_error ~ mask_c * facts_z + (1|subj_id),
                         data = question_first, family = binomial)
summary(facts_error_mod)
