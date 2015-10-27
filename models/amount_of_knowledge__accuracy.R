devtools::load_all("propertyverification")
data(question_first)

library(dplyr)
library(lme4)

question_first <- recode_mask_type(question_first)

# Drop outlier subjects
# ---------------------
question_first <- filter(question_first, subj_id %nin% question_first_outliers)

# Models predicting error rate
# ----------------------------
# Interference by amount of visual knowledge (imagery)
imagery_error_mod <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id),
                           data = question_first, family = binomial)
summary(imagery_error_mod)
report_glmer_effect(imagery_error_mod, "mask_c:imagery_z")
# 0.22 log-odds, 95% CI [0.01, 0.43], p = 0.0357

# Interference by amount of nonvisual knowledge (facts)
facts_error_mod <- glmer(is_error ~ mask_c * facts_z + (1|subj_id),
                         data = question_first, family = binomial)
summary(facts_error_mod)
report_glmer_effect(facts_error_mod, "mask_c:facts_z")
# -0.14 log-odds, 95% CI [-0.33, 0.05], p = 0.1517


# Interference by question difficulty (diff)
diff_error_mod <- glmer(is_error ~ mask_c * diff_z + (1|subj_id),
                        data = question_first, family = binomial)
summary(diff_error_mod)
report_glmer_effect(diff_error_mod, "mask_c:diff_z")
# -0.05 log-odds, 95% CI [-0.23, 0.13], p = 0.5984


# Interference by amount of visual knowledge (imagery)
# controlling for: diff_z
imagery_error_mod_diff <- glmer(is_error ~ mask_c * imagery_z + diff_z + (1|subj_id),
                                data = question_first, family = binomial)
summary(imagery_error_mod_diff)
report_glmer_effect(imagery_error_mod_diff, "mask_c:imagery_z")
# 0.23 log-odds, 95% CI [0.02, 0.45], p = 0.03


# Interference by amount of visual knowledge (imagery)
# controlling for: senses_mean
imagery_error_mod_senses <- glmer(is_error ~ mask_c * imagery_z + senses_mean + (1|subj_id),
                                  data = question_first, family = binomial)
summary(imagery_error_mod_senses)
report_glmer_effect(imagery_error_mod_senses, "mask_c:imagery_z")
# 0.23 log-odds, 95% CI [0.02, 0.43], p = 0.0318