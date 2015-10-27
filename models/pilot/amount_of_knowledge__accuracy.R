# ---- amount_of_knowledge__accuracy ----
devtools::load_all("propertyverification")
data(cue_first)

library(lme4)
library(dplyr)

# Remove outlier subjects
cue_first <- filter(cue_first, subj_id != cue_first_outliers)

# Create contrast variables
cue_first <- recode_mask_type(cue_first)

# Models prediction error rate from continuous measures

# Interference by amount of visual knowledge (imagery)
imagery_error_mod <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id),
                           data = cue_first, family = binomial)
summary(imagery_error_mod)
report_glmer_effect(imagery_error_mod, "mask_c:imagery_z")
# 0.07 log-odds, 95% CI [-0.06, 0.21], p = 0.27


# Interference by amount of nonvisual knowledge (facts)
facts_error_mod <- glmer(is_error ~ mask_c * facts_z + (1|subj_id),
                         data = cue_first, family = binomial)
summary(facts_error_mod)
report_glmer_effect(facts_error_mod, "mask_c:facts_z")
# -0.01 log-odds, 95% CI [-0.13, 0.12], p = 0.9319


# Interference by difficulty (diff)
diff_error_mod <- glmer(is_error ~ mask_c * diff_z + (1|subj_id),
                         data = cue_first, family = binomial)
summary(diff_error_mod)
report_glmer_effect(diff_error_mod, "mask_c:diff_z")
# 

