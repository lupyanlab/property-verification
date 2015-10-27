devtools::load_all("propertyverification")
data(property_verification)

library(lme4)
library(dplyr)


# Create contrast variables
# -------------------------
property_verification <- recode_mask_type(property_verification)
property_verification <- recode_feat_type(property_verification)
property_verification <- recode_exp(property_verification)

# Drop outlier subjects
# ---------------------
source("scripts/outliers.R")
property_verification <- filter(property_verification,
                                subj_id != cue_first_outliers,
                                subj_id != question_first_outliers)

# Model differences between experiments
rt_mod <- lm(rt ~ exp_c, data = property_verification)
summary(rt_mod)

# 
error_mod <- glm(is_error ~ exp_c, data = property_verification)
summary(error_mod)
