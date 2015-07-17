source("scripts/all_data.R")

library(lme4)
library(dplyr)

source("scripts/contrasts.R")

# Create contrast variables
# -------------------------
property_verification <- recode_mask_type(property_verification)
property_verification <- recode_feat_type(property_verification)
property_verification <- recode_exp(property_verification)

# Drop outlier subjects
# ---------------------
property_verification <- filter(property_verification,
                                subj_id != "MWPF129",
                                subj_id != "MWP214")

# 
imagery_mod <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id),
                   family = binomial, data = property_verification)
summary(imagery_mod)
