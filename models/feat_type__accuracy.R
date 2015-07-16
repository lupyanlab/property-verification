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
property_verification <- filter(property_verification, subj_id != "MWPF129")

# Predict error from
# --------------------
error_mod <- glmer(is_error ~ mask_c * feat_c * exp_c + (1|subj_id),
                   family = binomial, data = property_verification)
summary(error_mod)
