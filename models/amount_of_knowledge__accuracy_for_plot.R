source("scripts/all_data.R")

library(lme4)
library(dplyr)

source("scripts/contrasts.R")
source("scripts/outliers.R")
source("scripts/report_stats.R")

# Create contrast variables
# -------------------------
property_verification <- recode_mask_type(property_verification)
property_verification <- recode_exp(property_verification)

# Drop outlier subjects
# ---------------------
property_verification <- filter(property_verification,
                                subj_id %nin% question_first_outliers,
                                subj_id %nin% cue_first_outliers)


# Imagery by mask
imagery_mod <- glmer(is_error ~ mask_c * imagery_mean + (1|subj_id),
                     family = binomial, data = property_verification)

# Facts by mask
facts_mod <- glmer(is_error ~ mask_c * facts_mean + (1|subj_id),
                   family = binomial, data = property_verification)
