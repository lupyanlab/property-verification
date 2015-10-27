devtools::load_all("propertyverification")
data(cue_first)

library(lme4)
library(dplyr)

# Remove outlier subjects
# -----------------------
cue_first <- filter(cue_first, subj_id %nin% cue_first_outliers)

# Create contrast variables
# -------------------------
cue_first <- recode_mask_type(cue_first)

# Models prediction error rate from continuous measures
# -----------------------------------------------------
# Interference by amount of visual knowledge (imagery)
imagery_error_mod <- glmer(is_error ~ mask_c * imagery_mean + (1|subj_id),
                           data = cue_first, family = binomial)

# Interference by amount of nonvisual knowledge (facts)
facts_error_mod <- glmer(is_error ~ mask_c * facts_mean + (1|subj_id),
                         data = cue_first, family = binomial)
