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
cue_first <- recode_feat_type(cue_first)

# Models predicting error rate
# ----------------------------
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = cue_first)
