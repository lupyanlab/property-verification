devtools::load_all("propertyverification")
data(question_first)

library(dplyr)
library(lme4)

# Create contrast variables
# -------------------------
question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

# Drop outlier subjects
# ---------------------
question_first <- filter(question_first, subj_id %nin% question_first_outliers)

# Models predicting error rate
# ----------------------------
# Feature type (visual or nonvisual)
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = question_first)
