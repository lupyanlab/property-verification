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
imagery_error_mod <- glmer(is_error ~ mask_c * imagery_mean + (1|subj_id),
                           data = question_first, family = binomial)

# Interference by amount of nonvisual knowledge (facts)
facts_error_mod <- glmer(is_error ~ mask_c * facts_mean + (1|subj_id),
                         data = question_first, family = binomial)
