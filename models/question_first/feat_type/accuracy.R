source("scripts/question_first_data.R")
source("scripts/contrasts.R")
library(lme4)

question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = question_first)
summary(feat_type_error_mod)
