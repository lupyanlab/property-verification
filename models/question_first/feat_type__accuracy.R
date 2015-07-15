library(lme4)
source("scripts/question_first_data.R")
source("scripts/contrasts.R")

question_first <- recode_mask_type(question_first)
question_first <- recode_feat_type(question_first)

# Feature type (visual or nonvisual)
# ----------------------------------
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = question_first)
summary(feat_type_error_mod)
