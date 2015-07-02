library(lme4)
source("scripts/cue_first_data.R")
source("scripts/contrasts.R")

cue_first <- recode_mask_type(cue_first)


# Amount of visual knowledge (imagery)
# ------------------------------------
imagery_error_mod <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id),
                           data = cue_first, family = binomial)
summary(imagery_error_mod)


# Amount of nonvisual knowledge (facts)
# -------------------------------------
facts_error_mod <- glmer(is_error ~ mask_c * facts_z + (1|subj_id),
                         data = cue_first, family = binomial)
summary(facts_error_mod)
