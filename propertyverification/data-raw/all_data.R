
CLEAR_GLOBAL_ENVIRONMENT <- FALSE
source("scripts/cue_first_data.R")
source("scripts/question_first_data.R")

cue_first$exp <- "cue_first"
question_first$exp <- "question_first"

property_verification <- rbind_list(cue_first, question_first)

rm(list = setdiff(ls(), "property_verification"))