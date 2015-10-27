
source("data-raw/cue_first_data.R")
source("data-raw/question_first_data.R")

cue_first$exp <- "cue_first"
question_first$exp <- "question_first"

property_verification <- rbind_list(cue_first, question_first)

devtools::use_data(property_verification)
