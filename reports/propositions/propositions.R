# ---- setup
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(lme4)
library(stringr)

#' Get the coefficients from an lme4 mod in tidy format.
tidy_lmer_coefs <- function(mod, grouping_var = 1) {
  coefs_list <- coef(mod)
  if(!(grouping_var %in% names(coefs_list))) {
    grouping_var <- names(coefs_list)[grouping_var]
  }
  coefs_frame <- coefs_list[[grouping_var]] %>%
    as.data.frame
  
  # reset index
  coefs_frame[grouping_var] <- rownames(coefs_frame)
  rownames(coefs_frame) <- NULL
  
  # rename intercept column
  if("(Intercept)" %in% colnames(coefs_frame)) {
    coefs_frame <- dplyr::rename(coefs_frame, intercept = `(Intercept)`)
  }
  
  coefs_frame
}

# ---- data
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverificationdata")
library(propertyverificationdata)
data(norms_responses)
data(norms)
data(question_first)

# ---- truth-agreement

# For each proposition, calculate whether or not the normative truth value
# is significantly different from 0, which would mean that the proposition
# does indeed have a normatively correct response.

norms_mods <- norms_responses %>%
  # select only the truth responses and label the value column appropriately
  filter(measure == "truth") %>%
  select(-measure) %>%
  rename(truth = value) %>%
  # fit separate models to each part
  group_by(proposition_id) %>%
  do(diff_mod = lm(truth ~ 1, data = .))

# classify the results of the models based on whether people agree
# with the classification or they find the proposition ambiguous
proposition_classification <- norms_mods %>%
  tidy(diff_mod) %>%
  ungroup %>%
  mutate(agreement = ifelse(p.value < 0.05, "agree", "ambiguous")) %>%
  select(-term)

ggplot(proposition_classification, aes(x = estimate, y = p.value)) +
  geom_point(aes(color = agreement))

# ---- incorrect-code
# Of the propositions for which people think there is a normatively correct
# response, are there any that were incorrectly coded in the experiment?

# norm response is yes, no, or NA for ambiguous questions
proposition_classification$norm_response <- ifelse(
  proposition_classification$agreement == "ambiguous", NA,
    ifelse(proposition_classification$estimate > 0, "yes", "no")
)

proposition_classification <- merge(proposition_classification, norms)

proposition_classification <- proposition_classification %>%
  mutate(truth_verified = correct_response == norm_response)

table(proposition_classification$truth_verified, useNA = "ifany")