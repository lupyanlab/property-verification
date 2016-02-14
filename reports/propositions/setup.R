# ---- setup
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverificationdata")
library(propertyverificationdata)
data(norms_responses) # raw responses given in the norming study
data(norms)           # responses summarized by proposition
data(question_first)  # experiment data

library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(scales)
library(lme4)
library(stringr)

set.seed(253)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data

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

scale_x_difficulty <- scale_x_continuous("Rated difficulty of proposition (z-score)")
scale_y_error <- scale_y_continuous("Error rate in experiment", labels = percent)
scale_y_rt <- scale_y_continuous("Reaction time in experiment (msec)")

choose_scale <- function(key) {
  if(grepl("error", key)) {
    return(scale_y_error)
  } else if(grepl("rt", key)) {
    return(scale_y_rt)
  } else {
    stop(paste("scale key", key, "not found"))
  }
}

base_theme <- theme_minimal(base_size = 14) +
  theme(axis.ticks = element_blank())

