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
data(question_first)
data(norms)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_mask_type %>%
  recode_feat_type %>%
  recode_exp_run %>%
  left_join(norms)

# ---- proposition-error
baseline_error_from_means <- question_first %>%
  group_by(proposition_id) %>%
  summarize(
    n = n(),
    baseline_error = mean(is_error[mask_type == "mask"], na.rm = TRUE)
  )

question_first$nomask <- ifelse(question_first$mask_type == "nomask", 1, 0)

proposition_ranef_mod <- glmer(
  is_error ~ 1 + (nomask|proposition_id),
  family = binomial, data = question_first
)

proposition_ranef_coefs <- tidy_lmer_coefs(proposition_ranef_mod)

baseline_error_from_model <- proposition_ranef_coefs %>%
  transmute(proposition_id, baseline_error = intercept)

error_compare <- merge(baseline_error_from_means, baseline_error_from_model,
      by = "proposition_id", suffixes = c("_means", "_model"))
  #gather(error_type, error_rate, -(proposition_id:n))

ggplot(error_compare, aes(x = baseline_error_means, baseline_error_model)) +
  geom_point()