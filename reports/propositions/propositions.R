# ---- setup
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(scales)
library(lme4)
library(stringr)

set.seed(253)

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
  
# ---- data
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverificationdata")
library(propertyverificationdata)
data(norms_responses)
data(norms)

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
  mutate(
    agreement = ifelse(p.value < 0.05, "agree", "ambiguous"),
    norm_response = ifelse(agreement == "ambiguous", NA,
                           ifelse(estimate > 0, "yes", "no"))
  ) %>%
  select(-term) %>%
  left_join(norms)

scale_x_truth <- scale_x_continuous(
  "Truth of proposition",
  breaks = -2:2,
  labels = c("Definitely no", "Probably no", "Maybe", "Probably yes", "Definitely yes")
)

ggplot(proposition_classification, aes(x = estimate, y = p.value)) +
  geom_point(aes(color = agreement), shape = 1) +
  geom_hline(yintercept = 0.05, lty = 2, alpha = 0.4) +
  scale_x_truth +
  base_theme +
  ggtitle("Which propositions were ambiguous?")

# ---- incorrect-code
# Of the propositions for which people think there is a normatively correct
# response, are there any that were incorrectly coded in the experiment?

table(proposition_classification[, c("correct_response", "norm_response", "agreement")],
      useNA = "ifany")

# ---- save-ambiguous
ambiguous_propositions <- filter(proposition_classification, agreement == "ambiguous")
write.csv(ambiguous_propositions, "ambiguous_propositions.csv", row.names = FALSE)

# ---- difficulty
data(question_first)

proposition_difficulty <- question_first %>%
  tidy_property_verification_data %>%
  filter(mask_type == "nomask") %>%
  group_by(proposition_id) %>%
  summarize(
    num_trials = n(),
    num_errors = sum(is_error, na.rm = TRUE),
    difficulty_exp_error = mean(is_error, na.rm = TRUE),
    difficulty_exp_rt = mean(rt[is_correct == 1])
  ) %>%
  left_join(norms) %>%
  select(
    proposition_id,
    correct_response,
    feat_type,
    n_norms = difficulty_count,
    difficulty_norms = difficulty_z,
    n_exp = num_trials,
    difficulty_exp_error,
    difficulty_exp_rt
  )

plot_correlation <- function(frame, y, group_var = NULL) {
  scale_y <- choose_scale(y)
  ggplot(frame, aes_string(x = "difficulty_norms", y = y)) +
    geom_point(aes_string(color = group_var), shape = 1) +
    geom_smooth(aes_string(color = group_var), method = "lm") +
    scale_x_difficulty +
    scale_y +
    base_theme +
    ggtitle("Correlation between subjective and objective\nratings of proposition difficulty")
}

plot_correlation(proposition_difficulty, "difficulty_exp_error")
plot_correlation(proposition_difficulty, "difficulty_exp_rt")

# ---- difficulty-by-correctness
plot_correlation(proposition_difficulty, "difficulty_exp_error",
                 group_var = "correct_response")
plot_correlation(proposition_difficulty, "difficulty_exp_rt",
                 group_var = "correct_response")

# ---- difficulty-by-feat-type
plot_correlation(proposition_difficulty, "difficulty_exp_error",
                 group_var = "feat_type")
plot_correlation(proposition_difficulty, "difficulty_exp_rt",
                 group_var = "feat_type")