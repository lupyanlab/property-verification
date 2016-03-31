#' Create a map of subjects to outliers.
#' @export
create_outlier_map <- function(frame) {
  subj_mods <- frame %>%
    filter(mask_type == "nomask") %>%
    group_by(exp_run, subj_id) %>%
    do(error_mod = glm(is_error ~ 1, family = binomial, data = .))

  z_score <- function(x) (x - mean(x))/sd(x)

  subj_effects <- subj_mods %>%
    tidy(error_mod) %>%
    ungroup %>%
    select(exp_run, subj_id, baseline_error = estimate) %>%
    mutate(baseline_error_z = z_score(baseline_error))

  outlier_map <- subj_effects %>%
    group_by(subj_id) %>%
    summarize(outlier = abs(baseline_error_z) > 2)

  outlier_map
}

#' Label the ambiguity of propositions based on norming data.
#'
#' @import dplyr
#' @export
label_ambiguous_propositions <- function(frame) {
  proposition_classification <- determine_ambiguous_propositions()
  frame %>% left_join(proposition_classification)
}

#' Determine which propositions were ambiguous.
#'
#' @import dplyr
#' @importFrom broom tidy
determine_ambiguous_propositions <- function() {
  data(norms_responses)

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
    select(proposition_id, agreement)

  proposition_classification
}

#' Label the propositions that were too hard based on baseline performance.
#'
#' @import dplyr
#' @export
label_bad_baseline_performance <- function(frame, question_first) {
  if(missing(question_first)) {
    data(question_first)
    question_first <- question_first %>%
      tidy_property_verification_data
  }
  baseline_performance <- determine_bad_baseline_performance(question_first)
  frame %>% left_join(baseline_performance)
}

#' Determine which propositions had bad baseline performance.
#'
#' @import dplyr
#' @importFrom broom tidy
determine_bad_baseline_performance <- function(question_first) {
  baseline_performance_mods <- question_first %>%
    filter(mask_type == "nomask") %>%
    group_by(proposition_id) %>%
    do(mod = glm(is_error ~ 1, family = "binomial", data = .))

  baseline_performance_coefs <- baseline_performance_mods %>% tidy(mod)

  baseline_performance <- baseline_performance_coefs %>%
    mutate(
      baseline_difficulty = ifelse(
        estimate < -10, "easy",  # everyone got it right
        ifelse((estimate > 0) | (p.value > 0.05), "too_hard", "easy")
      )
    ) %>%
    select(proposition_id, baseline_difficulty)

  baseline_performance
}
