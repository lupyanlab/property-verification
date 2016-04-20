#' Create a map of subjects to outliers.
#' @export
create_subj_map <- function(frame) {
  subjs <- data.frame(subj_id = unique(frame$subj_id),
                      stringsAsFactors = FALSE)

  subjs$is_outlier <- 0
  subjs$reason <- ""

  label_outliers <- function(subjs, outlier_ids, reason) {
    is_outlier <- subjs$subj_id %in% outlier_ids
    subjs[is_outlier, "is_outlier"] <- 1
    subjs[is_outlier, "reason"] <- reason
    subjs
  }

  wrong_conditions <- frame %>%
    filter(exp_run == 4, computer == "LL-George", seed < 137) %>%
    .$subj_id %>%
    unique

  bad_compliance <- c(
    "PV123",
    "MWPF320",
    "MWPF323",
    "MWPF326"
  )

  not_understand <- c("MWPR127", "MWPR145")

  subjs <- subjs %>%
    label_outliers(wrong_conditions, "Monitor settings were incorrect.") %>%
    label_outliers(bad_compliance, "RAs reported bad compliance.") %>%
    label_outliers(not_understand, "Reported not understanding some questions.")

  subjs
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
