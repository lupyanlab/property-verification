#' Label the outlier subjects and items in the data.
#'
#' Outliers are labeled but no values are changed or dropped. To remove
#' outliers, filter them out.
#'
#' > label_outliers(question_first) %>% filter(is_outlier == FALSE)
#'
#' @export
label_outliers <- function(frame) {
  subj_map <- create_subj_map(frame)
  prop_map <- create_prop_map(frame)

  frame %>%
    left_join(subj_map) %>%
    left_join(prop_map) %>%
    mutate(is_outlier = any(is_subj_outlier, is_prop_outlier))
}

#' Create a map of subjects to outlier status.
#' @export
create_subj_map <- function(frame) {
  subjs <- data.frame(subj_id = unique(frame$subj_id),
                      stringsAsFactors = FALSE)

  # Innocent until proven guilty.
  subjs$is_subj_outlier <- 0
  subjs$subj_outlier_reason <- ""

  # These subjects had or might have had incorrect monitor settings.
  wrong_conditions <- frame %>%
    filter(exp_run == 4, computer == "LL-George", seed < 137) %>%
    .$subj_id %>%
    unique

  # The RAs reported that these subjects were not compliant.
  bad_compliance <- c(
    "PV123",
    "MWPF320",
    "MWPF323",
    "MWPF326"
  )

  # These subjects responded in the questionnaire that they didn't
  # understand some of the questions.
  not_understand <- c("MWPR127", "MWPR145")

  # Label the outliers with reasons.
  # For subjects labeled outliers for multiple reasons
  # only the last reason is kept.
  subjs <- subjs %>%
    label_outlier_subjs(wrong_conditions, "Monitor settings were incorrect.") %>%
    label_outlier_subjs(bad_compliance, "RAs reported bad compliance.") %>%
    label_outlier_subjs(not_understand, "Reported not understanding some questions.")

  subjs
}

#' Create a map of propositions to outlier status.
#'
#' @import dplyr
#' @export
create_prop_map <- function(frame) {
  props <- data.frame(proposition_id = unique(frame$proposition_id),
                      stringsAsFactors = FALSE)

  # Innocent until proven guilty.
  props$is_prop_outlier <- 0
  props$prop_outlier_reason <- ""

  ambiguous_propositions <- determine_ambiguous_propositions()
  bad_baseline_performance <- determine_bad_baseline_performance(frame)

  # Label the outliers with reasons.
  # For propositions labeled outliers for multiple reasons
  # only the last reason is kept.
  props <- props %>%
    label_outlier_props(bad_baseline_performance, "Bad baseline performance") %>%
    label_outlier_props(ambiguous_propositions, "Proposition was ambiguous")

  props
}

#' Determine which propositions were ambiguous based on norming responses.
#'
#' Warning! Anything named `norms_responses` may be clobbered from the env.
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

  # Identify ambiguous propositions as those that do not differ
  # significantly from the center of the truth scale.
  ambiguous_propositions <- norms_mods %>%
    tidy(diff_mod) %>%
    ungroup %>%
    filter(p.value > 0.05) %>%
    .$proposition_id

  rm(norms_responses)

  ambiguous_propositions
}

#' Determine which propositions had bad baseline performance.
#'
#' @param question_first The question_first data in tidy format.
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
    filter(baseline_difficulty == "too_hard") %>%
    .$proposition_id

  baseline_performance
}


#' Label the outlier rows in a data.frame of subjects or items with reasons.
label_outlier_map <- function(map, id_col, outlier_ids, reason,
                              outlier_col = "is_outlier",
                              reason_col = "reason") {
  is_outlier <- map[[id_col]] %in% outlier_ids
  map[is_outlier, outlier_col] <- 1
  map[is_outlier, reason_col] <- reason
  map
}


#' Partial wrapper around `label_outlier_map` for subjects.
label_outlier_subjs <- function(subjs, outlier_subjs, reason) {
  label_outlier_map(subjs, "subj_id", outlier_subjs, reason,
                    outlier_col = "is_subj_outlier",
                    reason_col = "subj_outlier_reason")
}


#' Partial wrapper around `label_outlier_map` for propositions.
label_outlier_props <- function(props, prop_ids, reason) {
  label_outlier_map(props, "proposition_id", prop_ids, reason,
                    outlier_col = "is_prop_outlier",
                    reason_col = "prop_outlier_reason")
}
