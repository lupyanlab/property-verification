#' Label the outlier subjects and items in the data.
#' @export
label_outliers <- function(frame) {
  subj_map <- create_subj_map(frame)
  prop_map <- create_prop_map(frame)

  frame %>%
    left_join(subj_map) %>%
    left_join(prop_map)
}

#' Create a map of subjects to outlier status.
#' @export
create_subj_map <- function(frame) {
  subjs <- data.frame(subj_id = unique(frame$subj_id),
                      stringsAsFactors = FALSE)

  subjs$is_subj_outlier <- 0
  subjs$subj_outlier_reason <- ""

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

  label_outlier_subjs <- function(subjs, outlier_subjs, reason) {
    label_outliers(subjs, "subj_id", outlier_subjs, reason,
                   outlier_col = "is_subj_outlier",
                   reason_col = "subj_outlier_reason")
  }

  subjs <- subjs %>%
    label_outlier_subjs(wrong_conditions, "Monitor settings were incorrect.") %>%
    label_outlier_subjs(bad_compliance, "RAs reported bad compliance.") %>%
    label_outlier_subjs(not_understand, "Reported not understanding some questions.")

  subjs
}

#' Create a map of propositions to outlier status.
#' @export
create_prop_map <- function(frame) {
  props <- data.frame(proposition_id = unique(frame$proposition_id),
                      stringsAsFactors = FALSE)

  ambiguous_propositions <- determine_ambiguous_propositions()
  bad_baseline_performance <- determine_bad_baseline_performance(frame)

  label_outlier_props <- function(props, prop_ids, reason) {
    label_outliers(props, "proposition_id", prop_ids, reason,
                   outlier_col = "is_prop_outlier",
                   reason_col = "prop_outlier_reason")
  }

  props <- props %>%
    label_outlier_props(ambiguous, "Proposition was ambiguous") %>%
    label_outlier_props(bad_baseline_performance, "Bad baseline performance")

  props
}

#' Determine which propositions were ambiguous based on norming responses.
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
    mutate(ambiguity = ifelse(p.value < 0.05, "agree", "ambiguous")) %>%
    select(proposition_id, ambiguity)

  proposition_classification
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

label_outliers <- function(map, id_col, outlier_ids, reason,
                           outlier_col = "is_outlier",
                           reason_col = "reason") {
  is_outlier <- map[[id_col]] %in% outlier_ids
  map[is_outlier, outlier_col] <- 1
  map[is_outlier, reason_col] <- reason
  map
}
