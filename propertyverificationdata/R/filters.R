#' Label the outlier subjects in the sample.
#'
#' @import dplyr
#' @importFrom broom tidy
#' @export
label_outlier_subjects <- function(frame) {
  data(question_first)

  subj_mods <- question_first %>%
    tidy_property_verification_data %>%
    recode_feat_type %>%
    recode_mask_type %>%
    group_by(subj_id) %>%
    do(error_mod = glm(is_error ~ feat_c * mask_c, family = binomial, data = .))

  subj_effects <- subj_mods %>%
    tidy(error_mod) %>%
    ungroup

  outlier_map <- subj_effects %>%
    group_by(subj_id) %>%
    summarize(outlier = any(abs(estimate) > 5))

  frame %>% left_join(outlier_map)
}

#' Label the ambiguity of propositions based on norming data.
#'
#' @import dplyr
#' @importFrom broom tidy
#' @export
label_ambiguous_propositions <- function(frame) {
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

  frame %>% left_join(proposition_classification)
}
