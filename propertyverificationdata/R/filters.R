
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
