source("reports/propositions/setup.R")

# ---- subj-obj-diff

proposition_difficulty_labeled <- difficulty_measures %>%
  left_join(select(norms, proposition_id, feat_type, correct_response))

ggplot(proposition_difficulty_labeled, aes(x = difficulty_exp_error)) +
  geom_histogram(aes(fill = feat_type)) +
  base_theme

plot_correlation <- function(frame, y, group_var = NULL) {
  scale_y <- choose_scale(y)
  ggplot(frame, aes_string(x = "difficulty_norms", y = y)) +
    geom_point(aes_string(color = group_var), shape = 1) +
    geom_smooth(aes_string(color = group_var), method = "lm") +
    scale_x_difficulty +
    scale_y +
    base_theme +
    ggtitle("Subjective and objective proposition difficulty")
}

plot_correlation(proposition_difficulty_labeled, "difficulty_exp_error")
plot_correlation(proposition_difficulty_labeled, "difficulty_exp_rt")

# ---- subj-obj-diff-by-correctness
plot_correlation(proposition_difficulty_labeled, "difficulty_exp_rt",
                 group_var = "correct_response")

# ---- subj-obj-diff-by-feat-type
plot_correlation(proposition_difficulty_labeled, "difficulty_exp_rt",
                 group_var = "feat_type")