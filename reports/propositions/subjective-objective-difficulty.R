source("reports/propositions/setup.R")

# ---- subj-obj-diff

# Summarize baseline task performance for each proposition
proposition_difficulty <- question_first %>%
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

ggplot(proposition_difficulty, aes(x = difficulty_exp_error)) +
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

plot_correlation(proposition_difficulty, "difficulty_exp_error")
plot_correlation(proposition_difficulty, "difficulty_exp_rt")

# ---- subj-obj-diff-by-correctness
plot_correlation(proposition_difficulty, "difficulty_exp_rt",
                 group_var = "correct_response")

# ---- subj-obj-diff-by-feat-type
plot_correlation(proposition_difficulty, "difficulty_exp_rt",
                 group_var = "feat_type")