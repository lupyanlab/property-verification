# ---- setup
library(devtools)
load_all("propertyverification")
data(question_first)

library(dplyr)
library(tidyr)

library(ggplot2)
library(scales)

library(lme4)
library(broom)

recode_exp_run <- function(frame) {
  exp_run_map <- data_frame(
    exp_run = c(1, 2),
    exp_run_c = c(-0.5, 0.5),
    exp_run_label = c("First run", "Second run"))
  frame %>% left_join(exp_run_map)
}

question_first <- question_first %>%
  recode_mask_type %>%
  recode_feat_type %>%
  recode_exp_run %>%
  filter(subj_id %nin% question_first_outliers)

run1_data <- filter(question_first, exp_run == 1)
run2_data <- filter(question_first, exp_run == 2)

# Create theme elements used in multiple plots
scale_x_feat_type <- scale_x_discrete("Feature type", labels = c("Encyclopedic", "Visual"))
scale_fill_mask_type <- scale_fill_discrete("Interference", labels = c("None", "Mask"))
base_theme <- theme_minimal() +
  theme(
    axis.ticks = element_blank()
  )

# Creates the main error bar plot for any dataset from the experiment
error_bar_plot <- function(frame) {
  ggplot(frame, aes(x = feat_type, y = is_error, fill = mask_f)) +
    geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
    scale_x_feat_type +
    scale_y_continuous("Error rate", labels = percent) +
    scale_fill_mask_type +
    coord_cartesian(ylim = c(0, 0.14)) +
    base_theme
}

# ---- subjs
subjs <- question_first %>%
  group_by(exp_run, subj_id) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error = mean(is_error, na.rm = TRUE)
  ) %>%
  ungroup %>%
  mutate(
    rank_rt = rank(rt),
    rank_error = rank(error, ties.method = "first")
  )

# subj theme
rank_axis_breaks <- c(1, seq(5, nrow(subjs), by = 5))
scale_x_subj_rank <- scale_x_continuous("Rank", breaks = rank_axis_breaks)
scale_shape_exp_run <- scale_shape_manual("Run", labels = c("first", "second"), values = c(1, 16))
scale_linetype_exp_run <- scale_linetype_discrete("Run", labels = c("first", "second"))
subj_xlim <- c(0.5, nrow(subjs) + 2.5)
subj_rt_ylim <- c(min(subjs$rt) - 100, max(subjs$rt) + 200) %>%
  round(digits = -1)
subj_error_ylim <- c(0, max(subjs$error) + 0.2) %>%
  round(digits = 1)
label_size = 3

subj_ids_by_error <- subjs %>%
  arrange(rank_error) %>%
  .$subj_id

subjs$subj_id <- factor(subjs$subj_id, levels = subj_ids_by_error)

ggplot(subjs, aes(x = rank_error, y = error, color = subj_id, shape = factor(exp_run))) +
  geom_point() +
  geom_text(aes(label = subj_id), angle = 90, hjust = -0.1, size = label_size) +
  scale_x_subj_rank +
  scale_y_continuous("Error rate", labels = percent) +
  scale_shape_exp_run +
  coord_cartesian(xlim = subj_xlim, ylim = subj_error_ylim) +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "top") +
  ggtitle("Average Error")

ggplot(subjs, aes(x = rank_rt, y = rt, color = subj_id, shape = factor(exp_run))) +
  geom_point() +
  geom_text(aes(label = subj_id), angle = 90, hjust = -0.1, size = label_size) +
  scale_x_subj_rank +
  scale_y_continuous("RT (ms)") +
  scale_shape_exp_run +
  coord_cartesian(xlim = subj_xlim, ylim = subj_rt_ylim) +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "top") +
  ggtitle("Average RT")

subjs_parallel <- subjs %>%
  select(-(rt:error)) %>%
  gather(rank_type, rank_value, -(exp_run:subj_id)) %>%
  mutate(rank_type = factor(rank_type, levels = c("rank_error", "rank_rt")))

# subjs_parallel <- subjs_parallel %>%
#   filter(rank_type == "rank_error") %>%
#   mutate(label_side = ifelse(rank_value %% 2, "rank_error", "rank_rt")) %>%
#   select(subj_id, label_side) %>%
#   left_join(subjs_parallel, .)

ggplot(subjs_parallel, aes(x = rank_type, y = rank_value, color = subj_id)) +
  geom_line(aes(group = subj_id, lty = factor(exp_run))) +
  geom_text(aes(label = subj_id),
            data = filter(subjs_parallel, rank_type == "rank_error"),
            size = label_size, hjust = 1) +
  scale_x_discrete("", labels = c("Error", "RT")) +
  scale_y_continuous("Rank", breaks = rank_axis_breaks) +
  scale_linetype_exp_run +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "top") +
  ggtitle("Correlation between Error and RT\n(rank)")

z_score <- function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)

subjs_parallel_z <- subjs %>%
  mutate(
    rt_z = z_score(rt), 
    error_z = z_score(error)
  ) %>% 
  select(exp_run, subj_id, rt_z, error_z) %>%
  gather(measure, z_score, -(exp_run:subj_id)) %>%
  mutate(measure = factor(measure, levels = c("error_z", "rt_z")))

ggplot(subjs_parallel_z, aes(x = measure, y = z_score, color = subj_id)) +
  geom_line(aes(group = subj_id, lty = factor(exp_run))) +
  geom_text(aes(label = subj_id),
            data = filter(subjs_parallel_z, measure == "error_z"),
            size = label_size, hjust = 1) +
  scale_x_discrete("", labels = c("Error", "RT")) +
  scale_y_continuous("z-score") +
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  scale_linetype_exp_run +
  base_theme +
  guides(color = "none") +
  theme(legend.position = "top") +
  ggtitle("Correlation between Error and RT\n(z-score)")

# ---- mod
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                             family = binomial, data = question_first)
tidy(feat_type_error_mod, effects = "fixed")

# ---- plot
error_bar_plot(question_first) +
  ggtitle("Effect of mask on error rate by feature type\n(both runs)")

error_bar_plot(question_first) +
  facet_wrap("exp_run_label") +
  ggtitle("Effect of mask on error rate by feature type")

question_first_timeout <- question_first %>%
  mutate(is_error = ifelse(response == "timeout", 0, is_error))

error_bar_plot(question_first_timeout) +
  facet_wrap("exp_run_label") +
  ggtitle("Effect of mask on error rate by feature type\n(timeouts counted as errors)")

# ---- run-diff-mod
run_diff_mod <- glmer(is_error ~ mask_c * feat_c * exp_run_c + (1|subj_id),
                 family = binomial, data = question_first)
tidy(run_diff_mod, effects = "fixed")

# ---- run-diff-plot
set.seed(169)

subj_effects <- question_first %>%
  group_by(exp_run, subj_id, feat_type) %>%
  summarize(
    # positive number means the mask increased the error rate
    effect_of_mask = mean(is_error[mask_type == "mask"], na.rm = TRUE) - 
                     mean(is_error[mask_type == "nomask"], na.rm = TRUE)
  )

# ggplot(subj_effects, aes(x = feat_type, y = effect_of_mask, color = factor(exp_run))) +
#   geom_point(shape = 1, position = position_jitter(width = 0.1, height = 0.0)) +
#   geom_point(stat = "summary", fun.y = "mean", size = 6) +
#   scale_x_feat_type +
#   scale_y_continuous("Effect of mask", labels = percent) +
#   annotate("text", x = 0.5, y = 0.075, label = "+ increased error rate",
#            angle = 90, size = 4) +
#   annotate("text", x = 0.5, y = -0.075, label = "- decreased error rate",
#            angle = 90, size = 4)

subj_effects_diff <- subj_effects %>%
  group_by(exp_run, subj_id) %>%
  summarize(
    # positive number means the mask effect was stronger for visual questions
    effect_of_mask_diff = mean(effect_of_mask[feat_type == "visual"], na.rm = TRUE) -
                          mean(effect_of_mask[feat_type == "nonvisual"], na.rm = TRUE)
  )

ggplot(subj_effects_diff, aes(x = factor(exp_run), y = effect_of_mask_diff, color = factor(exp_run))) +
  geom_point(shape = 1, position = position_jitter(width = 0.1, height = 0)) +
  geom_point(stat = "summary", fun.y = "mean", size = 6) +
  geom_hline(yintercept = 0, lty = 2, size = 0.1) +
  scale_x_discrete("Experiment run", labels = c("first", "second")) +
  scale_y_continuous("Effect of mask (interaction term)") +
  annotate("text", x = 0.5, y = 0.10, label = "+ larger for visual",
           angle = 90, size = 4) +
  annotate("text", x = 0.5, y = -0.10, label = "- larger for nonvisual",
           angle = 90, size = 4) +
  base_theme +
  theme(legend.position = "none")
  
# ---- overall-error
overall_error_mod <- glmer(is_error ~ exp_run_c + (1|subj_id),
                           family = binomial, data = question_first)
tidy(overall_error_mod, effects = "fixed")

# ---- overall-rt
overall_rt_mod <- lmer(rt ~ exp_run_c + (1|subj_id), data = question_first)
tidy(overall_rt_mod, effects = "fixed")

# ---- question-freq
question_freqs <- count(question_first, question_id, truth_coded)
ggplot(question_freqs, aes(x = n)) +
  geom_histogram(aes(fill = truth_coded)) +
  scale_x_continuous("Number of times proposition was seen") +
  scale_y_continuous("Frequency (number of propositions)") +
  scale_fill_discrete("Is proposition true?") +
  base_theme +
  ggtitle("Number of times a proposition was seen")

# ---- same-propositions
same_propositions <- question_first[, c("exp_run", "cue", "question")] %>% 
  unique %>%
  arrange(cue, question) %>%
  group_by(cue, question) %>%
  summarize(not_in_both_runs = !(n() == 2))

filter(same_propositions, not_in_both_runs == TRUE)

# ---- unique-propositions
unique_propositions <- question_first %>%
  group_by(exp_run, subj_id) %>%
  summarize(
    num_trials = n(),
    num_unique_propositions = length(unique(question_id))
  ) %>%
  mutate(all_unique = num_trials == num_unique_propositions)

unique_propositions %>%
  group_by(exp_run) %>%
  summarize(
    num_subjects = length(unique(subj_id)),
    num_all_unique = sum(all_unique)
  )

unique_propositions$num_duplicate <- with(unique_propositions, num_trials - num_unique_propositions)

hist(unique_propositions$num_duplicate,
     main = "How many questions were duplicates?",
     xlab = "Number of duplicated questions",
     ylab = "Number of participants")

# ---- drop-duplicates
question_first <- question_first %>%
  group_by(subj_id) %>%
  mutate(is_duplicate = label_duplicates(question_id))

error_bar_plot(filter(question_first, exp_run == 2, is_duplicate == FALSE)) +
  ggtitle("Effect of mask on error rate by feature type\n(second run; duplicates dropped)")

error_bar_plot(filter(question_first, is_duplicate == FALSE)) +
  ggtitle("Effect of mask on error rate by feature type\n(both runs; duplicates dropped)")

# ---- error-rate-on-duplicated-questions
filter(question_first, exp_run == 2) %>%
  group_by(is_duplicate) %>%
  summarize(error = mean(is_error, na.rm = TRUE))

# ---- run1-mod
run1_feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                                  family = binomial, data = run1_data)
tidy(run1_feat_type_error_mod, effects = "fixed")

# ---- run1-plot
error_bar_plot(run1_data) +
  ggtitle("Effect of mask on error by feature type\n(first run)")

# ---- run2-mod
run2_feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (1|subj_id),
                                  family = binomial, data = run2_data)
tidy(run2_feat_type_error_mod, effects = "fixed")

# ---- run2-plot
error_bar_plot(run2_data) +
  ggtitle("Effect of mask on error by feature type\n(second run)")

