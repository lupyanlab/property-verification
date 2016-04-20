# ---- setup
library(dplyr)
library(ggplot2)
library(scales)

# ---- theme
base_theme <- theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.ticks = element_blank()
  )

# ---- data
library(propertyverificationdata)
data(question_first)

question_first <- question_first %>%
  # Drop practice trials
  filter(block_type != "practice") %>%
  # Exclude RTs on timeout trials
  mutate(rt = ifelse(response == "timeout", NA, rt)) %>%
  # Exclude weirdly long RTs
  mutate(rt = ifelse(rt > 2000.0, NA, rt))

# ---- rt-hist
gg_rt_hist <- ggplot(question_first, aes(x = rt)) +
  base_theme

gg_rt_hist +
  geom_density() +
  ggtitle("All RTs")

gg_rt_hist +
  geom_density(aes(color = correct_response)) +
  ggtitle("RTs by correct response to proposition")

gg_rt_hist +
  geom_density(aes(color = correct_response)) +
  facet_wrap("exp_run", ncol = 1) +
  ggtitle("RTs by correct response for each experiment")

gg_rt_hist +
  geom_density(aes(color = correct_response)) +
  facet_wrap("is_correct") +
  ggtitle("RTs by correct response and correctness")

gg_rt_hist +
  geom_density(aes(color = correct_response)) +
  facet_grid(exp_run ~ is_correct)

# ---- error-by-rt
question_first <- question_first %>%
  group_by(subj_id) %>%
  mutate(
    rt_c = rt - mean(rt, na.rm = TRUE),
    rt_bin = cut(rt_c, breaks = 4)
  ) %>%
  ungroup()

ggplot(question_first, aes(x = rt_bin)) +
  geom_histogram()
