
library(ggplot2)
library(dplyr)

library(grid)

df <- read.csv("./data/mwpf2-final.csv")

df_summary <- df %.%
  group_by(question_id, mask_c) %.%
  summarize(
    is_correct = mean(is_correct, na.rm = T),
    obs = length(subj_id),
    imagery_z = mean(imagery_z)
  )

df_summary$imagery_z <- ifelse(df_summary$mask_c == -0.5, df_summary$imagery_z - 0.05, df_summary$imagery_z + 0.05)

ggplot(df_summary, aes(x = imagery_z, y = is_correct)) +
  geom_point(aes(size = obs, color = factor(mask_c))) +
  geom_line(aes(group = question_id), alpha = 0.5)
  #stat_smooth(aes(color = factor(mask_c)), data = df, method = "glm", formula = y ~ x)

# ggsave("./figures/feature_first/raw_accuracies.png", width = 8, height = 6, units = "in")
