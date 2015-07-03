library(ggplot2)
library(scales)
source("scripts/question_first_data.R")

subjs <- question_first %>%
  group_by(subj_id) %>% 
  summarize(
    rt = mean(rt, na.rm = T),
    err = mean(is_error, na.rm = T)
  ) %>% 
  mutate(
    rank_rt = rank(rt, ties.method = "random"),
    rank_err = rank(err, ties.method = "random")
  )

subj_rts <- ggplot(subjs, aes(x = rank_rt, y = rt, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 28), ylim = c(100, 950)) +
  scale_x_continuous("Subject (Ranked By RT)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Average RT (ms)") +
  theme(legend.position = "none")
subj_rts

subj_err <- ggplot(subjs, aes(x = rank_err, y = err, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 28), ylim = c(0.0, 0.25)) +
  scale_x_continuous("Subject (Ranked By Error Rate)", breaks = c(1, seq(5, 25, by = 5))) +
  scale_y_continuous("Error Rate", label = percent) +
  theme(legend.position = "none")
subj_err

# Notes
# -----
# MWPF214 is the slowest subj in the sample and had the highest error rate

ggsave("plots/question_first/descriptives/subj-rts.png", subj_rts, width = 10, height = 6)
ggsave("plots/question_first/descriptives/subj-err.png", subj_err, width = 10, height = 6)
