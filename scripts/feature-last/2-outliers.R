require(dplyr)
require(ggplot2)
require(scales)    # for percent axes
require(gridExtra) # arrange two ggplots

df <- read.csv('./feature-last/feature-last-final.csv')

# By Subject

subjs <- df %.% 
  group_by(subj_id) %.% 
  summarize(
    obs = length(rt),
    rt = mean(rt, na.rm = T),
    acc = mean(is_correct, na.rm = T)
  ) %.% 
  mutate(
    rank_rt = rank(rt, ties.method = "random"),
    rank_acc = rank(-acc, ties.method = "random")
  )

rts <- ggplot(subjs, aes(x = rank_rt, y = rt, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 27), ylim = c(100, 1000)) +
  scale_x_continuous("Subject (Ranked By RT)", breaks = c(1, seq(5, 45, by = 5), 46)) +
  scale_y_continuous("Average RT (ms)") +
  theme(legend.position = "none")

accs <- ggplot(subjs, aes(x = rank_acc, y = acc, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 27), ylim = c(0.7, 1.05)) +
  scale_x_continuous("Subject (Ranked By Accuracy)", breaks = c(1, seq(5, 45, by = 5), 46)) +
  scale_y_continuous("Accuracy", label = percent) +
  theme(legend.position = "none")

(outliers <- arrangeGrob(rts, accs, nrow = 2))

ggsave("./feature-last/2-outliers.png", plot = outliers, width = 12, height = 8, units = "in")
