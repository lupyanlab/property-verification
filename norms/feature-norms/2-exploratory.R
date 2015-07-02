library(ggplot2)

df <- read.csv('feature_norms.csv')

df$imagery_rank <- rank(df$imagery_mean, ties.method = "random")
df$facts_rank <- rank(df$facts_mean, ties.method = "random")
df$difficulty_rank <- rank(df$difficulty_mean, ties.method = "random")
df$truth_rank <- rank(df$truth_mean, ties.method = "random")

imagery_scale_y <- scale_y_continuous(
  name = "How much is it necessary to picture the object in your mind's eye?", 
  breaks = seq(0, 4, by = 1), 
  labels = c("None", "Little", "Some", "A Lot", "As much as possible")
)

imagery_scale_x <- scale_x_continuous(
  name = "How much is it necessary to picture the object in your mind's eye?", 
  breaks = seq(0, 4, by = 1), 
  labels = c("None", "Little", "Some", "A Lot", "As much as possible")
)

facts_scale_y <- scale_y_continuous(
  name = "How much do you rely on facts that you might read...?", 
  breaks = seq(0, 4, by = 1), 
  labels = c("None", "Little", "Some", "A Lot", "As much as possible")
)

facts_scale_x <- scale_x_continuous(
  name = "How much do you rely on facts that you might read...?", 
  breaks = seq(0, 4, by = 1), 
  labels = c("None", "Little", "Some", "A Lot", "As much as possible")
)

diff_scale_y <- scale_y_continuous(
  name = "How difficult was this question?", breaks = seq(-2, 2, by = 1),
  labels = c("Very Easy", "Easy", "Neutral", "Difficult", "Very Difficult")
)

diff_scale_x <- scale_x_continuous(
  name = "How difficult was this question?", breaks = seq(-2, 2, by = 1),
  labels = c("Very Easy", "Easy", "Neutral", "Difficult", "Very Difficult")
)

truth_scale_y <- scale_y_continuous(
  name = "What is the answer?", breaks = seq(-2, 2, by = 1), 
  labels = c("Definitely no", "Probably no", "Maybe", "Probably yes", "Definitely yes")
)

ggplot(df, aes(x = imagery_rank, y = imagery_mean, size = imagery_std, color = ftype)) +
  geom_point(position = position_jitter(height = 0.05, width = 10)) +
  coord_cartesian(ylim = c(-0.2,4.2)) +
  imagery_scale_y +
  scale_x_continuous("Question (Ranked)", breaks = c(0, 538)) +
  scale_size_continuous("Std Dev") +
  scale_color_discrete("Question Type", labels = c("Nonvisual", "Visual")) +
  theme(axis.text.y = element_text(angle = 45)) +
  ggtitle("Imagery")

# ggsave("../figures/feature-norms/imagery-ranked.png")

ggplot(df, aes(x = facts_rank, y = facts_mean, size = count, color = ftype)) +
  geom_point(position = position_jitter(height = 0.05, width = 10)) +
  coord_cartesian(ylim = c(-0.2,4.2)) +
  facts_scale_y +
  scale_x_continuous("Question (Ranked)", breaks = c(0, 538)) +
  scale_size_continuous("Std Dev") +
  scale_color_discrete("Question Type", labels = c("Nonvisual", "Visual")) +
  theme(axis.text.y = element_text(angle = 45)) +
  ggtitle("Facts")

# ggsave("../figures/feature-norms/facts-ranked.png")

ggplot(df, aes(x = imagery_mean, y = facts_mean, color = ftype)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  coord_cartesian(ylim = c(-0.2, 4.2), xlim = c(-0.2, 4.2)) +
  facts_scale_y +
  imagery_scale_x +
  scale_color_discrete("Question Type", labels = c("Nonvisual", "Visual")) +
  theme(axis.text.y = element_text(angle = 45),
        axis.text.x = element_text(angle = 45)) +
  ggtitle("Imagery and Facts")

# ggsave("../figures/feature-norms/imagery-by-facts.png")

ggplot(df, aes(x = difficulty_rank, y = difficulty_mean, size = count, color = ftype)) +
  geom_point(position = position_jitter(height = 0.05, width = 10)) +
  coord_cartesian(ylim = c(-2.2,2.2)) +
  diff_scale_y +
  scale_x_continuous("Question (Ranked)", breaks = c(0, 538)) +
  scale_size_continuous("Std Dev") +
  scale_color_discrete("Question Type", labels = c("Nonvisual", "Visual")) +
  theme(axis.text.y = element_text(angle = 45)) +
  ggtitle("Difficulty")

# ggsave("../figures/feature-norms/diff-ranked.png")

ggplot(df, aes(x = truth_rank, y = truth_mean, size = truth_std, color = truth_coded)) +
  geom_point(position = position_jitter(height = 0.05, width = 10)) +
  coord_cartesian(ylim = c(-2.2, 2.2)) +
  truth_scale_y +
  scale_x_continuous("Question (Ranked)", breaks = c(0, 538)) +
  scale_size_continuous("Std Dev") +
  scale_color_discrete("Coded truth_coded", labels = c("Yes", "No")) +
  theme(axis.text.y = element_text(angle = 45)) +
  ggtitle("Truth")

# ggsave("../figures/feature-norms/truth-ranked.png")
