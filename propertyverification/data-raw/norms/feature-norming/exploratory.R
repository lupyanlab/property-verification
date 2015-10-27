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
# ggsave("./figures/imagery-ranked.png")

ggplot(df, aes(x = facts_rank, y = facts_mean, size = count, color = ftype)) +
  geom_point(position = position_jitter(height = 0.05, width = 10)) +
  coord_cartesian(ylim = c(-0.2,4.2)) +
  facts_scale_y +
  scale_x_continuous("Question (Ranked)", breaks = c(0, 538)) +
  scale_size_continuous("Std Dev") +
  scale_color_discrete("Question Type", labels = c("Nonvisual", "Visual")) +
  theme(axis.text.y = element_text(angle = 45)) +
  ggtitle("Facts")
#ggsave("./figures/facts-ranked.png")

ggplot(df, aes(x = imagery_mean, y = facts_mean, color = ftype)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  coord_cartesian(ylim = c(-0.2, 4.2), xlim = c(-0.2, 4.2)) +
  facts_scale_y +
  imagery_scale_x +
  scale_color_discrete("Question Type", labels = c("Nonvisual", "Visual")) +
  theme(axis.text.y = element_text(angle = 45),
        axis.text.x = element_text(angle = 45)) +
  ggtitle("Imagery and Facts")
#ggsave("./figures/imagery-by-facts.png")

ggplot(df, aes(x = difficulty_rank, y = difficulty_mean, size = count, color = ftype)) +
  geom_point(position = position_jitter(height = 0.05, width = 10)) +
  coord_cartesian(ylim = c(-2.2,2.2)) +
  diff_scale_y +
  scale_x_continuous("Question (Ranked)", breaks = c(0, 538)) +
  scale_size_continuous("Std Dev") +
  scale_color_discrete("Question Type", labels = c("Nonvisual", "Visual")) +
  theme(axis.text.y = element_text(angle = 45)) +
  ggtitle("Difficulty")
ggsave("./figures/diff-ranked.png")

ggplot(df, aes(x = truth_rank, y = truth_mean, size = truth_std, color = response)) +
  geom_point(position = position_jitter(height = 0.05, width = 10)) +
  coord_cartesian(ylim = c(-2.2, 2.2)) +
  truth_scale_y +
  scale_x_continuous("Question (Ranked)", breaks = c(0, 538)) +
  scale_size_continuous("Std Dev") +
  scale_color_discrete("Coded Response", labels = c("Yes", "No")) +
  theme(axis.text.y = element_text(angle = 45)) +
  ggtitle("Truth")
ggsave("./figures/truth-ranked.png")

################################################################################
# recode
if (FALSE) {
df <- read.csv('feature_norms.csv')
df$response <- ifelse(df$truth_mean > 0, 'yes', 'no')
df <- df[df$truth_mean != 0, ]
cor(df[,c("imagery_mean","facts_mean","difficulty_mean","truth_mean")])

write.csv(df[,c("cue","ftype","qid","question","response")], file = "questions2.csv", row.names = F)

df$question_id <- with(df, paste(cue, ftype, response, qid, sep = ":"))
write.csv(df, file = "feature_norms2.csv", row.names = F)
}

################################################################################

library(lme4)
df$ftype_c <- ifelse(df$ftype == "nonvisual", 0, 1)

mod <- glm(ftype_c ~ imagery_mean, family = "binomial", data = df)
summary(mod)

mod2 <- glm(ftype_c ~ difficulty_mean, family = "binomial", data = df)
summary(mod2)

mod3 <- glm(ftype_c ~ facts_mean, family = "binomial", data = df)
summary(mod3)

mod4 <- glm(ftype_c ~ truth_mean, family = "binomial", data = df)
summary(mod4)

################################################################################
ggplot(df, aes(x = imagery_mean, fill = ftype)) +
  geom_density(alpha = 0.5) +
  imagery_scale_x

ggsave("./figures/imagery.png")

ggplot(df, aes(x = facts_mean, fill = ftype)) +
  geom_density(alpha = 0.5) +
  facts_scale_x

ggsave("./figures/facts.png")

ggplot(df, aes(x = difficulty_mean, fill = ftype)) +
  geom_density(alpha = 0.5) +
  coord_cartesian(xlim = c(-2.0, 2.0)) +
  diff_scale_x

ggsave("./figures/diff.png")


library(dplyr)

head(arrange(df, ftype, imagery_mean))
