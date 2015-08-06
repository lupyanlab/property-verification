# ---- speed-accuracy-tradeoff ----
source("scripts/question_first_data.R")

library(lme4)
library(ggplot2)

source("scripts/outliers.R")

# Drop outlier subjects
question_first <- filter(question_first, subj_id %nin% question_first_outliers)

# Compute by subject means
by_subj_means <- question_first %>% group_by(subj_id, mask_type) %>%
  summarize(correct_rt = mean(rt, na.rm = TRUE),     # missing RTs on timeout and incorrect trials
            overall_rt = mean(raw_rt, na.rm = TRUE), # missing RTs on timeout trials only
            accuracy = mean(is_correct, na.rm = TRUE))

# Look at speed-accuracy correlations
cor(by_subj_means[,c("correct_rt", "overall_rt", "accuracy")])

# Predict accuracy from RT
tradeoff_mod <- glmer(is_error ~ raw_rt + (1|subj_id),
                      family = binomial, data = question_first)
summary(tradeoff_mod)

# Visualize the effect
ggplot(question_first, aes(x = raw_rt, y = is_error)) +
  geom_point(shape = 1, position = position_jitter(width = 0.0, height = 0.08)) +
  stat_smooth(method = "glm", se = FALSE)
