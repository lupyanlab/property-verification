####################################################################################################
# data manipulation
library(dplyr)
library(reshape2)
library(car)

# plots
library(ggplot2)
library(scales)
library(grid)
####################################################################################################
df <- read.csv("./data/mwpf1-2014-04-18.csv", stringsAsFactors = F)

(N <- length(unique(df$subj_id)))

df <- df[df$block_ix > -1, ]       # remove practice trials
df[df$is_correct == 0, "rt"] <- NA # remove RTs from incorrect and timeout trials
# df[df$response == "timeout", "is_correct"] <- NA # should timeout trials be considered incorrect?

####################################################################################################
subjs <- df %.% 
  group_by(subj_id) %.% 
  summarize(
    obs = length(rt),
    rt = mean(rt, na.rm = T),
    acc = mean(is_correct, na.rm = T)
  ) %.% 
  mutate(
    rank_rt = rank(rt, ties.method = "random"),
    rank_acc = rank(acc, ties.method = "random")
  )

ggplot(subjs, aes(x = rank_rt, y = rt, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 48), ylim = c(550, 1200)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 45, by = 5), 46)) +
  scale_y_continuous("Average RT (ms)") +
  theme(legend.position = "none")

# ggsave("./figures/feature_last/subj-rt.png", width = 12, height = 5, units = "in")

ggplot(subjs, aes(x = rank_acc, y = acc, label = subj_id, color = subj_id)) +
  geom_point() +
  geom_text(hjust = 0, vjust = -0.5, angle = 45, size = 4) +
  coord_cartesian(xlim = c(0.5, 48), ylim = c(0.7, 1.05)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 45, by = 5), 46)) +
  scale_y_continuous("Accuracy", label = percent) +
  theme(legend.position = "none")

# ggsave("./figures/feature_last/subj-acc.png", width = 12, height = 5, units = "in")
####################################################################################################
if (FALSE) { # shortcut to read data from the top
  df <- read.csv("./data/mwpf1-2014-04-18.csv", stringsAsFactors = F)
  df <- df[df$block_ix > -1, ]       # remove practice trials
  df[df$is_correct == 0, "rt"] <- NA # remove RTs from incorrect and timeout trials
}

# make a data.frame of unique trial types
# used to code factors and to generate model predictions
trial_types <- expand.grid(cue_mask = c("nomask", "mask"),
                           ftype = c("nonvisual", "visual"))
trial_types$mask_c <- recode(trial_types$cue_mask, "'nomask' = -1/2; 'mask' = 1/2", as.factor.result = F)
trial_types$feature_c <- recode(trial_types$ftype, "'nonvisual' = -1/2; 'visual' = 1/2", as.factor.result = F)

df <- merge(df, trial_types, by = c("cue_mask", "ftype"))

df <- df %.%
  # filter(subj_id != "MWPF214") %.%
  select(subj_id, block_ix, trial_ix, 
         cue, cue_mask, ftype, 
         question, response, qid, 
         response.1, rt, is_correct, 
         mask_c, feature_c) %.%
  arrange(subj_id, block_ix, trial_ix)

# write.csv(df, file = "./data/mwpf1-final.csv", row.names = FALSE)
# df <- read.csv("./data/mwpf1-final.csv")
####################################################################################################
# stats
library(lme4)
library(car)
library(AICcmodavg)
library(pbkrtest)
####################################################################################################
# Is there an overall effect of the mask?
if (FALSE) {
  set.seed(711)
  mask_rt <- lmer(rt ~ mask_c + (mask_c|subj_id), data = df)
  mask_rt_nomask <- update(mask_rt, . ~ . - mask_c)
  mask_rt_chi <- anova(mask_rt, mask_rt_nomask)
  mask_rt_anova <- NULL # KRmodcomp(mask_rt, mask_rt_nomask)
  
  save("mask_rt", "mask_rt_nomask", "mask_rt_chi", "mask_rt_anova",
       file = "./models/feature_last/mask_rt.RData")
}

load("./models/feature_last/mask_rt.RData")

summary(mask_rt)
mask_rt_chi
mask_rt_anova

# Does the mask affect accuracy?
if (FALSE) {
  set.seed(289)
  mask_acc <- glmer(is_correct ~ mask_c + (mask_c|subj_id), family = "binomial", data = df)
  mask_acc_nomask <- update(mask_acc, . ~ . - mask_c)
  mask_acc_chi <- anova(mask_acc, mask_acc_nomask)
  
  save("mask_acc", "mask_acc_nomask", "mask_acc_chi",
       file = "./models/feature_last/mask_acc.RData")
}

load("./models/feature_last/mask_acc.RData")

summary(mask_acc)
mask_acc_chi

####################################################################################################
# Is there an overall difference between question types?
if (FALSE) {
  set.seed(711)
  feature_rt <- lmer(rt ~ feature_c + (feature_c|subj_id), data = df)
  feature_rt_nofeature <- update(feature_rt, . ~ . - feature_c)
  feature_rt_chi <- anova(feature_rt, feature_rt_nofeature)
  feature_rt_anova <- NULL # KRmodcomp(feature_rt, feature_rt_nofeature)
  
  save("feature_rt", "feature_rt_nofeature", "feature_rt_chi", "feature_rt_anova",
       file = "./models/feature_last/feature_rt.RData")
}

load("./models/feature_last/feature_rt.RData")

summary(feature_rt)
feature_rt_chi
feature_rt_anova

# Does the feature affect accuracy?
if (FALSE) {
  set.seed(426)
  feature_acc <- glmer(is_correct ~ feature_c + (feature_c|subj_id), family = "binomial", data = df)
  feature_acc_nofeature <- update(feature_acc, . ~ . - feature_c)
  feature_acc_chi <- anova(feature_acc, feature_acc_nofeature)
  
  save("feature_acc", "feature_acc_nofeature", "feature_acc_chi",
       file = "./models/feature_last/feature_acc.RData")
}

load("./models/feature_last/mask_acc.RData")

summary(feature_acc)
feature_acc_chi

####################################################################################################
# Does the effect of the mask vary by feature?
if (FALSE) {
  set.seed(156)
  inter_rt <- lmer(rt ~ feature_c * mask_c + (feature_c * mask_c|subj_id), data = df)
  inter_rt_nointer <- update(inter_rt, . ~ . - feature_c:mask_c)
  inter_rt_chi <- anova(inter_rt, inter_rt_nointer)
  inter_rt_anova <- NULL # KRmodcomp(inter_rt, inter_rt_nointer)
  
  save("inter_rt", "inter_rt_nointer", "inter_rt_chi", "inter_rt_anova",
       file = "./models/feature_last/inter_rt.RData")
}

load("./models/feature_last/inter_rt.RData")

summary(inter_rt)
inter_rt_chi
inter_rt_anova

if (FALSE) {
  set.seed(076)
  inter_acc <- glmer(is_correct ~ feature_c * mask_c + (feature_c * mask_c|subj_id), family = "binomial", data = df)
  inter_acc_nointer <- update(inter_acc, . ~ . - feature_c:mask_c)
  inter_acc_chi <- anova(inter_acc, inter_acc_nointer)
  
  save("inter_acc", "inter_acc_nointer", "inter_acc_chi",
       file = "./models/feature_last/inter_acc.RData")
}

load("./models/feature_last/inter_acc.RData")

summary(inter_acc)
inter_acc_chi

####################################################################################################
if (FALSE) {
  load("./models/feature_last/inter_rt.RData")
  
  trial_types <- expand.grid(cue_mask = c("nomask", "mask"),
                             ftype = c("nonvisual", "visual"))
  trial_types$mask_c <- recode(trial_types$cue_mask, "'nomask' = -1/2; 'mask' = 1/2", as.factor.result = F)
  trial_types$feature_c <- recode(trial_types$ftype, "'nonvisual' = -1/2; 'visual' = 1/2", as.factor.result = F)
}

y_preds <- predictSE.mer(inter_rt, newdata = trial_types, se.fit = T, type = "response")
preds <- cbind(trial_types, y_preds)
preds$upr <- with(preds, fit + se.fit)
preds$lwr <- with(preds, fit - se.fit)

ggplot(preds, aes(y = fit, ymin = lwr, ymax = upr, x = ftype, fill = cue_mask)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.0) +
  coord_cartesian(ylim = c(600, 1000)) +
  scale_y_continuous("Response Time (ms)") +
  scale_x_discrete("Feature Type", labels = c("Nonvisual", "Visual")) +
  scale_fill_discrete("", labels = c("No Mask", "Mask")) +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = c(0.16, 0.88),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank())

# ggsave("figures/feature_last/reactiontime.png", width = 6, height = 6, units = "in")

####################################################################################################
if (FALSE) {
  load("./models/feature_last/inter_acc.RData")
  
  trial_types <- expand.grid(cue_mask = c("nomask", "mask"),
                             ftype = c("nonvisual", "visual"))
  trial_types$mask_c <- recode(trial_types$cue_mask, "'nomask' = -1/2; 'mask' = 1/2", as.factor.result = F)
  trial_types$feature_c <- recode(trial_types$ftype, "'nonvisual' = -1/2; 'visual' = 1/2", as.factor.result = F)
}

y_preds <- predictSE.mer(inter_acc, newdata = trial_types, se.fit = T, type = "response")
preds <- cbind(trial_types, y_preds)
preds$upr <- with(preds, fit + se.fit)
preds$lwr <- with(preds, fit - se.fit)

ggplot(preds, aes(y = fit, ymin = lwr, ymax = upr, x = ftype, fill = cue_mask)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.0) +
  coord_cartesian(ylim = c(0.8, 1.0)) +
  scale_y_continuous("Accuracy", labels = percent) +
  scale_x_discrete("Feature Type", labels = c("Nonvisual", "Visual")) +
  scale_fill_discrete("", labels = c("No Mask", "Mask")) +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = c(0.16, 0.88),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank())

# ggsave("figures/feature_last/accuracy.png", width = 6, height = 6, units = "in")

####################################################################################################

fixed_efs <- as.data.frame(summary(inter_rt)$coefficients)[1:2]
colnames(fixed_efs) <- c("beta", "se")
fixed_efs$param <- row.names(fixed_efs)
row.names(fixed_efs) <- NULL

rand_efs <- coef(inter_rt)$subj_id
rand_efs$subj_id <- row.names(rand_efs)
row.names(rand_efs) <- NULL

rand_efs$r1 <- rank(rand_efs[,1], ties.method = "random")
rand_efs$r2 <- rank(rand_efs[,2], ties.method = "random")
rand_efs$r3 <- rank(rand_efs[,3], ties.method = "random")
rand_efs$r4 <- rank(rand_efs[,4], ties.method = "random")

rand_efs <- melt(rand_efs, measure.vars = c("(Intercept)", "feature_c", "mask_c", "feature_c:mask_c"), 
                 id.vars = c("subj_id", "r1", "r2", "r3", "r4"), variable.name = "param", value.name = "beta")

ggplot(rand_efs, aes(y = beta)) +
  geom_point(aes(x = r1, color = subj_id)) +
  geom_pointrange(aes(x = 12.5, ymin = beta - se, ymax = beta + se), data = fixed_efs, size = 1.2) +
  coord_cartesian(xlim = c(0.5, 25.5)) +
  scale_x_continuous("Subject (Ranked)", breaks = c(1, seq(5, 45, by = 5), 46)) +
  scale_y_continuous("Parameter Estimate") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(param ~ ., scales = "free_y") +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = "none")

# ggsave("./figures/feature_last/randomeffects_rt.png", width = 10, height = 9, units = "in")

####################################################################################################
# df <- read.csv("./data/mwpf1-final.csv")
norms <- read.csv("../feature-norming/feature_norms.csv")
norms$imagery_z <- (norms$imagery_mean - mean(norms$imagery_mean, na.rm = T))/sd(norms$imagery_mean, na.rm = T)
norms$facts_z <- (norms$facts_mean - mean(norms$facts_mean, na.rm = T))/sd(norms$facts_mean, na.rm = T)
norms$diff_z <- (norms$difficulty_mean - mean(norms$difficulty_mean, na.rm = T))/sd(norms$difficulty_mean, na.rm = T)

df$question_id <- with(df, paste(cue, ftype, response, qid, sep = ":"))
norms$question_id <- with(norms, paste(cue, ftype, response, qid, sep = ":"))

df <- merge(df, norms, all.x = T)

####################################################################################################
if (FALSE) {
  set.seed(624)
  imag_acc <- glmer(is_correct ~ imagery_z * mask_c + (imagery_z * mask_c|subj_id), family = "binomial", data = df)
  imag_acc_nointer <- update(imag_acc, . ~ . - imagery_z:mask_c)
  imag_acc_chi <- anova(imag_acc, imag_acc_nointer)
  
  save("imag_acc", "imag_acc_nointer", "imag_acc_chi",
       file = "./models/feature_last/imag_acc.RData")
  
  set.seed(933)
  imag_acc_diff <- glmer(is_correct ~ imagery_z * mask_c + diff_z + 
                           (imagery_z * mask_c|subj_id), family = "binomial", data = df)
  imag_acc_diff_nointer <- update(imag_acc_diff, . ~ . - imagery_z:mask_c)
  imag_acc_diff_chi <- anova(imag_acc_diff, imag_acc_diff_nointer)
  
  save("imag_acc_diff", "imag_acc_diff_nointer", "imag_acc_diff_chi",
       file = "./models/feature_last/imag_acc_diff.RData")
}

load("./models/feature_last/imag_acc.RData")

summary(imag_acc)
imag_acc_chi

load("./models/feature_last/imag_acc_diff.RData")

summary(imag_acc_diff)
imag_acc_diff_chi

####################################################################################################
load("./models/feature_last/imag_acc.RData")

# range(df$imagery_z)
imagery_range <- seq(-2.3, 3.2, by = 0.1)

trial_types <- expand.grid(cue_mask = c("nomask", "mask"),
                           imagery_z = imagery_range,
                           diff_z = 0.0)
trial_types$mask_c <- recode(trial_types$cue_mask, "'nomask' = -1/2; 'mask' = 1/2", as.factor.result = F)

y_preds <- predictSE.mer(imag_acc, newdata = trial_types, se.fit = T, type = "response")
preds <- cbind(trial_types, y_preds)
preds$upr <- with(preds, fit + se.fit)
preds$lwr <- with(preds, fit - se.fit)

ggplot(preds, aes(y = fit, ymin = lwr, ymax = upr, x = imagery_z, color = cue_mask)) +
  geom_smooth(stat = "identity") + 
  coord_cartesian(xlim = c(-2.35, 3.25), ylim = c(0.7, 1.0)) +
  scale_y_continuous("Accuracy", breaks = seq(0.7, 1.0, by = 0.05), labels = percent) +
  scale_x_continuous("Imagery (z-score)") +
  scale_color_discrete("", labels = c("No Mask", "Mask")) +
  theme(text = element_text(color = 'black', size = 18),
        line = element_line(color = 'black'),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'gray', linetype = 3, size = 0.6),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length = unit(8, 'points'),
        legend.position = c(0.8, 0.88),
        legend.background = element_blank(),
        legend.key = element_blank())

# ggsave("figures/feature_last/accuracy_imagery.png", width = 6, height = 6, units = "in")