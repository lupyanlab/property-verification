library(lme4)
library(AICcmodavg)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

source("./helper/within-subj-error.R")
source("./figure/base_theme.R")

# ------------------------------------------------------------------------------
# source("./figure/base_theme.R")
base_theme <- theme(
  text = element_text(family = "Helvetica", color = 'black', size = 8),
  line = element_line(color = 'black', size = 0.3),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = 'black'),
  axis.text = element_text(color = 'black', size = 8),
  axis.title = element_text(vjust = 0.3, size = 8),
  axis.ticks.x = element_line(color = 'black'),
  axis.ticks.y = element_line(color = 'black'),
  axis.ticks.length = unit(4, 'points'),
  legend.direction = "vertical",
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 8, face = "plain"),
  legend.key.size = unit(10, units = "points"),
  plot.margin = unit(c(0.2,0,0,0), units = "in")
)

nointer_inter <- c("#92c5de", "#ca0020")

# ------------------------------------------------------------------------------

df <- read.csv("./feature-first/feature-first-final.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# Discrete question type

mod <- glmer(is_correct ~ feat_c * mask_c + (1|subj_id), data = df, family = binomial)
x_preds <- unique(df[,c("feat_type", "feat_c", "mask_type", "mask_c")])
y_preds <- predictSE(mod, x_preds, type = "response", se.fit = TRUE)
acc_mean_se <- cbind(x_preds, y_preds) %.%
  mutate(
    feat_type = factor(feat_type, levels = c("nonvisual", "visual")),
    mask_type = factor(mask_type, levels = c("nomask", "mask"))
  ) %.%
  select(feat_type, mask_type, is_correct = fit, mean_se = se.fit) %.%
  arrange(feat_type, mask_type)

acc_diff_se <- df %.%
  summarySEwithin(measurevar = "is_correct", na.rm = TRUE, idvar = "subj_id",
                  withinvars = c("feat_type", "mask_type")) %.%
  mutate(
    feat_type = factor(feat_type, levels = c("nonvisual", "visual")),
    mask_type = factor(mask_type, levels = c("nomask", "mask"))
  ) %.% 
  select(feat_type, mask_type, diff_se = se, ci) %.%
  arrange(feat_type, mask_type)

acc_means <- merge(acc_mean_se, acc_diff_se)

discrete_plot <- ggplot(acc_means, aes(x = feat_type, y = is_correct, fill = mask_type)) +
  geom_bar(aes(fill = mask_type), stat = "identity", position = position_dodge(width = 0.6),
           color = "black", width = 0.6, size = 0.3) +
  geom_bar(aes(y = is_correct - 0.01, fill = mask_type), stat = "identity", position = position_dodge(width = 0.6),
           width = 0.59) +
  geom_errorbar(aes(ymin = is_correct - mean_se, ymax = is_correct + mean_se), 
                position = position_dodge(width = 0.6), size = 0.4, width = 0.1) +
  coord_cartesian(ylim = c(0.70, 1.00)) +
  scale_y_continuous("Accuracy", breaks = seq(0,1,by=0.05), labels = percent) +
  scale_x_discrete("", labels = c("Nonvisual Feature Question", "Visual Feature Question")) +
  scale_fill_manual("", labels = c("No Interference", "Visual Interference"), values = nointer_inter) +
  base_theme + theme(
    legend.position = c(0.8, 0.96)
  )
discrete_plot

# ------------------------------------------------------------------------------
# Continuous question type

question_means <- df %.% 
  group_by(imagery_mean = cut(imagery_mean, breaks = seq(0, 3.5, by = 0.25), labels = seq(0.125, 3.375, by = 0.25), right = FALSE), mask_type) %.%
  summarize(
    obs = n(),
    se = sd(is_correct, na.rm = TRUE) / sqrt(obs),
    is_correct = mean(is_correct, na.rm = TRUE)
  ) %.% ungroup() %.%
  mutate(
    imagery_mean = as.numeric(levels(imagery_mean))[imagery_mean],
    mask_type = factor(mask_type, levels = c("nomask", "mask"))
  )

mod_imag_acc <- glmer(is_correct ~ imagery_mean * mask_c + (1|subj_id), data = df, family = binomial)
x_preds <- expand.grid(imagery_mean = seq(min(df$imagery_mean), max(df$imagery_mean), by = 0.05),
                       mask_c = c(-0.5, 0.5))
y_preds <- predictSE(mod_imag_acc, x_preds, type = "response", se = TRUE)
preds <- cbind(x_preds, y_preds) %.%
  mutate(
    is_correct = fit,
    mask_type = ifelse(mask_c == -0.5, "nomask", "mask"),
    mask_type = factor(mask_type, levels = c("nomask", "mask"))
  )

continuous_estimates <- ggplot() +
  geom_smooth(aes(x = imagery_mean, y = is_correct, color = mask_type, 
                  ymin = is_correct - se.fit, ymax = is_correct + se.fit, linetype = mask_type), data = preds, stat = "identity",
              show_guide = FALSE) +
  geom_line(aes(x = imagery_mean, y = is_correct, color = mask_type, 
                ymin = is_correct - se.fit, ymax = is_correct + se.fit, linetype = mask_type), data = preds, stat = "identity") +
  geom_point(aes(x = imagery_mean, y = is_correct, color = mask_type, size = obs,
                 shape = mask_type), data = question_means, position = position_jitter(width = 0.06, height = 0.0)) +
  # geom_linerange(aes(x = imagery_mean, y = is_correct, color = mask_type, 
  #   ymin = is_correct - se, ymax = is_correct + se), data = question_means) +
  scale_y_continuous("Accuracy", breaks = seq(0,1,by=0.05), labels = percent) +
  scale_x_continuous("\"How much is it necessary to picture the object in your mind's eye?\"", 
                     breaks = seq(0, 3.5, by = 0.25), 
                     labels = c("None", "", "", "", "Little", "", "", "", "Some", "", "", "", "A Lot", "", "")) +
  scale_color_manual("", labels = c("No Interference", "Visual Interference"), values = nointer_inter) +
  scale_linetype_manual("", labels = c("No Interference", "Visual Interference"), values = c(3, 1)) +
  scale_shape_discrete("", labels = c("No Interference", "Visual Interference"), solid = TRUE) +
  scale_size_continuous(range = c(1,3)) +
  coord_cartesian(ylim = c(0.70, 1.00)) +
  guides(size = "none", fill = "none") +
  base_theme +
  theme(
    legend.position = c(0.2, 0.2),
    legend.key.size = unit(18, units = "points"),
    plot.margin = unit(c(-0.1, 0, 0, 0), units = "in")
  )
continuous_estimates

# ------------------------------------------------------------------------------
stacked <- arrangeGrob(discrete_plot, continuous_estimates,
                       nrow = 2 , ncol = 1)
stacked

png("../doc/figures/sup-fig4.png", width = 5, height = 5, units = "in", res = 150)
stacked
dev.off()

pdf("../doc/figures/sup-fig4.pdf", width = 5, height = 5)
stacked
dev.off()
