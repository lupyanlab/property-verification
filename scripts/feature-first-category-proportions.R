library(dplyr)
library(lme4)
library(AICcmodavg)
library(ggplot2)
library(scales)
library(grid)

frame <- read.csv("feature-first/feature-first-final.csv", 
                  stringsAsFactors = FALSE)

mod_imagery <- glmer(is_error ~ mask_c * imagery_z + (1|subj_id), 
                     data = frame,
                     family = binomial)
summary(mod_imagery)

mod_prop_vis <- glmer(is_error ~ mask_c * prop_visual + (1|subj_id),
                      data = filter(frame, truth_coded == "yes"), 
                      family = binomial)
summary(mod_prop_vis)

mod_prop_vis__nomask <- glmer(is_error ~ prop_visual + (1|subj_id),
                              data = filter(frame, truth_coded == "yes", mask_type == "nomask"), 
                              family = binomial)
summary(mod_prop_vis__nomask)

mod_prop_vis__mask <- glmer(is_error ~ prop_visual + (1|subj_id),
                            data = filter(frame, truth_coded == "no", mask_type == "mask"), 
                            family = binomial)
summary(mod_prop_vis__mask)

# ----

question_means <- frame %.% 
  group_by(cue, mask_type) %.%
  summarize(
    obs = n(),
    se = sd(is_correct, na.rm = TRUE) / sqrt(obs),
    prop_visual = prop_visual[1],
    is_correct = mean(is_correct, na.rm = TRUE)
  ) %.% ungroup() %.%
  mutate(
    mask_type = factor(mask_type, levels = c("nomask", "mask"))
  )

question_means

mod_prop_acc <- glmer(is_correct ~ prop_visual * mask_c + (1|cue), 
                      data = filter(frame, truth_coded == "yes"), 
                      family = binomial)
x_preds <- expand.grid(prop_visual = seq(0.1, 0.7, by = 0.05),
                       mask_c = c(-0.5, 0.5))
y_preds <- predictSE(mod_prop_acc, x_preds, type = "response", se = TRUE)
preds <- cbind(x_preds, y_preds) %.%
  mutate(
    is_correct = fit,
    mask_type = ifelse(mask_c == -0.5, "nomask", "mask"),
    mask_type = factor(mask_type, levels = c("nomask", "mask"))
  )

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

continuous_estimates <- ggplot() +
  geom_smooth(aes(x = prop_visual, y = is_correct, color = mask_type, 
                  ymin = is_correct - se.fit, ymax = is_correct + se.fit, linetype = mask_type), data = preds, stat = "identity",
              show_guide = FALSE) +
  geom_line(aes(x = prop_visual, y = is_correct, color = mask_type, 
                ymin = is_correct - se.fit, ymax = is_correct + se.fit, linetype = mask_type), data = preds, stat = "identity") +
  geom_point(aes(x = prop_visual, y = is_correct, color = mask_type, size = obs,
                 shape = cue), data = question_means, position = position_jitter(width = 0.00, height = 0.0)) +
  # geom_linerange(aes(x = imagery_mean, y = is_correct, color = mask_type, 
  #   ymin = is_correct - se, ymax = is_correct + se), data = question_means) +
  scale_y_continuous("Accuracy", breaks = seq(0,1,by=0.05), labels = percent) +
  scale_x_continuous("Proportion of category features that are visual", breaks = seq(0, 1, by = 0.1)) + 
  scale_color_manual("", labels = c("No Interference", "Visual Interference"), values = nointer_inter) +
  scale_linetype_manual("", labels = c("No Interference", "Visual Interference"), values = c(3, 1)) +
  scale_shape_manual("", values = substr(question_means$cue, 1, 3)) +
  scale_size_continuous(range = c(3,5)) +
  # coord_cartesian(ylim = c(0.50, 1.00)) +
  guides(size = "none", fill = "none", shape = "none") +
  base_theme +
  theme(
    legend.position = c(0.2, 0.2),
    legend.key.size = unit(18, units = "points"),
    plot.margin = unit(c(-0.1, 0, 0, 0), units = "in")
  )
continuous_estimates
