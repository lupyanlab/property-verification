source("reports/knowledge_type/utils.R")

blank_icon <- png_to_grob("reports/knowledge_type/files/blank.png", alpha = 0.8)
mask_icon <- png_to_grob("reports/knowledge_type/files/interference.png", alpha = 0.8)
trial <- png_to_grob("reports/knowledge_type/files/trial.png", alpha = 1.0)

library(dplyr)
library(lme4)
library(AICcmodavg)
library(broom)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(extrafont)
loadfonts()

library(propertyverificationdata)
data(question_first)
question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_outliers %>%
  filter(is_subj_outlier == 0)

question_first_all_props <- question_first
question_first <- filter(question_first, is_prop_outlier == 0)

# ---- error-mods
overall_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = question_first)

# ---- error-plot
overall_preds_x <- expand.grid(feat_c = c(-0.5, 0.5), mask_c = c(-0.5, 0.5))
overall_preds_y <- predictSE(overall_mod, overall_preds_x)
overall_preds <- cbind(overall_preds_x, overall_preds_y) %>%
  rename(is_error = fit, se = se.fit) %>%
  recode_mask_type %>%
  recode_feat_type

icon_width = 0.24
icon_y = -0.048

colors <- RColorBrewer::brewer.pal(3, "Set2")
names(colors) <- c("green", "orange", "blue")
feat_type_colors <- c(colors["green"], colors["blue"])

scale_x_mask <- scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("Blank screen", "Visual interference"))
scale_y_error <- scale_y_continuous("Error rate", labels = percent, breaks = seq(0, 1, by = 0.02))
scale_fill_feat_type <- scale_fill_manual("Question type", labels = c("Encyclopedic knowledge", "Visual knowledge"), values = unname(feat_type_colors))
scale_alpha_mask <- scale_alpha_manual(values = c(0.5, 0.9))

coord_ylim_error <- c(0, 0.061)

base_theme <- theme_minimal(base_size = 28) +
  theme(
    text = element_text(family="Arial"),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.margin = unit(0, "lines")
  )

overall_plot <- ggplot(overall_preds, aes(x = mask_c, y = is_error)) +
  geom_bar(aes(fill = feat_type, alpha = mask_f),
           stat = "identity", width = 1.0) +
  geom_linerange(aes(ymin = is_error-se, ymax = is_error+se)) +
  facet_wrap("feat_label") +
  scale_x_mask +
  scale_y_error +
  scale_fill_feat_type +
  scale_alpha_mask +
  coord_cartesian(ylim = coord_ylim_error) +
  annotation_custom(blank_icon, xmin = -0.5 - icon_width, xmax = -0.5 + icon_width, ymin = icon_y) +
  annotation_custom(mask_icon, xmin = 0.5 - icon_width, xmax = 0.5 + icon_width, ymin = icon_y) +
  base_theme
overall_plot

ggsave("reports/knowledge_type/graphical_abstract.pdf", overall_plot, device = "pdf")