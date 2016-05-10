source("reports/knowledge_type/utils.R")

# ---- setup
blank_icon <- png_to_grob("files/blank.png", alpha = 0.8)
mask_icon <- png_to_grob("files/interference.png", alpha = 0.8)
trial <- png_to_grob("files/trial.png", alpha = 1.0)

library(dplyr)
library(lme4)
library(AICcmodavg)
library(broom)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

colors <- RColorBrewer::brewer.pal(3, "Set2")
names(colors) <- c("green", "orange", "blue")
feat_type_colors <- c(colors["green"], colors["blue"])

scale_x_mask <- scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("Blank", "Interference"))
scale_y_error <- scale_y_continuous("Error rate", labels = percent, breaks = seq(0, 1, by = 0.02))
scale_fill_feat_type <- scale_fill_manual("Question type", labels = c("Encyclopedic knowledge", "Visual knowledge"), values = unname(feat_type_colors))
scale_alpha_mask <- scale_alpha_manual(values = c(0.5, 0.9))

coord_ylim_error <- c(0, 0.081)

base_theme <- theme_minimal(base_size = 10) +
  theme(axis.ticks = element_blank(),
        legend.position = "none",
        panel.margin = unit(0, "lines"))

# ---- data
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
feat_type_mod <- glmer(is_error ~ feat_c + (1|subj_id), family = "binomial", data = filter(question_first, mask_type == "nomask"))
vis_mask_mod <- glmer(is_error ~ mask_c + (1|subj_id), family = "binomial", data = filter(question_first, feat_type == "visual"))
non_mask_mod <- glmer(is_error ~ mask_c + (1|subj_id), family = "binomial", data = filter(question_first, feat_type == "nonvisual"))

original_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = filter(question_first, exp_run == 1))
replication_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = filter(question_first, exp_run %in% c(2, 3)))
preregistered_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = filter(question_first, exp_run == 4))

original_feat_type_mod <- glmer(is_error ~ feat_c + (1|subj_id), family = "binomial", data = filter(question_first, exp_run == 1, mask_type == "nomask"))
original_vis_mask_mod <- glmer(is_error ~ mask_c + (1|subj_id), family = "binomial", data = filter(question_first, exp_run == 1, feat_type == "visual"))
original_non_mask_mod <- glmer(is_error ~ mask_c + (1|subj_id), family = "binomial", data = filter(question_first, exp_run == 1, feat_type == "nonvisual"))

overall_w_outliers <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = question_first_all_props)
original_w_outliers <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = filter(question_first_all_props, exp_run == 1))
replication_w_outliers <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = filter(question_first_all_props, exp_run %in% c(2, 3)))
preregistered_w_outliers <- glmer(is_error ~ feat_c * mask_c + (1|subj_id), family = "binomial", data = filter(question_first_all_props, exp_run == 4))

# ---- error-plot
overall_preds_x <- expand.grid(feat_c = c(-0.5, 0.5), mask_c = c(-0.5, 0.5))
overall_preds_y <- predictSE(overall_mod, overall_preds_x)
overall_preds <- cbind(overall_preds_x, overall_preds_y) %>%
  rename(is_error = fit, se = se.fit) %>%
  recode_mask_type %>%
  recode_feat_type

icon_width = 0.2
icon_y = -0.065

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

# ---- error-by-exp
error_by_exp <- question_first %>%
  group_by(mask_c, feat_c, exp_run_label) %>%
  summarize(is_error = mean(is_error, na.rm = TRUE)) %>%
  recode_mask_type %>%
  recode_feat_type

exp_plot <- ggplot(error_by_exp, aes(x = mask_c, y = is_error)) +
  geom_line(aes(y = is_error, color = exp_run_label),
            size = 1.5) +
  facet_wrap("feat_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_discrete("") +
  base_theme +
  theme(
    legend.position = "top",
    panel.margin = unit(4, "lines")
  )
exp_plot

# ---- outliers
extract_model_params <- function(mod, exp_label, outlier_label) {
  tidy(mod, effects = "fixed") %>%
    filter(term == "feat_c:mask_c") %>%
    mutate(
      exp_label = exp_label,
      outlier_label = outlier_label
    ) %>%
    select(exp_label, outlier_label, estimate, std.error)
}

compare_model_params <- function(no_outliers, w_outliers, exp_label) {
  rbind(
    extract_model_params(no_outliers, exp_label, outlier_label = "no_outliers"),
    extract_model_params(w_outliers, exp_label, outlier_label = "w_outliers")
  )
}

recode_exp_label <- function(frame) {
  exp_labels <- c("overall", "preregistered", "replication", "original")
  exp_label_map <- data_frame(
    exp_label = exp_labels,
    exp_label_f = factor(exp_labels,
                         labels = c("Overall", "Preregistered", "Replication", "Original"),
                         levels = exp_labels)
  )
  outlier_dodge <- 0.08
  left_join(frame, exp_label_map) %>%
    mutate(
      exp_label_x = as.numeric(exp_label_f) + ifelse(outlier_label == "no_outliers", -outlier_dodge, outlier_dodge)
    )
}

outlier_comparison <- rbind(
  compare_model_params(overall_mod, overall_w_outliers, "overall"),
  compare_model_params(original_mod, original_w_outliers, "original"),
  compare_model_params(replication_mod, replication_w_outliers, "replication"),
  extract_model_params(preregistered_mod, "preregistered", "no_outliers")
) %>%
  recode_exp_label

outliers_plot <- ggplot(outlier_comparison, aes(x = exp_label_x, y = estimate, color = outlier_label)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  size = 0.8) +
  geom_hline(yintercept = 0.0, lty = 2) +
  coord_flip() +
  scale_x_continuous("", breaks = 1:4, labels = levels(outlier_comparison$exp_label_f)) +
  scale_y_continuous("Knowledge type x Interference (log-odds)") +
  scale_color_manual("Ambiguous propositions", labels = c("Excluded", "Included"),
                     values = unname(feat_type_colors)) +
  base_theme +
  theme(legend.position = "top", legend.text = element_text(size = 8))
outliers_plot

# ---- fig3
grid.arrange(trial, overall_plot, exp_plot, outliers_plot, nrow = 2)
