# ---- setup
library(dplyr)
library(ggplot2)
library(scales)
library(lme4)
library(broom)
library(AICcmodavg)

base_theme <- theme_minimal(base_size = 16) +
  theme(axis.ticks = element_blank())

scale_x_mask <- scale_x_continuous("", breaks = c(-0.5, 0.5), labels = c("Blank screen", "Visual interference"))

scale_y_error <- scale_y_continuous("Error Rate", labels = percent)
scale_y_rt <- scale_y_continuous("Reaction Time (msec)")
scale_y_options <- list(
  is_error = scale_y_error,
  rt = scale_y_rt
)

light_blue <- "#9ecae1"
dark_blue <- "#08519c"
light_green <- "#a1d99b"
dark_green <- "#006d2c"

scale_fill_trial_type <- scale_fill_manual(values = c(dark_green, light_green, dark_blue, light_blue))

iv_colors_options <- list(
  imagery_z = c(light_blue, dark_blue),
  facts_z = c(light_green, dark_green)
)

scale_x_amount <- function(iv) {
  axis_title_options <- list(
    imagery_z = "Amount of visual knowledge",
    facts_z = "Amount of encyclopedic knowledge"
  )
  scale_x_continuous(axis_title_options[[iv]])
}

plot_feat_type <- function(frame, dv) {
  # switch on dv
  scale_y <- scale_y_options[[dv]]

  # use aes_string so dv can be provided
  ggplot(frame, aes_string(x = "mask_c", y = dv)) +
    geom_bar(aes(fill = trial_type, width = 1.0),
             stat = "summary", fun.y = "mean") +
    facet_wrap("feat_label") +
    scale_x_mask +
    scale_y +
    scale_fill_trial_type +
    guides(fill = "none") +
    base_theme
}

continuous_x_preds <- expand.grid(
  mask_type = c("nomask", "mask"),
  amount_of_knowledge = seq(-3, 3, by = 0.1),
  stringsAsFactors = FALSE
)

get_mod_preds <- function(mod, iv) {
  x_preds <- continuous_x_preds %>%
    rename_(.dots = setNames("amount_of_knowledge", iv)) %>%
    recode_property_verification_data
  y_preds <- predictSE(mod, newdata = x_preds, se = TRUE)
  preds <- cbind(x_preds, y_preds) %>%
    as.data.frame
  preds
}

plot_amount_of_knowledge <- function(frame, mod, iv) {
  # switch on iv
  scale_x <- scale_x_amount(iv)
  scale_color <- scale_color_manual(values = iv_colors_options[[iv]])
  
  mod_preds <- get_mod_preds(mod, iv)
  
  ggplot(frame, aes_string(x = iv, y = "is_error")) +
    geom_smooth(aes(y = fit, ymin = fit - se.fit, ymax = fit + se.fit, color = mask_f),
                stat = "identity", data = mod_preds) +
    scale_x +
    scale_y_error +
    scale_color +
    guides(color = "none") +
    base_theme
}

# ---- data
# devtools::install_github("property-verification", "lupyanlab", subdir = "propertyverificationdata")
library(propertyverificationdata)
data(question_first)
data(norms)

question_first <- question_first %>%
  tidy_property_verification_data %>%
  recode_property_verification_data %>%
  label_ambiguous_propositions %>%
  label_outlier_subjects %>%
  left_join(norms)

# ---- filters
question_first <- question_first %>%
  filter(
    agreement != "ambiguous",
    outlier == FALSE
  )

# ---- feat-type-error
feat_type_error_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                             family = binomial, data = question_first)
tidy(feat_type_error_mod, effects = "fixed")

plot_feat_type(question_first, "is_error") +
  ggtitle("Effect of interference on error rate")

# ---- feat-type-error-by-exp-run
plot_feat_type(question_first, "is_error") +
  facet_grid(exp_run_label ~ feat_label) +
  ggtitle("Effect of interference on error rate")

# ---- feat-type-rt
feat_type_rt_mod <- lmer(rt ~ feat_c * mask_c + (1|subj_id), data = question_first)
tidy(feat_type_rt_mod, effects = "fixed")

plot_feat_type(question_first, "rt") +
  ggtitle("Effect of interference on reaction time")

# ---- imagery
imagery_mod <- glmer(is_error ~ imagery_z * mask_c + (1|subj_id),
                     family = binomial, data = question_first)
tidy(imagery_mod, effects = "fixed")

plot_amount_of_knowledge(question_first, imagery_mod, "imagery_z")

# ---- facts
facts_mod <- glmer(is_error ~ facts_z * mask_c + (1|subj_id),
                   family = binomial, data = question_first)
tidy(facts_mod, effects = "fixed")

plot_amount_of_knowledge(question_first, facts_mod, "facts_z")