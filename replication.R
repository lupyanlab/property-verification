# ---- setup
library(devtools)
load_all("propertyverification")
data(question_first)

library(dplyr)
library(tidyr)

library(ggplot2)
library(scales)

library(lme4)

question_first <- question_first %>%
  recode_mask_type %>%
  recode_feat_type %>%
  filter(subj_id %nin% question_first_outliers)

# ---- run1
feat_type_error_mod <- glmer(is_error ~ mask_c * feat_c + (mask_c * feat_c|subj_id),
                             family = binomial, data = question_first)
summary(feat_type_error_mod)

ggplot(question_first, aes(x = feat_type, y = is_error, fill = mask_type)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_y_continuous(labels = percent) +
  facet_wrap("exp_run")

# ---- run2
