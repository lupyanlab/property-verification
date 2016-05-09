library(dplyr)
library(broom)

library(propertyverificationdata)
data("norms_responses")

norms_responses <- norms_responses %>%
  recode_feat_type

feat_type_diffs <- norms_responses %>%
  group_by(measure) %>%
  do(mod = lm(value ~ feat_c, data = .)) %>%
  glance(mod) %>%
  ungroup %>%
  select(measure, df.residual, statistic, p.value)
feat_type_diffs
