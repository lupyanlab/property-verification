library(dplyr)
library(ggplot2)

library(propertyverificationdata)
data("norms")

norms <- norms %>%
  recode_feat_type

ggplot(norms, aes(x = imagery_mean)) +
  geom_density(aes(fill = feat_type), alpha = 0.5)

head(norms_responses)
