require(dplyr)
require(reshape2)
require(ggplot2)
require(scales)
require(grid)

df <- read.csv("./feature-last/feature-last-final.csv")

df <- df %.%
  group_by(subj_id) %.%
  mutate(acc_c = is_correct - mean(is_correct, na.rm = T)) %.%
  ungroup() %.%
  group_by(question_id, mask_c) %.%
  summarize(
    imagery_z = unique(imagery_z),
    acc = mean(acc_c, na.rm = T)
  ) %.%
  dcast(question_id + imagery_z ~ mask_c, value.var = "acc") %.%
  plyr::rename(c("-0.5" = "nomask", "0.5" = "mask")) %.%
  mutate(cost_of_mask = nomask - mask)

ggplot(df, aes(x = imagery_z, y = cost_of_mask)) +
  geom_point(position = position_jitter(height = 0.02, width = 0.1))


