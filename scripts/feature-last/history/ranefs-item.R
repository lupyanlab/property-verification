require(dplyr)


RAN <- coef(acc.imagery)$cue
RAN$cue <- row.names(RAN); row.names(RAN) <- NULL
RAN$rank_mask <- rank(RAN$mask_C, ties.method = "random")

RAN <- plyr::rename(RAN, c("imagery_z:mask_C" = "inter"))

DF <- read.csv("./feature-last/feature-last.csv")

AVE <- DF %.%
  group_by(cue, ftype, truth_coded, qid, mask_C) %.%
  summarize(
    obs = length(is_correct),
    acc = mean(is_correct, na.rm = T)
  ) %.% ungroup()

ALL <- merge(AVE, RAN[,c("cue", "mask_C")], all.x = T)
head(ALL)

ggplot(ALL, aes(x = imagery_z, y = accuracy,))
