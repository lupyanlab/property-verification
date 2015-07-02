library(dplyr)
library(lme4)
library(lmerTest)

df <- read.csv("./feature-last/feature-last-final.csv", stringsAsFactors = FALSE)
df <- filter(df, subj_id != "MWPF214")

info <- read.csv("./cue_info.csv", stringsAsFactors = FALSE)
info$animacy_c <- info$animacy - 0.5
info$handheld_c <- ifelse(info$handheld == -1, NA, info$handheld - 0.5)

arrange(info, animacy, handheld)

df <- merge(df, info, all.x = TRUE)

base <- lmer(is_error ~ mask_c * feat_c + (1|subj_id), data = df)
summary(base)

by_animacy <- lmer(is_error ~ mask_c * feat_c * animacy_c + (1|subj_id), data = df)
summary(by_animacy)

animate <- lmer(is_error ~ mask_c * feat_c + (1|subj_id),
                data = filter(df, animacy == 1))
summary(animate)

inanimate <- lmer(is_error ~ mask_c * feat_c + (1|subj_id),
                  data = filter(df, animacy == 0))
summary(inanimate)

df %>% filter(animacy == 0) %>%
  group_by(handheld, feat_type) %>%
  summarize(
    error = mean(is_error, na.rm = T)
  )

handheld <- lmer(is_error ~ mask_c * feat_c * handheld_c + (1|subj_id), 
                 data = filter(df, animacy == 0))
summary(handheld)
