library(dplyr)

# ---
# Get questions that are high in imagery and low in facts and vice versa.

ratings <- read.csv("./feature-norms/feature_norms.csv", stringsAsFactors = FALSE)

arrange(ratings, desc(imagery_mean)) %>% 
  select(cue, question, imagery_mean, facts_mean) %>%
  filter(cue %in% c("jacket", "swan")) %>%
  head(n = 2)

arrange(ratings, desc(facts_mean)) %>%
  select(cue, question, imagery_mean, facts_mean) %>%
  filter(cue %in% c("mouse", "camel")) %>%
  head(n = 2)

ratings %>% filter(cue == "bike", ftype == "visual", truth_coded == "yes") %>%
  select(cue, question, imagery_mean, facts_mean)

ratings %>% filter(ftype == "visual", truth_coded == "yes") %>%
  arrange(cue, desc(imagery_mean)) %>%
  select(cue, question, imagery_mean)

# ---
# Get stats for table
library(lme4)

df <- read.csv("./feature-last/feature-last-final.csv", stringsAsFactors = FALSE)
df <- filter(df, subj_id != "MWPF214")

# add in senses_z
# NOTE: The join adds a few rows, and I don't know why.
sensory_ratings <- read.csv("./feature-norms-senses/feature_norms_senses.csv", stringsAsFactors = FALSE)
df <- left_join(df, select(sensory_ratings, cue, question, senses_z))

# amount of visual knowledge
summary(glmer(is_error ~ imagery_z * mask_c + (1|subj_id), data = df, family = binomial))

# amount of nonvisual knowledge
summary(glmer(is_error ~ facts_z * mask_c + (1|subj_id), data = df, family = binomial))

# question difficulty
summary(glmer(is_error ~ diff_z * mask_c + (1|subj_id), data = df, family = binomial))

# question abstractness
summary(glmer(is_error ~ senses_z * mask_c + (1|subj_id), data = df, family = binomial))

# full model
summary(glmer(is_error ~ mask_c * (imagery_z + facts_z + diff_z + senses_z) + (1|subj_id), data = df, family = binomial))
