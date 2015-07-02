require(lme4)

df <- read.csv("./feature-last/feature-last-final.csv")

# Does the mask influence reaction times overall?
rt_mask <- lmer(rt ~ mask_c + (mask_c|subj_id), data = df)
rt_mask.chisqr <- anova(rt_mask, update(rt_mask, . ~ . - mask_c))

rt_mask.satt <- lmerTest::lmer(rt ~ mask_c + (mask_c|subj_id), data = df)
rt_mask.satt.summ <- lmerTest::summary(rt_mask.satt)
rt_mask.satt.anova <- lmerTest::anova(rt_mask.satt)

# Does the mask increase reaction time more on the visual trials?
rt_feature <- lmer(rt ~ feature_c * mask_c + (1|subj_id) + (0 + feature_c * mask_c|subj_id), data = df)
rt_feature.chisqr <- anova(rt_feature, update(rt_feature, . ~ . - feature_c:mask_c))

rt_feature.satt <- lmerTest::lmer(rt ~ feature_c * mask_c + (feature_c * mask_c|subj_id), data = df)
rt_feature.satt.summ <- lmerTest::summary(rt_feature.satt)
rt_feature.satt.anova <- lmerTest::anova(rt_feature.satt)

# Does the mask increase reaction time as a function of imagery?
rt_imagery <-  lmer(rt ~ imagery_z * mask_c + (mask_c|subj_id), data = df)
rt_imagery.chisqr <- anova(rt_imagery, update(rt_imagery, . ~ . - imagery_z:mask_c))

rt_imagery.satt <- lmerTest::lmer(rt ~ imagery_z * mask_c + (mask_c|subj_id), data = df)
rt_imagery.satt.summ <- lmerTest::summary(rt_imagery.satt)
rt_imagery.satt.anova <- lmerTest::anova(rt_imagery.satt)

objs <- c("rt_mask", "rt_mask.chisqr", "rt_mask.satt.summ", "rt_mask.satt.anova",
          "rt_feature", "rt_feature.chisqr", "rt_feature.satt.summ", "rt_feature.satt.anova",
          "rt_imagery", "rt_imagery.chisqr", "rt_imagery.satt.summ", "rt_feature.satt.anova")
save(list = objs, file = "./feature-last/models-rt.RData")
