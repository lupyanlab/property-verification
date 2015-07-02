require(lme4)

df <- read.csv("./feature-last/feature-last-final.csv")

# Is there a difference in accuracy on the nonmasked trials?
df.nomask <- subset(df, mask_type == "nomask")
acc_feature.nomask <- glmer(is_correct ~ feature_c + (feature_c|subj_id),
                            data = df.nomask, family = "binomial")
acc_feature.nomask.chisqr <- anova(acc_feature.nomask, 
                            update(acc_feature.nomask, . ~ . - feature_c))

# Is there an effect of the mask on the nonvisual trials?
df.nonvis <- subset(df, feature_type == "nonvisual")
acc_mask.nonvis <- glmer(is_correct ~ mask_c + (mask_c|subj_id),
                         data = df.nonvis, family = "binomial")
acc_mask.nonvis.chisqr <- anova(acc_mask.nonvis,
                         update(acc_mask.nonvis, . ~ . - mask_c))

# Is there an effect of the mask on the visual trials?
df.vis <- subset(df, feature_type == "visual")
acc_mask.vis <- glmer(is_correct ~ mask_c + (mask_c|subj_id),
                      data = df.vis, family = "binomial")
acc_mask.vis.chisqr <- anova(acc_mask.vis,
                      update(acc_mask.vis, . ~ . - mask_c))

# Does the mask decrease accuracy more on the visual trials?
acc_inter <- glmer(is_correct ~ feature_c * mask_c + (feature_c * mask_c|subj_id), 
                   data = df, family = "binomial")
acc_inter.chisqr <- anova(acc_inter, 
                   update(acc_inter, . ~ . - feature_c:mask_c))

objs <- c("acc_feature.nomask", "acc_feature.nomask.chisqr",
          "acc_mask.nonvis", "acc_mask.nonvis.chisqr",
          "acc_mask.vis", "acc_mask.vis.chisqr",
          "acc_inter", "acc_inter.chisqr")
save(list = objs, file = "./feature-last/models-acc.RData")
