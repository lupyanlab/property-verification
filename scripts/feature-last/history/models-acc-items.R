require(lme4)

df <- read.csv("./feature-last/feature-last-final.csv")

# Does the mask decrease accuracy as a function of imagery?
acc_imagery <-  glmer(is_correct ~ imagery_z * mask_c + (mask_c|subj_id),
                      data = df, family = "binomial")
acc_imagery.chisqr <- anova(acc_imagery, 
                     update(acc_imagery, . ~ . - imagery_z:mask_c))

# Does the mask decrease accuracy as a function of facts?
acc_facts <- glmer(is_correct ~ facts_z * mask_c + (mask_c|subj_id),
                     data = df, family = "binomial")
acc_facts.chisqr <- anova(acc_facts, 
                   update(acc_facts, . ~ . - facts_z:mask_c))

objs <- c("acc_imagery", "acc_imagery.chisqr",
          "acc_facts", "acc_facts.chisqr")
save(list = objs, file = "./feature-last/models-acc-items.RData")
