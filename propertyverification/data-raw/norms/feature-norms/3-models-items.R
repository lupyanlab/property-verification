require(car)
require(lme4)

df <- read.csv("./feature-norms/feature_norms.csv")

df$ftype_c <- recode(df$ftype, "'nonvisual'=-0.5; 'visual'=0.5")

diff.ftype <- lm(difficulty_z ~ ftype_c, data = df)
diff.imag <- lm(difficulty_z ~ imagery_z, data = df)
diff.fact <- lm(difficulty_z ~ facts_z, data = df)

df$ftype_b <- recode(df$ftype, "'nonvisual'=0; 'visual'=1")

imag.ftype <- glm(ftype_b ~ imagery_z, data = df, family = "binomial")
imag.facts <- glm(ftype_b ~ facts_z, data = df, family = "binomial")

objs <- c("diff.ftype", "diff.imag", "diff.fact",
          "imag.ftype", "imag.facts")
save(list = objs, file = "./feature-norms/models-diff.RData")
