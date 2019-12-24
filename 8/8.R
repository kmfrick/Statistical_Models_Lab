# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));

# Exercise 2

cat("\n\n *** Exercise 2 *** \n\n");

part <- read.table("participation.txt", header = TRUE);
part$nopart <- as.numeric(part$participation == "NO");
part.model <- glm(nopart ~ income, family = "binomial", data = part);
summary(part.model);
part.L.full <- logLik(part.model);
part.model.red <- glm(nopart ~ 1, data=part, family="binomial");
part.L.red <- logLik(part.model.red);
anova(part.model, part.model.red, test="LRT");

# Exercise 3

cat("\n\n *** Exercise 3 *** \n\n");
preg <- read.table("pregnancy.txt", header = TRUE);
preg$duration <- factor(preg$duration, labels=c("preterm", "on term"));
preg$ageclass <- factor(preg$ageclass, labels=c("lower than 21", "21-30", "greater than 30"));
preg$alcohol <- factor(preg$alcohol, labels=c("no", "yes"));
preg$smoke <- factor(preg$smoke, labels=c("no", "yes"));
preg$preterm <- as.numeric(preg$duration == "preterm");
preg.model <- glm(preterm ~ (nutrition + ageclass + alcohol + smoke), "binomial", preg);
preg.model.red <- glm(preterm ~(nutrition + alcohol + smoke), "binomial", preg);
anova(preg.model, preg.model.red, test="LRT");
# b_smokeyes is positive, so tobacco increases odds of preterm birth
# p-value for G test < 0.001, so age is definitely significant

