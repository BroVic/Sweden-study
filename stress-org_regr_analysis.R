# ... continued from multivariate.R where this outcome-predictor pair was used
# to test assumptions for the model
# Effect display and values rendered from fitting model shows that 
# 'risks.communication' & 'risks.unclearPolicy' show no statistically 
# significant with the response variable.
# We run the model again after removing them.
library(ggplot2)
library(tidyr)
library(effects)
library(Formula)
library(rtf)
mydata <- readRDS("clean_x.rds")

myformula <- concern.stress | concern.bullying | concern.violence ~ 
  risks.time + risks.communication + risks.lackemployeecontrol + 
  risks.unclearPolicy + risks.workinghours | risks.poor.cooperation +
  risks.jobinsecurity + risks.difficultpeople + risks.relationship +
  risks.discrimination
length(myformula)
theFORMULA <- Formula(myformula)
length(theFORMULA)
theFORMULA <- formula(theFORMULA, lhs = 1, rhs = 1)

# Subset the formula, removing the influence of the two variables
fm <- update(theFORMULA, . ~ risks.time + risks.lackemployeecontrol + 
               risks.workinghours)
mod <- MASS::polr(fm, data = mydata, Hess = TRUE)
summary(mod)

predictors <- c("risks.time", "risks.lackemployeecontrol", "risks.workinghours")
eff.obj1 <- effect("risks.time", mod)
eff.obj3 <- effect("risks.lackemployeecontrol", mod)
eff.obj5<- effect("risks.workinghours", mod)
plot(eff.obj1, main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = TRUE)
plot(eff.obj3, main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2, 
     nrow = 1, ncol = 3, more = TRUE)
plot(eff.obj5, main = FALSE, grid = TRUE, ylab = "", row = 1, col = 3, 
     nrow = 1, ncol = 3, more = FALSE)

# Tabulate coefficients and store p-values
table.coef <- coef(summary(mod))
pval <- pnorm(abs(table.coef[, "t value"]), lower.tail = FALSE) * 2
(table.coef <- cbind(table.coef, "p-value" = pval))

# Obtain the Odds Ratio and match with 95% confidence intervals
CI <- confint(mod)
default.CI <- confint.default(mod)
odd.ratio <- exp(coef(mod))
(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI)))

# Prediction
ks <- rep(c("Yes", "No"), 600)
data.pred <- as.data.frame(matrix(rep(ks, 3), ncol = 3, byrow = FALSE))
colnames(data.pred) <- c("risks.time", "risks.lackemployeecontrol",
                         "risks.workinghours")
data.pred <- cbind(data.pred, predict(mod, data.pred, type = "probs"))
head(data.pred)

# Tidy the dataframe
data.pred <- data.pred %>%
  gather(category, probability, `No concern`:`Major concern`, factor_key = TRUE)
head(data.pred)

#plot the predicted probabilities
p1 <- ggplot(data = data.pred, aes(x = category, y = probability, 
                                   colour = risks.time, group = risks.time)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p1

file.edit("bully-org_regr.R")