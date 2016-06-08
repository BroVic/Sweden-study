# ... continued from stress-org_regr.R 
library(MASS)
source("stress-org_regr_analysis.R")

length(myformula)
theFORMULA <- Formula(myformula)
length(theFORMULA)
(theFORMULA <- formula(theFORMULA, lhs = 2, rhs = 1))

# Fit the polytomous logistic model
mod <- polr(theFORMULA, data = mydata, Hess = TRUE)
summary(mod)

# Tabulate coefficients and store p-values
# Write a function to do this easily across board
printPV <- function(x) {
  table.coef <- coef(summary(x))
  pval <- pnorm(abs(table.coef[, "t value"]), lower.tail = FALSE) * 2
  table.coef <- cbind(table.coef, "p-value" = pval)
  print(table.coef)
}
# Applying the function
printPV(mod)

# Display these effects
predictors <- c("risks.time", "risks.communication",
                "risks.lackemployeecontrol", "risks.unclearPolicy",
                "risks.workinghours")
eff.obj <- list(NA)
for (i in 1:5) {
  eff.obj[[i]] <- effect(predictors[i], mod)
}

plot(eff.obj[[1]], main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = TRUE)
plot(eff.obj[[2]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2, 
     nrow = 1, ncol = 3, more = TRUE)
plot(eff.obj[[3]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 3, 
     nrow = 1, ncol = 3, more = FALSE)
plot(eff.obj[[4]], main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = TRUE)
plot(eff.obj[[5]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2, 
     nrow = 1, ncol = 3, more = FALSE)

# Based on the output, drop variable with non-significant effect from formula
(fm_x <- update(theFORMULA, . ~ risks.time + risks.workinghours))

mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
summary(mod_x)
printPV(mod_x)
predictors_x <- predictors[c(1, 5)]

eff.obj_x <- list(NA)
for (i in 1:2) {
  eff.obj_x[[i]] <- effect(predictors_x[i], mod_x)
}
plot(eff.obj_x[[1]], main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = TRUE)
plot(eff.obj_x[[2]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2, 
     nrow = 1, ncol = 3, more = FALSE)


# Obtain the Odds Ratio and match with 95% confidence intervals
CI <- confint(mod_x)
default.CI <- confint.default(mod_x)
odd.ratio <- exp(coef(mod_x))
(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI)))

# Prediction
ks <- rep(c("Yes", "No"), 600)
data.pred <- as.data.frame(matrix(rep(ks, 2), ncol = 2, byrow = FALSE))
colnames(data.pred) <- c("risks.time", "risks.workinghours")
data.pred <- cbind(data.pred, predict(mod_x, data.pred, type = "probs"))
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

file.edit("viol-org_regr.R")
