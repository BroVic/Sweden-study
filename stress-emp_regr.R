# ... continued from viol-org_regr.R 
source("viol-org_regr.R")

length(myformula)
theFORMULA <- Formula(myformula)
length(theFORMULA)
(theFORMULA <- formula(theFORMULA, lhs = 1, rhs = 2))

# Fit the polytomous logistic model
mod <- polr(theFORMULA, data = mydata, Hess = TRUE)
summary(mod)

# Tabulate coefficients and store p-values
printPV(mod)

# Display these effects
predictors <- c("risks.poor.cooperation", "risks.jobinsecurity",
                "risks.difficultpeople", "risks.relationships",
                "risks.discrimination")
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

# refit model after extration of variables with relative large p-values
(fm_x <- update(theFORMULA, . ~ risks.poor.cooperation + risks.jobinsecurity +
                risks.relationships))

mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
summary(mod_x)
printPV(mod_x)                  # NB: t value of job insecurity increases!!!
predictors_x <- predictors[c(1, 2, 4)]

eff.obj_x <- list(NA)
for (i in 1:3) {
  eff.obj_x[[i]] <- effect(predictors_x[i], mod_x)
}
plot(eff.obj_x[[1]], main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = TRUE)
plot(eff.obj_x[[2]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2,
     nrow = 1, ncol = 3, more = TRUE)
plot(eff.obj_x[[3]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 3, 
     nrow = 1, ncol = 3, more = FALSE)
# Also compare plots of same variables side by side

# Obtain the Odds Ratio and match with 95% confidence intervals
CI <- confint(mod_x)
default.CI <- confint.default(mod_x)
odd.ratio <- exp(coef(mod_x))
(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI)))

# Prediction
ks <- rep(c("Yes", "No"), 600)
data.pred <- as.data.frame(matrix(rep(ks, 3), ncol = 3, byrow = FALSE))
colnames(data.pred) <- predictors_x
data.pred <- cbind(data.pred, predict(mod_x, data.pred, type = "probs"))
head(data.pred)

# Tidy the dataframe
data.pred <- data.pred %>%
  gather(category, probability, `No concern`:`Major concern`, factor_key = TRUE)
head(data.pred)

#plot the predicted probabilities
p1 <- ggplot(data = data.pred,
             aes(x = category, y = probability, colour = risks.jobinsecurity,
                 group = risks.jobinsecurity)) + 
  geom_point(size = 3) + 
  geom_line(linetype = "dashed", size = 2)
p1

file.edit("bully-emp_regr.R")
