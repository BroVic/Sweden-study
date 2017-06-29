# ... continuation from bully-org_regr.R

theFORMULA <- Formula(myformula)
length(theFORMULA)
(theFORMULA <- formula(theFORMULA, lhs = 3, rhs = 1)) # for this analysis

# Fit the model as beforehand
mod <- polr(theFORMULA, data = mydata, Hess = TRUE)
summary(mod)

# Tabulate coefficients and store p-values
printPV(mod)

# Display these effects
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

# Based on the output, drop variables from model beginning with the 
# least significant to check for any changes

# 1st
(fm_x <- update(theFORMULA, . ~ risks.time + risks.lackemployeecontrol + 
                  risks.unclearPolicy + risks.workinghours))

mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
printPV(mod_x)

# 2nd
(fm_x <- update(theFORMULA, . ~ risks.time + risks.lackemployeecontrol + 
                  risks.workinghours))

mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
printPV(mod_x)

# 3rd
(fm_x <- update(theFORMULA, . ~ risks.lackemployeecontrol + risks.workinghours))
mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
printPV(mod_x)

# 4th
(fm_x <- update(theFORMULA, . ~ risks.workinghours))
mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
printPV(mod_x)

predictors_x <- predictors[5]
eff.obj_x <- effect(predictors_x, mod_x)
plot(eff.obj_x, main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = FALSE)

# Obtain the Odds Ratio and match with 95% confidence intervals
CI <- confint(mod_x)
CI
(odd.ratio <- exp(coef(mod_x)))

# Prediction
ks <- rep(c("Yes", "No"), 600)
data.pred <- as.data.frame(matrix(rep(ks, 1), ncol = 1))
colnames(data.pred) <- "risks.workinghours"
data.pred <- cbind(data.pred, predict(mod_x, data.pred, type = "probs"))
head(data.pred)

# Tidy the dataframe
data.pred <- data.pred %>%
  gather(violence, probability, `No concern`:`Major concern`, factor_key = TRUE)
head(data.pred)

#plot the predicted probabilities
p3 <- ggplot(data = data.pred, 
             aes(x = violence, y = probability, colour = risks.workinghours,
                 group = risks.workinghours)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p3

file.edit("stress-emp_regr.R")
