# ... continued from bully-emp_regr.R 

length(myformula)
theFORMULA <- Formula(myformula)
length(theFORMULA)
(theFORMULA <- formula(theFORMULA, lhs = 3, rhs = 2))

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

# check for any changes on removal of least significant predictors
# No. 1 - remove the 3 predictors with clearly lowest t values
(fm_x <- update(theFORMULA, . ~ risks.jobinsecurity + risks.relationships))
mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
printPV(mod_x)                  

# No. 2 - remove job insecurity
(fm_x <- update(theFORMULA, . ~ risks.relationships))
mod_x <- polr(formula = fm_x, data = mydata, Hess = TRUE)
printPV(mod_x) 

predictors_x <- predictors[4]
eff.obj_x <- effect(predictors_x, mod_x)

plot(eff.obj_x, main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = FALSE)

# Obtain the Odds Ratio and match with 95% confidence intervals
CI <- confint(mod_x)
(odd.ratio <- exp(coef(mod_x)))
(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI)))

# Prediction
data.pred <- as.data.frame(matrix(ks, ncol = 1))
colnames(data.pred) <- predictors_x
data.pred <- cbind(data.pred, predict(mod_x, data.pred, type = "probs"))
head(data.pred)

# Tidy the dataframe
data.pred <- data.pred %>%
  gather(category, probability, `No concern`:`Major concern`, factor_key = TRUE)
head(data.pred)

#plot the predicted probabilities
p6 <- ggplot(data = data.pred,
             aes(x = category, y = probability, colour = risks.relationships,
                 group = risks.relationships)) + 
  geom_point(size = 3) + 
  geom_line(linetype = "dashed", size = 2)
p6
