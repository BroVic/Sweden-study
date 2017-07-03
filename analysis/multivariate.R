# Analysis2.R
# Advanced Analyses
library(Hmisc)
library(tidyr)
library(effects)

mydata<- readRDS("data-cleaning/clean_x.rds")
mydata <- mydata[, c(1, 4, 5, 11, 12, 7)]

# Create object with outcome vs. predictors formula
orgFormula <- concern.stress ~ risks.time + risks.communication + 
  risks.lackemployeecontrol + risks.unclearPolicy + 
  risks.workinghours

# Build an array and flatten it into a multi-way contingency table
flat <- ftable(xtabs(~ risks.time + risks.communication + risks.lackemployeecontrol + 
  risks.unclearPolicy + risks.workinghours + concern.stress, data = mydata))
flat <- as.data.frame(flat)
head(flat)
flat <- flat %>%
  spread(concern.stress, Freq)
head(flat)

# Visualize the tabulation
vcd::doubledecker(orgFormula, data = mydata, main = "Doubledecker plot of the data")

# Proportional odds logit model
ordered.fit <- MASS::polr(formula = orgFormula, data = mydata, Hess = TRUE)
summary(ordered.fit)

# Display the effects of this model
predictors <- c("risks.time", "risks.communication",
                "risks.lackemployeecontrol", "risks.unclearPolicy",
                "risks.workinghours")
eff.obj1 <- effect("risks.time", ordered.fit)
eff.obj2 <- effect("risks.communication", ordered.fit)
eff.obj3 <- effect("risks.lackemployeecontrol", ordered.fit)
eff.obj4 <- effect("risks.unclearPolicy", ordered.fit)
eff.obj5<- effect("risks.workinghours", ordered.fit)
plot(eff.obj1, main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,
     ncol = 3, more = TRUE)
plot(eff.obj2, main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2,
     nrow = 1, ncol = 3, more = TRUE)
plot(eff.obj3, main = FALSE, grid = TRUE, ylab = "", row = 1, col = 3, 
     nrow = 1, ncol = 3, more = FALSE)
plot(eff.obj4, main = FALSE, grid = TRUE, row = 1, col = 1, 
     nrow = 1, ncol = 3, more = TRUE)
plot(eff.obj5, main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2, 
     nrow = 1, ncol = 3, more = FALSE)


# Tabulate coefficients and store p-values
table.coef <- coef(summary(ordered.fit))
pval <- pnorm(abs(table.coef[, "t value"]), lower.tail = FALSE) * 2
(table.coef <- cbind(table.coef, "p-value" = pval))

# Obtain the Odds Ratio and match with 95% confidence intervals
CI <- confint(ordered.fit)
default.CI <- confint.default(ordered.fit)
odd.ratio <- exp(coef(ordered.fit))
(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI)))

# --- Evaluating the assumption of proportional odds ---
props <- function(y) {
  c('Y >= 1' = qlogis(mean(y >= 1)),
    'Y >= 2' = qlogis(mean(y >= 2)),
    'Y >= 3' = qlogis(mean(y >= 3)))
}
(trial <- with(mydata,
               summary(as.numeric(concern.stress) ~ risks.time + 
                         risks.communication + risks.lackemployeecontrol + 
                         risks.unclearPolicy + risks.workinghours, fun = props)))

# Treat outcome as a binary variable via "cut-off" points to test assumption
glm(I(as.numeric(concern.stress) >= 2) ~ risks.time + 
      risks.communication + risks.lackemployeecontrol + 
      risks.unclearPolicy + risks.workinghours, family = "binomial",
    data = mydata)

glm(I(as.numeric(concern.stress) >= 3) ~ risks.time + 
      risks.communication + risks.lackemployeecontrol + 
      risks.unclearPolicy + risks.workinghours, family = "binomial",
    data = mydata)

trial[, 4] <- trial[, 4] - trial[, 3]
trial[, 3] <- trial[, 3] - trial[, 3]
trial

# Plot the logits
plot(trial, which = 1:3, pch = 1:3, xlab = "logit", main = "",
     xlim = range(trial[, 3:4]))

# Fit the model if assumption is met
# Create a dummy dataframe to predict probabilities
ks <- rep(c("Yes", "No"), 600)
data.pred <- as.data.frame(matrix(rep(ks, 5), ncol = 5, byrow = FALSE))
colnames(data.pred) <- c("risks.time", "risks.communication",
                         "risks.lackemployeecontrol", "risks.unclearPolicy",
                         "risks.workinghours")
data.pred <- cbind(data.pred, predict(ordered.fit, data.pred, type = "probs"))
head(data.pred)

# Tidy the dataframe
data.pred <- data.pred %>%
  gather(category, probability, `No concern`:`Major concern`, factor_key = TRUE)
head(data.pred)

#plot the predicted probabilities
p1 <- ggplot(data = data.pred,
       aes(x = category, y = probability, colour = risks.time, group = risks.time)) + 
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p1
# End 
