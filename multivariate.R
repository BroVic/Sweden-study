# Analysis2.R
# Advanced Analyses
library(MASS)
library(Hmisc)

data<- readRDS("clean_x.rds")
mydata <- data[, c(2, 5, 6, 12, 13, 8)] # extract variables of interest
rm(data)

# define main objects to be used in modelling and analysis
# Formula object with outcome vs. predictors
orgFormula <- concern.stress ~ risks.time + risks.communication + 
  risks.lackemployeecontrol + risks.unclearPolicy + 
  risks.workinghours         # organisational issues

# Formula object for multi-way tabulation/visualization
covars <- ~ risks.time + risks.communication + risks.lackemployeecontrol + 
  risks.unclearPolicy + risks.workinghours + concern.stress


# Build array and multi-way contingency table
(array <- xtabs(covars, data = mydata))
(flat <- ftable(array))


# Visualize
vcd::doubledecker(orgFormula, 
                  data = mydata, main = "Doubledecker plot of the data")
rm(array, covars)


# Proportional odds logit model
ordered.fit <- polr(formula = orgFormula, data = mydata, Hess = TRUE)
summary(ordered.fit)

# Tabulate coefficients 
table.coef <- coef(summary(ordered.fit))
table.coef

# calculate and store p-values
pval <- pnorm(abs(table.coef[, "t value"]), lower.tail = FALSE) * 2
table.coef <- cbind(table.coef, "p-value" = pval)
table.coef
rm(table.coef, pval)

# Calculate 95% confidence intervals
CI <- confint(ordered.fit)
CI
confint.default(ordered.fit)

# Plot the probabilities
plot(effects::allEffects(ordered.fit))

# Obtain the Odds Ratio and match with confidence intervals
odd.ratio <- exp(coef(ordered.fit))
odd.ratio

# Tabulate O.R. and CI
(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI)))
rm(odd.ratio, CI, odds.tabl)

# Compare the fit with an intercept-only model
nullmodel <- polr(concern.stress ~ 1, data = mydata, Hess = TRUE)
deviance(nullmodel) - deviance(ordered.fit)
df.residual(nullmodel) - df.residual(ordered.fit)

# --- Evaluating the assumption of proportional odds ---#
props <- function(y) {
  c('Y >= 1' = qlogis(mean(y >= 1)),
    'Y >= 2' = qlogis(mean(y >= 2)),
    'Y >= 3' = qlogis(mean(y >= 3)))
}

trial <- with(mydata,
               summary(as.numeric(concern.stress) ~ risks.time + 
                         risks.communication + risks.lackemployeecontrol + 
                         risks.unclearPolicy + risks.workinghours, fun = props))
trial

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
rm(trial)

# --- Done with assumption testing ---#

# Fit the model if assumption is met
# Create a dummy dataframe to predict probabilities
ks <- rep(c("Yes", "No"), 600)
data.pred <- as.data.frame(matrix(rep(ks, 5), ncol = 5, byrow = FALSE)); rm(ks)
colnames(data.pred) <- c("risks.time", "risks.communication",
                         "risks.lackemployeecontrol", "risks.unclearPolicy",
                         "risks.workinghours")
data.pred <- cbind(data.pred, predict(ordered.fit, data.pred, type = "probs"))
head(data.pred)

# Tidy the dataframe
data.pred <- tidyr::gather(data.pred, category, probability,
                           `No concern`:`Major concern`, factor_key = TRUE)
head(data.pred)

# Plot the probabilities
p1 <- ggplot(data = data.pred,
             aes(x = category, y = probability, colour = risks.time,
                 group = risks.time)) + geom_point(size = 3) + 
  geom_line(linetype = "dashed", size = 2)
p2 <- ggplot(data = data.pred,
             aes(x = category, y = probability, colour = risks.communication,
                 group = risks.communication)) + geom_point(size = 3) + 
  geom_line(linetype = "dashed", size = 2)
p3 <- ggplot(data = data.pred,
             aes(x = category, y = probability,
                 colour = risks.lackemployeecontrol,
                 group = risks.lackemployeecontrol)) + geom_point(size = 3) + 
  geom_line(linetype = "dashed", size = 2)
p4 <- ggplot(data = data.pred,
             aes(x = category, y = probability, colour = risks.unclearPolicy,
                 group = risks.unclearPolicy)) + geom_point(size = 3) + 
  geom_line(linetype = "dashed", size = 2)
p5 <- ggplot(data = data.pred,
             aes(x = category, y = probability, colour = risks.workinghours,
                 group = risks.workinghours)) + geom_point(size = 3) + 
  geom_line(linetype = "dashed", size = 2)

source("multiplot.R") # from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot(p1, p2, p3, p4, p5, cols = 3)

rm(list = ls())

# End 
