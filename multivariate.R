# Analysis2.R
# Advanced Analyses
library(MASS)
library(Hmisc)
library(tidyr)
library(vcd)
library(vcdExtra)
library(gmodels)

data<- readRDS("clean_x.rds")
mydata <- data[, c(2, 5, 6, 12, 13, 8)] # extract variables of interest
rm(data)

# Partition the dataset
index <- caret::createDataPartition(mydata$concern.stress, times = 1, p = .8,
                                    list = FALSE)
train <- mydata[index, ]
test <- mydata[-index, ]
rm(index)

# define main objects to be used in modelling and analysis
# Formula object with outcome vs. predictors
orgFormula <- concern.stress ~ risks.time + risks.communication + 
  risks.lackemployeecontrol + risks.unclearPolicy + 
  risks.workinghours         # organisational issues

# Formula object for multi-way association
covars <- ~ risks.time + risks.communication + risks.lackemployeecontrol + 
  risks.unclearPolicy + risks.workinghours + concern.stress


# Build array and multi-way contingency table
(array <- xtabs(covars, data = mydata))
(flat <- ftable(array))

CMHtest(flat)                      # Cochran-Mantel-Haenszel test

# Visualize
doubledecker(orgFormula, data = mydata, main = "Doubledecker plot of the data")
rm(array, covars)


# Proportional odds logit model
ordered.fit <- polr(formula = orgFormula, data = train, Hess = TRUE)
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

# Obtain the Odds Ratio and match with confidence intervals
odd.ratio <- exp(coef(ordered.fit))
odd.ratio

# Tabulate O.R. and CI
(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI)))
rm(odd.ratio, CI, odds.tabl)
# --- Evaluating the assumption of proportional odds ---
props <- function(y) {
  c('Y >= 1' = qlogis(mean(y >= 1)),
    'Y >= 2' = qlogis(mean(y >= 2)),
    'Y >= 3' = qlogis(mean(y >= 3)))
}

trial <- with(train,
               summary(as.numeric(concern.stress) ~ risks.time + 
                         risks.communication + risks.lackemployeecontrol + 
                         risks.unclearPolicy + risks.workinghours, fun = props))
trial

# Treat outcome as a binary variable via "cut-off" points to test assumption
glm(I(as.numeric(concern.stress) >= 2) ~ risks.time + 
      risks.communication + risks.lackemployeecontrol + 
      risks.unclearPolicy + risks.workinghours, family = "binomial",
    data = train)

glm(I(as.numeric(concern.stress) >= 3) ~ risks.time + 
      risks.communication + risks.lackemployeecontrol + 
      risks.unclearPolicy + risks.workinghours, family = "binomial",
    data = train)


trial[, 4] <- trial[, 4] - trial[, 3]
trial[, 3] <- trial[, 3] - trial[, 3]
trial

# Plot the logits
plot(trial, which = 1:3, pch = 1:3, xlab = "logit", main = "",
     xlim = range(trial[, 3:4]))
rm(trial)


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


# cleaning up search path and the workspace
Vectorize(detach)(name = paste0("package:",
                                c("MASS", "Hmisc", "lattice", "survival",
                                  "Formula", "ggplot2", "tidyr", "vcd", "grid",
                                  "vcdExtra", "gnm")),
                  character.only = TRUE)
rm(list = ls())

# End 
