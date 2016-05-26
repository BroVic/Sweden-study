# Analysis2.R
# Advanced Analyses
library(MASS)
library(Hmisc)
library(tidyr)


data<- readRDS("clean_x.rds")
mydata <- data[, c(2, 5, 6, 12, 13, 8)] # extract variables of interest

# Partition the dataset
index <- caret::createDataPartition(mydata$concern.stress, times = 1, p = .8,
                                    list = FALSE)
train <- mydata[index, ]
test <- mydata[-index, ]
rm(index)

# group the predictors
orgFormula <- concern.stress ~ risks.time + risks.communication + risks.lackemployeecontrol + 
  risks.unclearPolicy +  risks.workinghours         # organisational issues

# Build a multi-way contingency table
ftable(xtabs(~ concern.stress + risks.time + 
               risks.communication + risks.lackemployeecontrol + 
               risks.unclearPolicy + risks.workinghours, data = train))

# Some visualizations
col <- c("green", "yellow", "red")
# Draw a spine plot
spineplot(concern.stress ~ risks.time, data = mydata, col = col)

vcd::mosaic(concern.stress ~ 
              risks.time+risks.communication+risks.unclearPolicy,
            data = mydata, shade = TRUE,
            highlighting_fill = col)
x <- table(mydata$concern.stress, mydata$risks.time)
x
vcd::assoc(x)

# Proportional odds logistic regression?
ordered.fit <- polr(formula = orgFormula, data = mydata, Hess = TRUE)

summary(ordered.fit)

table.coef <- coef(summary(ordered.fit))
table.coef

# calculate and store p-values
pval <- pnorm(abs(table.coef[, "t value"]), lower.tail = FALSE) * 2
table.coef <- cbind(table.coef, "p-value" = pval)
table.coef

CI <- confint(ordered.fit)
CI
confint.default(ordered.fit)

# Odds Ratio
exp(coef(ordered.fit))

# O.R. and CI
table_cior <- exp(cbind("O.R." = coef(ordered.fit), CI))
table_cior

# Evaluating assumption of proportional odds
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

glm(I(as.numeric(concern.stress) >= 2) ~ risks.time, family = "binomial",
    data = mydata)

glm(I(as.numeric(concern.stress) >= 3) ~ risks.time, family = "binomial",
    data = mydata)

trial[, 4] <- trial[, 4] - trial[, 3]
trial[, 3] <- trial[, 3] - trial[, 3]
trial

plot(trial, which = 1:3, pch = 1:3, xlab = "logit", main = "",
     xlim = range(trial[, 3:4]))

data.pred <- data.frame(risks.time = rep(c("Yes", "No"), 600),
                        risks.communication = rep(c("Yes", "No"), 600),
                        risks.lackemployeecontrol = rep(c("Yes", "No"), 600),
                        risks.unclearPolicy = rep(c("Yes", "No"), 600),
                        risks.workinghours = rep(c("Yes", "No"), 600))
data.pred <- cbind(data.pred, predict(ordered.fit, data.pred, type = "probs"))
head(data.pred)

# Reshaping the dataframe
data.pred <- data.pred %>%
  gather(category, probability, `No concern`:`Major concern`,
                factor_key = TRUE)
head(data.pred)



Vectorize(detach)(name = paste0("package:",c("MASS", "Hmisc", "lattice", "survival", "Formula", "ggplot2", "tidyr")), character.only = TRUE)
rm(list = ls())
# End 
