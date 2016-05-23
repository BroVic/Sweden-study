# Analysis2.R
# Advanced Analyses
library(MASS)
library(Hmisc)
library(ggplot2)

mydata<- readRDS("clean_x.rds")

col <- c("green", "yellow", "red")
# Run sample code for visualization and analyses
spineplot(concern.stress ~ risks.time, data = mydata, col = col)
vcd::mosaic(concern.stress ~ 
              risks.time+risks.communication+risks.unclearPolicy,
            data = mydata, shade = TRUE,
            highlighting_fill = col)
x <- table(mydata$concern.stress, mydata$risks.time)
x
vcd::assoc(x)

# Partition the dataset
index <- createDataPartition(mydata$concern.stress, times = 1, p = .8,
                             list = FALSE)
train <- mydata[index, ]
test <- mydata[-index, ]
rm(index)

# Logistic regression
glm.fitted <- glm(concern.stress ~ risks.time + risks.communication + 
                    risks.lackemployeecontrol + risks.unclearPolicy + 
                    risks.workinghours, data = mydata, family = "binomial")
summary(glm.fitted)


# Proportional odds logistic regression
# Subset, selecting the variables of interest
mydata <- mydata[, c(2, 5, 6, 12, 13, 8)]
ordered.fit <- polr(concern.stress ~ risks.time + risks.communication + 
                            risks.lackemployeecontrol + risks.unclearPolicy + 
                            risks.workinghours, data = mydata, Hess = TRUE)

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

plot(trial, which = 1:3, pch = 1:3, xlab = "logit", main = " ",
     xlim = range(trial[, 3:4]))

data.regr <- data.frame(risks.time = rep(0:1, 1200),
                        risks.communication = rep(0:1, 1200),
                        risks.lackemployeecontrol = rep(0:1, 1200),
                        risks.unclearPolicy = rep(0:1, 1200),
                        risks.workinghours = rep(0:1, 1200))
data.regr <- cbind(data.regr, predict(ordered.fit, data.regr, type = "probs"))
head(data.regr)

# Reshaping the dataframe
tidy.regr <- data.regr %>%
  tidyr::gather(category, Probability, `No concern`:`Major concern`,
                factor_key = TRUE)

# Boxplot of the predicted probabilities
png("plots/probplot-stress_orgvars.png")
layout(matrix(1:6, nrow = 2, byrow = TRUE))
for (i in 2:6) {
boxplot(tidy.regr$Probability ~ tidy.regr[, i],
        col = c("pink", "blue"))
}
layout(matrix(1))
dev.off()

# End