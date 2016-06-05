# scaling.R
# A Scale-Up of the multivariate analysis across all the variable pairs

library(ggplot2)
library(tidyr)
library(effects)
library(Formula)

# Load the data
mydata<- readRDS("clean_x.rds")

# Create formula object for use in this script
myformula <- concern.stress | concern.bullying | concern.violence ~ 
  risks.time + risks.communication + risks.lackemployeecontrol + 
  risks.unclearPolicy + risks.workinghours | risks.jobinsecurity + 
  risks.difficultpeople + risks.relationships + risks.discrimination + 
  risks.poor.cooperation

theFORMULA <- Formula(myformula)

########### This is yet to be treated #######################################
# Build an array and flatten it into a multi-way contingency table          #
flat <- ftable(xtabs(~ risks.time + risks.communication +                   # 
                       risks.lackemployeecontrol + risks.unclearPolicy +    #
                       risks.workinghours + concern.stress, data = mydata)) #
flat <- as.data.frame(flat)                                                 #
flat <- flat %>%                                                            # 
  spread(concern.stress, Freq)                                              #
#############################################################################


# Visualize the tabulation
for (i in 1:3) {
  for (j in 1:2) {
    dd <- formula(theFORMULA, lhs = i, rhs = j)
    vcd::doubledecker(dd, data = mydata)
  }
}

# Proportional odds logit models for each pair of outcome-predictors
ordered.fit <- list(NA)                 # empty list to store the 6 models
for (j in 1:3) {
  for (k in 1:2) {
    ff <- formula(theFORMULA, lhs = j, rhs = k)
    for (i in 1:6) {
      mod <- MASS::polr(formula = ff, data = mydata, Hess = TRUE)
      mod <- summary(mod)     # each summary printed to console
      print(mod)
      ordered.fit[[i]] <- mod
    }
  }
}


# Display the effects of this model
# Prepare vectors of each of the predictor sets
predictors1 <- c("risks.time", "risks.communication",
                 "risks.lackemployeecontrol", "risks.unclearPolicy",
                 "risks.workinghours")
predictors2 <-c("risks.jobinsecurity", "risks.difficultpeople",
                "risks.relationships", "risks.discrimination",
                "risks.poor.cooperation")

################# Effect Displays of the models (yet to be scaled up) ########
for (i in 1:3){                                                              #
  eff.obj <- list(NA)                                                        #
  for (j in 1:5)                                                             #
    eff.obj[[j]] <- effect(predictors1[j], ordered.fit[[i]])                 #
  plot(eff.obj[[1]], main = FALSE, grid = TRUE, row = 1, col = 1, nrow = 1,  #
       ncol = 3, more = TRUE)                                                #
  plot(eff.obj[[2]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2, #
       nrow = 1, ncol = 3, more = TRUE)                                      #
  plot(eff.obj[[3]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 3, #
       nrow = 1, ncol = 3, more = FALSE)                                     #
  plot(eff.obj[[4]], main = FALSE, grid = TRUE, row = 1, col = 1,            #
       nrow = 1, ncol = 3, more = TRUE)                                      #
  plot(eff.obj[[5]], main = FALSE, grid = TRUE, ylab = "", row = 1, col = 2, #
       nrow = 1, ncol = 3, more = FALSE)                                     #
}                                                                            #
rm(eff.obj)                                                                  #
##############################################################################


# Tabulate coefficients and store p-values
for (i in 1:6) {
  table.coef <- coef(summary(ordered.fit[[i]]))
  pval <- pnorm(abs(table.coef[, "t value"]), lower.tail = FALSE) * 2
  print(table.coef <- cbind(table.coef, "p-value" = pval)) # print to console
}

# Obtain the Odds Ratio and match with 95% confidence intervals
for (i in 1:6) {
  CI <- confint(ordered.fit[[i]])
  default.CI <- confint.default(ordered.fit[[i]])
  odd.ratio <- exp(coef(ordered.fit[[i]]))
  print(odds.tabl <- cbind("O.R." = odd.ratio, exp(CI))) # print to console
}

# Create a dummy dataframe to predict probabilities
data.pred <- list(NA)             # Blank list to contain all the dataframes
for (i in 1:6){
  ks <- rep(c("Yes", "No"), 600)
  data.pred[[i]] <- as.data.frame(matrix(rep(ks, 5), ncol = 5, byrow = FALSE))
  if (i <= 3)
    colnames(data.pred[[i]]) <- predictors1 # organisation related predictors
  else if (i > 3)
    colnames(data.pred[[i]]) <- predictors2 # employee related predictors
}

# This will add output variable with predicted probabilities to the dataframes
for (i in 1:6) {
  data.pred[[i]] <- as.data.frame(data.pred[[i]])
  data.pred[[i]] <- cbind(data.pred[[i]],
                        predict(ordered.fit[[i]], data.pred[[i]],
                                type = "probs"))
}

# Tidy the dataframes i.e. rearrange them
for (i in 1:6) {
  data.pred[[i]] <- data.pred[[i]] %>%
    gather(category,
           probability, `No concern`:`Major concern`, factor_key = TRUE)
}

#plot the predicted probabilities

p1 <- ggplot(data = data.pred[[1]],
             aes(x = category, 
                 y = probability, colour = risks.time, group = risks.time)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)

p2 <- ggplot(data = data.pred[[2]],
             aes(x = category, 
                 y = probability, colour = risks.time, group = risks.time)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p3 <- ggplot(data = data.pred[[3]],
             aes(x = category, 
                 y = probability, colour = risks.time, group = risks.time)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p4 <- ggplot(data = data.pred[[4]],
             aes(x = category, y = probability, colour = risks.jobinsecurity,
                 group = risks.jobinsecurity)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p5 <- ggplot(data = data.pred[[5]],
             aes(x = category, y = probability, colour = risks.jobinsecurity,
                 group = risks.jobinsecurity)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p6 <- ggplot(data = data.pred[[6]],
             aes(x = category, y = probability, colour = risks.jobinsecurity,
                 group = risks.jobinsecurity)) +
  geom_point(size = 3) + geom_line(linetype = "dashed", size = 2)
p1; p2; p3; p4; p5; p6

# put them together in a single plot
source("multiplot.R")
multiplot(p1, p4, p2, p5, p3, p6, cols = 3)
# End 
