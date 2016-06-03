 # Write a function 'printtest() to print out Pearson's Chi-Square test results
# in console.
printtest <- function(x, vec) {
  for (i in vec) {
    result <- chisq.test(x, mydata[, i])$p.value
    print(result)
  }
  }
# Calling the function on all variables as earlier designated
for (i in 2:4) {              # looping the 3 outcome variables
printtest(mydata[, i], covars)
}

library("MASS")
train <- readRDS("training-data.rds")
test <- readRDS("test-data.rds")
dplyr::glimpse(train)

train.fit <- lda(concern.stress~risks.time+risks.workinghours+risks.unclearPolicy+risks.lackemployeecontrol, data = mydata)




# match concerns about bullying with those of violence
physical.threat <- table(mydata$concern.bullying, mydata$concern.violence)
barplot(physical.threat, beside = TRUE, legend = TRUE, xlab = "Bullying",
        legend.text = TRUE)
chisq.test(physical.threat)




# Partition the dataset
index <- caret::createDataPartition(mydata$concern.stress, times = 1, p = .8,
                                    list = FALSE)
train <- mydata[index, ]
test <- mydata[-index, ]
rm(index)

####################################################################
# A function to run CMH test
printCMH <- function(x, vec) { # where x = outcome var, vec = pred. indices
  if (!require(vcdExtra))
    stop("package 'vcdExtra' not found")
  for (i in vec) {
    tab <- table(mydata[, i], x, dnn = c(colnames(mydata[, i]), names(x)))
    result <- CMHtest(tab)
    print(result)
  }
}

for (i in 2:4) {
  printCMH(mydata[, i], c(org_rel, emp_rel))
}

########################################################################
library(effects)
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



###########################################################################
# draw a formatted table
library(rtf)
output <- "tables.doc"
rtf <- RTF(output, width = 8.5, height = 11, font.size = 11, omi = c(1, 1, 1, 1))
flat <- as.data.frame(flat) %>%
  spread(concern.stress, Freq)
head(flat)
colnames(flat) <- gsub("\\.", " ", colnames(flat))
addTable(rtf, flat, font.size = 9, row.names = FALSE, NA.string = "-")
done(rtf)
