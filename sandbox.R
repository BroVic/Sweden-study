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
