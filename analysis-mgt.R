# Analysis of Management Staff Perceptions
library(dplyr)
mydata <- readRDS("clean_2.rds")

## Outcome variables
# Stress - pos 66
# Violence/threats - pos 67
# Bullying/harrassment - pos 68

## i. Organisational Factors 
# •	Time pressure (Pos  69 - MM202.1)
# •	Poor communication between management and employees (Pos 70 - MM202.2)
# •	Long or irregular working hours (Pos 76 - MM202.8)
# •	An unclear human resources policy (Pos 77 - MM202.9)
# •	Lack of employee control in organising their work(Pos 72 - MM202.4)

## ii. Employee related i.e. Factors at individual & group (interpersonal relationship among colleagues) level 
# •	Job insecurity(Pos 73 - MM202.5)
# •	Having to deal with difficult customers, patients, pupils etc. (Pos 74 - MM202.6)
# •	Problems in supervisor - employee relationships(Pos 75 - MM202.7)
# •	Discrimination (for example due to gender, age or ethnicity) (Pos 78 - MM202.10)
# •	Poor co-operation amongst colleagues(Pos 71 - MM202.3)

summary(mydata[, 66:68])
stress <- mydata[, 66]
violence <- mydata[, 67]
bullying <- mydata[, 68]

# Univariate plots of designated outcome variables
barplot(table(stress))
barplot(table(violence))
barplot(table(bullying))

# Bivariate plots:
# A function to plot barcharts for the respective bivariates
check <- function(x, pos) {
  ht <- table(x, mydata[, pos]) 
  barplot(ht,
          beside = TRUE,
          legend = FALSE,
          ylim = c(0, 8000),
          yaxt = "n",
          col = c("blue", "yellow", "red", "green"))
}

oldpar <- par() # plot one outcome per row, one independent variable per column 
par(mar = c(1, 1, 1, 1))
layout(matrix(c(1:10,11:20, 21:30), nrow = 3, ncol = 10, byrow = TRUE))

for (i in 69:78) {              # iteration of plotting across columns 69 to 78
  check(stress, i)         
  check(violence, i)
  check(bullying, i)
}
layout(matrix(1))               # return graphics device to former state
oldpar
rm(oldpar)

# run Chi-squared test of independence
# Write a function 'printtest() to print out Pearson's Chi-Square test results
# in console.
printtest <- function(x, vec) {
  for (i in vec) {
    result <- chisq.test(x, mydata[, i])
    print(result)
  }
}

# Calling the function on all variables as earlier designated
vars <- c(69:78)                # a vector of the covariates...
for (i in 66:68) {              # looping the 3 outcome variables
  printtest(mydata[, i], vars)
}
