# Analysis of Management Staff Perceptions

# Having reduced the no. of variable, new positions are assigned, where new
# position = old position - 64 e.g Stress (pos 66) becomes pos 2 in the dataframe

## Outcome variables:
# Stress - pos 2
# Violence/threats - pos 3
# Bullying/harrassment - pos 4

## i. Organisational Factors 
# •	Time pressure (Pos  5 - MM202.1)
# •	Poor communication between management and employees (Pos 6 - MM202.2)
# •	Long or irregular working hours (Pos 12 - MM202.8)
# •	An unclear human resources policy (Pos 13 - MM202.9)
# •	Lack of employee control in organising their work(Pos 8 - MM202.4)

## ii. Employee related i.e. Factors at individual & group (interpersonal relationship among colleagues) level 
# •	Job insecurity(Pos 9 - MM202.5)
# •	Having to deal with difficult customers, patients, pupils etc. (Pos 10 - MM202.6)
# •	Problems in supervisor - employee relationships(Pos 11 - MM202.7)
# •	Discrimination (for example due to gender, age or ethnicity) (Pos 14 - MM202.10)
# •	Poor co-operation amongst colleagues(Pos 7 - MM202.3)

#################################################################################
mydata <- readRDS("clean_x.rds") # load the data

summary(mydata[, 2:4])
stress <- mydata[, 2]; attr(stress, which = "name") <- "STRESS"
violence <- mydata[, 3]; attr(violence, which = "name") <- "VIOLENCE"
bullying <- mydata[, 4]; attr(bullying, which = "name") <- "BULLYING"

covars <- c(5:14)                # a vector of the covariates...

# Univariate plots of designated outcome variables
barplot(table(stress), ylim = c(0, max(table(stress))),
  main = "Is work-related stress a concern?")
barplot(table(violence),
        ylim = c(0, max(table(violence))),
        main = "Is violence or threat of violence a concern?")
barplot(table(bullying), ylim = c(0, max(table(bullying))),
        main = "Is bullying or harrassment a concern?")

# Goodness of Fit
chisq.test(table(stress))
chisq.test(table(violence))
chisq.test(table(bullying))


# Bivariate Analyses
# Introduce new vectors to represent indices of independent variables
org_rel <- c(5, 6, 8, 12, 13) # organisational factors
emp_rel <- c(7, 9:11, 14)     # employee-related factors

# Build contingengy tables and print to console (all)
for (i in 2:4) {
  for (j in covars)
    print(table(mydata[, i], mydata[, j],
                dnn = c(colnames(mydata[i]), colnames(mydata[j]))))
}
# run Chi-squared test of independence
# Write a function 'printtest() to print out Pearson's Chi-Square test results
# in console.
printchisq <- function(x, vec) {
  for (i in vec) {
    result <- chisq.test(x, mydata[, i])
    print(result)
  }
}
# Calling the function on all variables as earlier designated
for (i in 2:4) {              # looping the 3 outcome variables
  printchisq(mydata[, i], c(org_rel, emp_rel))
}


# Plots:
# Custom function "multiplot()" to generate multiple charts 
multiplot <- function(x, y) {
  col <- c("green", "yellow", "red")
  layout(matrix(c(1:6), nrow = 2, byrow = TRUE))
  for (i in y) {
    ht <- table(x, mydata[, i])
    par(mar = c(5, 3, 2, 1))
    barplot(ht,
            beside = TRUE,
            legend = FALSE,
            ylim = c(0, max(ht)),
            yaxt = "s",
            col = col,
            xlab = colnames(mydata[i]))
  }
  plot(1, type = "n", axes = FALSE, ylab = "", xlab = "")
  legend("top", inset = 0, legend = levels(x),
         horiz = FALSE, fill = col, col = col,
         title = paste("Concern about", attr(x, which = "name")))
  layout(matrix(1))
}

# Generating plots across the variable groupings and jointly displaying them
multiplot(stress, org_rel)
multiplot(stress, emp_rel)
multiplot(violence, org_rel)
multiplot(violence, emp_rel)
multiplot(bullying, org_rel)
multiplot(bullying, emp_rel)

# Housekeeping
rm(list = ls())
