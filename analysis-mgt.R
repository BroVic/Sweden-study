# Analysis of Management Staff Perceptions
mydata <- readRDS("clean_x.rds")

summary(mydata[, 1:3])
stress <- mydata[, 1]; attr(stress, which = "name") <- "STRESS"
violence <- mydata[, 2]; attr(violence, which = "name") <- "VIOLENCE"
bullying <- mydata[, 3]; attr(bullying, which = "name") <- "BULLYING"

covars_index <- c(4:13)

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
org_factors <- c(4, 5, 7, 11, 12)
emp_factors <- c(6, 8:10, 13)

# Contingengy tables
for (i in 1:3) {
  for (j in covars_index)
    print(table(mydata[, i], mydata[, j],
                dnn = c(colnames(mydata[i]), colnames(mydata[j]))))
}

# Plots: Function to generate multiple charts 
multiPlot <- function(x, y) {
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

multiPlot(stress, org_factors)
multiPlot(stress, emp_factors)
multiPlot(violence, org_factors)
multiPlot(violence, emp_factors)
multiPlot(bullying, org_factors)
multiPlot(bullying, emp_factors)

# Test of independence
# Function to print out results
printchisq <- function(x, vec) {
  for (i in vec) {
    result <- chisq.test(x, mydata[, i])
    print(result)
  }
}

for (i in 1:3) {
  chisq_list[i] <- printchisq(mydata[, i], c(org_factors, emp_factors))
}

# Housekeeping
rm(list = ls())
