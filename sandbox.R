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

# Fresh plotting approach
# Plotting function
multiplot <- function(x, y) {
  layout(matrix(c(1:3, 4:6, 7, 7, 7), nrow = 3, byrow = TRUE))
  par(mar = c(0, 3, 5, 1))
  col <- c("green", "yellow", "red")
  for (i in y) {
    ht <- table(x, mydata[, i])
    barplot(ht,
            beside = TRUE,
            legend = FALSE,
            ylim = c(0, max(ht)),
            yaxt = "s",
            col = c("green", "yellow", "red"),
            xlab = colnames(mydata[i]))
  }
  plot(1, type = "n", axes = FALSE, ylab = "")
  legend("top", inset = 0, legend = levels(x),
         horiz = FALSE, fill = col, col = col,
         title = attr(x, which = "name"), bty = "n")
  layout(matrix(1))
  oldpar <- par()
}
