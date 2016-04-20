# Data cleaning 2

library(dplyr)
library(Amelia)

draft <- readRDS("clean_1.rds")

# Assess missing values
# Build a function to test for missing values at different stages of cleaning
show_miss <- function(x) {
  if (is.data.frame(x) == TRUE)
    apply(x, MARGIN = 2, function(y) sum(is.na(y)))
  }

# reveal number of missing values in each variable
sum_miss <- show_miss(draft)

# A plot of the missing values
# Plot No. 1
plot(sum_miss, ylab = "No. of missing values", xlab = "Variable column no.",
     col = "red", pch = "+", main = "Map of Missing Values vs. Variables",
     axes = FALSE, ylim = c(0, 30000))
axis(1, las = 1); axis(2, las = 2)
grid(NA, ny = NULL, lwd = 2, lty = "dotted", col = "black")

# Plot No. 2
missmap(draft, main = "Map of Missing Values in the dataset", legend = FALSE,
        y.labels = NULL, y.at = NULL)
legend("topleft", legend = c("Missed", "Observed"), bty = "n", 
       title = "Legend", fill = c("wheat", "darkred"), border = "black")

# remove all zero-response variables
all_zero <- which(sum_miss == 28649)
mydata <- draft[, -all_zero]

# plot missing values again
sum_miss2 <- show_miss(mydata)
plot(sum_miss2, ylab = "No. of missing values", xlab = "Variable column no.",
     col = "darkgreen", pch = "+",
     main = "Plot #2 of Missing Values vs. Variables", axes = FALSE,
     ylim = c(0, 30000), sub = "after removal of all zero-response variables")
axis(1, las = 1); axis(2, las = 2)
grid(NA, ny = NULL, lwd = 2, lty = "dotted", col = "black")

# Examine structure of the data after setting arbitrary cut-offs
# What variable have less than 5,000 missing values?
less_5k <- which(sum_miss2 <= 5000)
less_data <- mydata[, less_5k]

show_miss(less_data)
missmap(less_data)

compl <- which(complete.cases(less_data))
compl.data <- less_data[compl, ]

# weigh proportions
all <- dim(less_data)
most <- sum(complete.cases(less_data))
most/all[1]

show_miss(compl.data) # no more NAs
glimpse(compl.data) # inspect overview

saveRDS(compl.data, "clean_2.rds")
