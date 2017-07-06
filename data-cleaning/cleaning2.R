# Data cleaning 2
# Dealing with missing values
library(dplyr)

draft <- readRDS("data-cleaning/clean_1.rds")
dim(draft)

# Adds up missing values in data frame by columns
show_missing <- function(x) {
  if (is.data.frame(x) == TRUE)
    apply(x, MARGIN = 2, function(y) sum(is.na(y)))
}

show_missing(draft) 

# subset the data frame to the current focus of the study
subdata <- draft[, c(1, 66:78, 230:243)]

# Incorporate actual survey questions as metadata
connexn <- file("dataset/selection-varlabels.txt")
open(connexn, "r")
for (i in 2:ncol(subdata)) {
  lab <- readLines(con = connexn, n = 1)
  attr(subdata[, i], which = "comment") <- lab
}
close(connexn)

(sum_missing <- show_missing(subdata))

# A plot of the missing values
plot(sum_missing, ylab = "No. of missing values", xlab = "Variable column no.",
     col = "red", pch = "+", main = "Map of Missing Values vs. Variables",
     axes = FALSE, ylim = c(0, 30000))
axis(1, las = 1); axis(2, las = 2)
grid(NA, ny = NULL, lwd = 2, lty = "dotted", col = "black")


saveRDS(subdata, "data-cleaning/clean_2.rds")

rm(list = ls())
#END
