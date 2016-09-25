# Data cleaning 2
# Dealing with missing values
library(dplyr)

draft <- readRDS("clean_1.rds")

show_missing <- function(x) {
  if (is.data.frame(x) == TRUE)
    apply(x, MARGIN = 2, function(y) sum(is.na(y)))
}

show_missing(draft)

# subset data frame to focus of study
subdata <- draft[, c(66:73, 230:243)]

(sum_missing <- show_missing(subdata))

# A plot of the missing values
plot(sum_missing, ylab = "No. of missing values", xlab = "Variable column no.",
     col = "red", pch = "+", main = "Map of Missing Values vs. Variables",
     axes = FALSE, ylim = c(0, 30000))
axis(1, las = 1); axis(2, las = 2)
grid(NA, ny = NULL, lwd = 2, lty = "dotted", col = "black")


saveRDS(subdata, "clean_2.rds")

detach(package:dplyr, unload = TRUE)
