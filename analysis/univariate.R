# Exploratory Data Analysis
library(dplyr)
library(tibble)

# Load the data
eurodat <- readRDS("data-cleaning/clean_2.rds")

# generate overview of data table
glimpse(eurodat)
summary(eurodat)


# subset categorical variables
categoricals <-  eurodat %>%
  select(-est_wei1, -est_wei2, -emp_wei1, -emp_wei2)

numericals <- eurodat %>%
  select(est_wei1, est_wei2, emp_wei1, emp_wei2)

# print out univariate tables of all categorical variables in console
sapply(categoricals, table)


# print out bar charts of all the categorical variables in the dataset
for (i in 1:length(tablesList)) {
  tab <- table(categoricals[, i])
  ncol <- colnames(categoricals)
  barplot(tab, main = paste("Barplot of ", ncol[i]))
}

summary(numericals)
# generic plots for numerical data
apply(numericals, 2, function(x) plot(x))
