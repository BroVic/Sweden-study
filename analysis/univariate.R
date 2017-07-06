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
glimpse(categoricals)

numericals <- eurodat %>%
  select(id, est_wei1, est_wei2, emp_wei1, emp_wei2)
glimpse(numericals)

# print out univariate tables of all categorical variables in console
tablesList <- sapply(categoricals, table)


# print out bar charts of all the categorical variables in the dataset
for (i in 2:length(tablesList)) {
  tab <- table(categoricals[, i])
  name_col <- colnames(categoricals)
  barplot(tab,
          main = paste("Barplot of", name_col[i]),
          sub = paste("Variable label:", comment(categoricals[, i])))
}

summary(numericals)

# generate various kinds of plots for numerical data
apply(numericals, 2, function(x) plot(x))

allCols <- 2:ncol(numericals)
for (i in allCols)
  boxplot(numericals[, i])

for (i in allCols) {
  hist(numericals[, i],
       main = paste("Histogram of", colnames(numericals)[i]),
       sub = paste("Variable label", comment(numericals[, i]))
  )
}
