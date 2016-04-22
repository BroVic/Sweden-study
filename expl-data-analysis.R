# Exploratory Data Analysis
library(dplyr)
library(tidyr)

# Load the data
eurodat <- readRDS("clean_2.rds")

# generate overview of data table
tbl_df(data = eurodat)

# subset categorical variables
categoricals <-  eurodat %>%
  select(-id, -femaleEmployees, -over50)

# print out tables of all categorical variables in console
sapply(categoricals, table)


# print out bar charts of all the categorical variables in the dataset
for (i in 1:96) {
  tab <- table(categoricals[, i])
  ncol <- colnames(categoricals)
  barplot(tab, main = paste("Barplot of ", ncol[i]))
}

