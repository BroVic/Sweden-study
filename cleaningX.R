# cleaning3.R

# Additional adjustments to the data. This was necessary by discoveries made in the
# course of the analysis.
# A large number of variables have responses that could effectively be treated as
# NAs, and thus will be replaced and the impact reviewed side-by-side with the 
# previous approach to the analysis.

# load data
interim <- readRDS("clean_2.rds")
dim(interim)

# quantify the problem. Culprit responses are "Unknown" and "No answer"
problem <- apply(interim, MARGIN = 2, function(x)
  length(x[x == "Unknown" | x == "No answer"]))
problem
# Visualise
# First a function to ease plotting...
viewProblem <- function(x) {
  plot(x,
     pch = "x",
     main = "Number of problematic data",
     col = "red", 
     ylab = "Freq")
}
# Draw plot
viewProblem(problem)

# Replace with 'true' NAs and visualise them
interim[interim == "Unknown" | interim == "No answer"] <- NA
newNAs <- apply(interim, MARGIN = 2, function(x) sum(is.na(x)))
newNAs
viewProblem(newNAs)
if ("Amelia" %in% rownames(installed.packages()) == FALSE)
  install.packages("Amelia") # if package not already in library (requires internet connection)
Amelia::missmap(interim, main = "Plot of the Missing Values")

# How many observations are "truly" complete and who are they?
sum(complete.cases(interim))
index_row <- which(complete.cases(interim)) # index of rows

index_col <- apply(interim, MARGIN = 2, anyNA)


# subset the data to retain only non-NA records
final_clean <- interim[index_row, ]
any_missing <- apply(interim, MARGIN = 2, anyNA)
viewProblem(any_missing)

interim <- droplevels(interim)

dim(interim)
dplyr::glimpse(interim)

# Saving...
saveRDS(interim, file = "clean_x.rds")

# Cleaning up
rm(list = ls())
unloadNamespace("Amelia");
unloadNamespace("dplyr")
unloadNamespace("Rcpp")



# Caveat: These massive cuts in the dataset may contribute to undesirable bias
# in the distribution of missing values could not be properly characterized
# and the is a strong possibilty for clustering of the data points.

# THESE PARTICULAR DATA WILL BE USED IN THE EVENT WE DO A REGRESSION ANALYSIS

