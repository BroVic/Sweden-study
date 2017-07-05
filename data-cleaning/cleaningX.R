# cleaningX.R
# Extra data cleaning - focused on converting certain responses to true NAs

interim <- readRDS("data-cleaning/clean_2.rds")
dim(interim)

# How big is the problem of non-response?
problem <- apply(interim, MARGIN = 2, function(x)
  length(x[x == "Unknown" | x == "No answer"]))
problem   

viewProblem <- function(x) {
  plot(x,
     pch = "x",
     main = "Number of problematic data",
     col = "red", 
     ylab = "Freq")
}
viewProblem(problem)

# Now replace these responses with NAs
interim[interim == "Unknown" | interim == "No answer"] <- NA
newNAs <- apply(interim, MARGIN = 2, function(x) sum(is.na(x)))
newNAs
viewProblem(newNAs)

Amelia::missmap(interim, main = "Plot of the Missing Values")

sum(complete.cases(interim))
index_row <- which(complete.cases(interim)) 

final_clean <- interim[index_row, ]
any_missing <- apply(final_clean, MARGIN = 2, anyNA)
viewProblem(any_missing)

final_clean <- droplevels(final_clean)
level_check <- lapply(final_clean, levels)
level_check

dplyr::glimpse(final_clean)

saveRDS(final_clean, file = "clean_x.rds")

rm(list = ls())
unloadNamespace("Amelia");
unloadNamespace("dplyr")
unloadNamespace("Rcpp")
# END OF DATA CLEANING
