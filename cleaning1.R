# Data cleaning for esener2009_20090924.tab
library(dplyr)

rawdata <- read.delim("dataset/esener2009_20090924.tab")
source("helpers.R") # contains user-defined functions & other useful objects   
# Note: These functions will only work with this dataset 

tbl_df(rawdata)

length(var.names)
colnames(rawdata) <- var.names
head(colnames(rawdata)); tail(colnames(rawdata))

# Change variable types as appropriate
# iteration paradigm to be applied to columns with similar responses
# Note that for some loops, user-defined functions are called

rawdata[, 1] <- seq(1, 28649, by = 1)

# Columns with the most repeated response
col1 <- c(4, 6:14, 16:20, 23, 25:40, 53:61, 69:78, 82:87, 91:96, 101:106,
          110:112, 137, 140:156, 158:176, 178:183, 185:189, 201:216, 223:226,
          229)
for (i in col1)
  rawdata[, i] <- yesno1(i)

for (i in c(89:90, 98:99, 107:109, 113:114, 117:119))
  rawdata[, i] <- yesno2(i)

for (i in 41:52)
  rawdata[, i] <- majorminor(i)

for (i in c(62:68, 194:200))
  rawdata[, i] <- concerns(i)

for (i in c(127:128, 237:238))
  rawdata[, i] <- props(i)

for (i in c(129, 131:134))
  rawdata[, i] <- interview(i)

for (i in c(130, 157)) {
  recode <- c("Training is sufficient" = 1,
              "More training would be desirable" = 2, "Unknown" = 3) 
  rawdata[, i] <- factor(rawdata[, i], levels = recode, labels = names(recode))
}

for (i in c(191:193)) {
  recode <- c("Agree" = 1, "Neither agree nor disagree" = 2, "Disagree" = 3,
              "Unknown" = 4) 
  rawdata[, i] <- factor(rawdata[, i], levels = recode, labels = names(recode))
}

for (i in c(217:222)) {
  recode <- c("Very effective" = 1, "Quite effective" = 2,
              "Quite ineffective" = 3, "Very ineffective" = 4, "Unknown" = 5) 
  rawdata[, i] <- factor(rawdata[, i], levels = recode, labels = names(recode))
}

for (i in 235:236) {
  recode <- c("Yes" = 1, "No" = 2)
  rawdata[, i] <- factor(rawdata[, i], levels = recode, labels = names(recode))
}

recode <- c("single" = 1, "multiple" = 2, "unknown" = 3)
rawdata[, 2] <- factor(rawdata[, 2], levels = recode,
                       labels = names(recode))
recode <- c("Headquarters" = 1, "Subsidiary" = 2, "Unknown" = 3)
rawdata[, 3] <- factor(rawdata[, 3],
                       labels = names(recode))

recode <- c("< 1990" = 1, "1990-2006" = 2, "> 2006" = 3, "unknown" = 4)
rawdata[, 5] <- factor(rawdata[, 5], levels = recode, ordered = TRUE,
                       labels = names(recode))

recode <- c("Practically no impact" = 3, "Some Impact" = 2, "Large Impact" = 1, 
             "Unknown" = 4)
rawdata[, 15] <- factor(rawdata$policy.impact, levels = recode, ordered = TRUE,
                        labels = names(recode))

recode <- c("Practically never", "Occasionally" = 2, "Regularly" = 1,
            "Unknown" = 4)
rawdata[, 21] <- factor(rawdata[, 21], levels = recode, ordered = TRUE,
                        labels = names(recode))

recode <- c("Very low" = 4, "Quite low" = 3, "Quite high" = 2, "Very high" = 1,
             "Unknown" = 5)
rawdata[, 22] <- factor(rawdata[, 22], levels = recode, ordered = TRUE,
                        labels = names(recode))

recode <- c("Conducted by own staff" = 1,
            "Contracted to external providers" = 2, "Both about equally" = 3,
            "Unknown" = 4)
rawdata[, 24] <- factor(rawdata[, 24], levels = recode, labels = names(recode))

recode <- c("Yes" = 1, "No" = 2, "Work-related stress not an issue" = 3,
            "No answer" = 4)
rawdata[, 79] <- factor(rawdata[, 79], levels = recode, labels = names(recode))

recode <- c("Yes" = 1, "No" = 2, "These problems are not an issue" = 3,
            "No answer" = 4)
rawdata[, 80] <- factor(rawdata[, 80], levels = recode, labels = names(recode))

recode <- c("Yes" = 1, "No" = 2, "Work-related violence not an issue" = 3,
            "No answer" = 4)
rawdata[, 81] <- factor(rawdata[, 81], levels = recode, labels = names(recode))

recode <- c("Yes" = 1, "No" = 2,
            "Long or irregular working hours not an issue" = 3,
            "No answer" = 4)
rawdata[, 88] <- factor(rawdata[, 88], levels = recode, labels = names(recode))

recode <- c("Very ineffective" = 4, "Quite ineffective" = 3,
            "Quite effective" = 2, "Very effective" = 1, "No answer" = 5) 
rawdata[, 97] <- factor(rawdata[, 97], levels = recode, ordered = TRUE,
                        labels = names(recode))

recode <- c("No difference" = 3, "Less difficult" = 2, "More difficult" = 1, 
             "No answer" = 4) 
rawdata[, 100] <- factor(rawdata[, 100], ordered = TRUE, levels = recode,
                         labels = names(recode))

recode <- c("Totally unimportant" = 4, "Quite unimportant" = 3,
            "Quite important" = 2, "Very important" = 1, "No answer" = 5)
rawdata[, 115] <- factor(rawdata[, 115], levels = recode, ordered = TRUE,
                         labels = names(recode))

recode <- c("Practically never" = 3, "Sometimes" = 2, "Often" = 1,  
            "No answer" = 4)
rawdata[, 116] <- factor(rawdata[, 116], levels = recode, ordered = TRUE,
                         labels = names(recode))

recode <- c("None at all" = 1, "< 20%" = 2, "20% - 40%" = 3,
            "40% - 60%" = 4, "60% - 80%" = 5, "80% - 100%" = 6,
            "All" = 7, "Unknown" = 8) 
rawdata[, 121] <- factor(rawdata[, 121], levels = recode, ordered = TRUE,
                         labels = names(recode))

recode <- c("None at all" = 1, "< 20%" = 2, "20% - 40%" = 3,
            "40% - 60%" = 4, "60% - 80%" = 5, "80% - 100%" = 6, "All" = 7,
            "Unknown" = 8) 
rawdata[, 123] <- factor(rawdata[, 123], levels = recode, ordered = TRUE,
                         labels = names(recode))

recode <- c("Very low" = 5, "Quite low" = 4, "About average" = 3,
            "Quite high" = 2, "Very high" = 1, "No answer" = 6) 
rawdata[, 124] <- factor(rawdata[, 124], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Very bad" = 5, "Quite bad" = 4, "Neither good nor bad" = 3,
            "Quite good" = 2, "Very good" = 1, "No answer" = 6) 
rawdata[, 125] <- factor(rawdata[, 125], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Yes" = 1, "No" = 2, "Refused" = 3) 
rawdata[, 130] <- factor(rawdata[, 130], levels = recode,
                         labels = names(recode))

recode <- c("Respondent agrees" = 1, "Respondent maintains refusal" = 2,
            "Agrees to interview but can't give address" = 3,
            "Respondent is this person" = 4)
rawdata[, 135] <- factor(rawdata[, 135], levels = recode,
                         labels = names(recode))

recode <- c("Yes, agrees" = 1, "No, does not agree" = 2, "Unknown" = 3) 
rawdata[, 136] <- factor(rawdata[, 136], levels = recode,
                         labels = names(recode))

recode <- c("Less than once a year" = 5, "Once a year" = 4,
            "Several times a year" = 3, "Once a month" = 2,
            "Several times a month" = 1, "Unknown" = 6) 
rawdata[, 138] <- factor(rawdata[, 138], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Practically never" = 3, "Sometimes" = 2, "Often" = 1,  
            "Unknown" = 4) 
rawdata[, 139] <- factor(rawdata[, 139], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Practically no impact" = 3, "Some impact" = 2,
            "Large Impact" = 1, "Unknown" = 4) 
rawdata[, 177] <- factor(rawdata[, 177], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("No" = 2, "Only partly" = 3, "Yes" = 1, "Unknown" = 4) 
rawdata[, 184] <- factor(rawdata[, 184], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Very low" = 4, "Quite low" = 3, "Quite high" = 2,
            "Very high" = 1, "Unknown" = 5) 
rawdata[, 190] <- factor(rawdata[, 190], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Less difficult" = 2, "No difference" = 3, "More difficult" = 1,
            "Unknown" = 4) 
rawdata[, 227] <- factor(rawdata[, 227], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Very unwilling" = 4, "Quite unwilling" = 3, "Quite willing" = 2,
            "Very willing" = 1, "Unknown" = 5) 
rawdata[, 228] <- factor(rawdata[, 228], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("10 to 19" = 1, "20 to 49" = 2, "50 to 249" = 3, "250 to 499" = 4,
            "500+" = 5) 
rawdata[, 230] <- factor(rawdata[, 230], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("Producing industries" = 1, "Private services" = 2,
            "Public services" = 3)
rawdata[, 231] <- factor(rawdata[, 231], levels = recode,
                         labels = names(recode))

recode <- c("NACE C" = 1, "NACE D" = 2, "NACE E" = 3, "NACE F" = 4,
            "NACE G" = 5, "NACE H" = 6, "NACE I" = 7, "NACE J" = 8,
            "NACE K" = 9, "NACE L" = 10, "NACE M" = 11, "NACE N" = 12,
            "NACE O" = 13) 
rawdata[, 232] <- factor(rawdata[, 232], levels = recode,
                         labels = names(recode))

recode <- c("BE" = 1, "DK" = 2, "DE" = 3, "EL" = 4, "ES" = 5, "FI" = 6,
            "FR" = 7, "IE" = 8, "IT" = 9, "LU" = 10, "NL" = 11, "AT" = 12,
            "PT" = 13, "SE" = 14, "UK" = 15, "BG" = 31, "CY" = 32, "CZ" = 33,
            "EE" = 34, "HU" = 35, "LV" = 36, "LT" = 37, "MT" = 38, "PL" = 39,
            "RO" = 40, "SK" = 41, "SI" = 42, "TR" = 43, "HR" = 46, "CH" = 51,
            "NO" = 52, "MK" = 63) 
rawdata[, 233] <- factor(rawdata[, 233], levels = recode,
                         labels = names(recode))

recode <- c("10 to 19" = 1, "20 to 49" = 2, "50 to 99" = 3, "100 to 149" = 4,
            "150 to 199" = 5, "200 to 249" = 6, "250 to 299" = 7,
            "300 to 399" = 8, "400 to 499" = 9, "500+" = 10) 
rawdata[, 234] <- factor(rawdata[, 234], levels = recode,
                         labels = names(recode), ordered = TRUE)

recode <- c("BE" = 1, "BG" = 2, "CZ" = 3, "DK" = 4, "DE" = 5, "EE" = 6,
            "IE" = 7, "EL" = 8, "ES" = 9, "FR" = 10, "IT" = 11, "CY" = 12,
            "LV" = 13, "LT" = 14, "LU" = 15, "HU" = 16, "MT" = 17, "NL" = 18,
            "AT" = 19, "PL" = 20, "PT" = 21, "RO" = 22, "SI" = 23, "SK" = 24,
            "FI" = 25, "SE" = 26, "UK" = 27, "HR" = 28, "TR" = 29, "CH" = 30,
            "NO" = 31) 
rawdata[, 243] <- factor(rawdata[, 243], levels = recode,
                         labels = names(recode))

# Examine overview of dataset noting the variable types
glimpse(rawdata)

# --------------------- #

# Save data as an R object after 1st round of cleaning
saveRDS(rawdata, "clean_1.rds")

# Remove all evidence :)
rm(list = ls())
detach(package:dplyr, unload = TRUE)

