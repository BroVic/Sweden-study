# Analysis of Management Staff Perceptions
library(dplyr)
mydata <- readRDS("clean_2.rds")

## Outcome variables
# Stress - pos 66
# Violence/threats - pos 67
# Bullying/harrassment - pos 68

## i. Organisational Factors 
# •	Time pressure (Pos  69 - MM202.1)
# •	Poor communication between management and employees (Pos 70 - MM202.2)
# •	Long or irregular working hours (Pos 76 - MM202.8)
# •	An unclear human resources policy (Pos 77 - MM202.9)
# •	Lack of employee control in organising their work(Pos 72 - MM202.4)

## ii. Employee related i.e. Factors at individual & group (interpersonal relationship among colleagues) level 
# •	Job insecurity(Pos 73 - MM202.5)
# •	Having to deal with difficult customers, patients, pupils etc. (Pos 74 - MM202.6)
# •	Problems in supervisor - employee relationships(Pos 75 - MM202.7)
# •	Discrimination (for example due to gender, age or ethnicity) (Pos 78 - MM202.10)
# •	Poor co-operation amongst colleagues(Pos 71 - MM202.3)

summary(mydata[, 66:68])
stress <- mydata[, 66]
violence <- mydata[, 67]
bully <- mydata[, 68]

barplot(table(stress))
barplot(table(violence))
barplot(table(bully))

check <- function(x, pos ) {
  x <- table(stress, mydata[, pos]) 
  barplot(stress_time, beside = TRUE, legend = TRUE)
}

for (i in 69:78) {
  check(stress, i)
  check(violence, i)
  check(bully, i)
}