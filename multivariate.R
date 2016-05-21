# Analysis2.R
# Advanced Analyses


mydata<- readRDS("clean_x.rds")

col <- c("green", "yellow", "red")
# Run sample code for visualization and analyses
spineplot(concern.stress ~ risks.time, data = mydata, col = col)
vcd::mosaic(concern.stress ~ 
              risks.time+risks.communication+risks.unclearPolicy,
            data = mydata, shade = TRUE,
            highlighting_fill = col)
x <- table(mydata$concern.stress, mydata$risks.time)
x
vcd::assoc(x)

# Partition the dataset
index <- createDataPartition(mydata$concern.stress, times = 1, p = .8, list = FALSE)
train <- mydata[index, ]
test <- mydata[-index, ]
rm(index)

glm.fitted <- glm(concern.stress ~ risks.time+risks.communication, data = mydata, family = "binomial")
summary(glm.fitted)

lda.fitted <- MASS::lda(risks.time ~ femaleEmployees+over50, data = mydata, 
                        subset = train, na.omit) # this is just a test run.



