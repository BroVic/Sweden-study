# country-comparisons
library(ggplot2)

alldata <- readRDS("clean_x.rds")

levels(alldata$country)
alldata$country <- factor(alldata$country, levels = names(sort(table(alldata$country), decreasing = TRUE)))
levels(alldata$country)

countryPlot <- ggplot(alldata, aes(x = country, fill = concern.stress)) +
  geom_bar() +
  ggtitle("Distribution of countries")
countryPlot
