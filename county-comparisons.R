# country-comparisons
library(ggplot2)

alldata <- readRDS("clean_x.rds")

levels(alldata$country)
alldata$country <- factor(alldata$country,
                          levels = names(sort(table(alldata$country),
                                              decreasing = TRUE)))
levels(alldata$country)

country_table <- table(alldata$country, alldata$concern.stress)
country_table
country_proptab <- round(prop.table(country_table, margin = 1), digits = 2)
country_proptab
country_margtabRow <- margin.table(country_table, margin = 1)
country_margtabRow
country_margtabCol <- margin.table(country_table, margin = 2)
country_margtabCol 

countryPlot <- ggplot(alldata, aes(x = country, fill = concern.stress)) +
  geom_bar() +
  ggtitle("Distribution of countries")
countryPlot

vcdExtra::CMHtest(country_table) # Cochrane-Mantel-Haenzl test
coin::cmh_test(country_table)   # same but focusing on general associaion
vcdExtra::GKgamma(country_table) # Goodman-Kruskal statistic

