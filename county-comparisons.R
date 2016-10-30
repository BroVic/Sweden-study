# country-comparisons
library(ggplot2)
library(vcdExtra)

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
country_margtabRow <- margin.table(country_table, 1)
country_margtabRow
country_margtabCol <- margin.table(country_table, 2)
country_margtabCol 


countryPlot_stress <- ggplot(alldata, aes(x = country, fill = concern.stress)) +
  geom_bar() +
  ggtitle("Distribution of countries vs. stress")
countryPlot_stress

countryPlot_bullying <- 
  ggplot(alldata, aes(x = country, fill = concern.bullying)) +
  geom_bar() +
  ggtitle("Distribution of countries vs. bullying")
countryPlot_bullying

countryPlot_violence <-
  ggplot(alldata, aes(x = country, fill = concern.violence)) +
  geom_bar() +
  ggtitle("Distribution of countries vs. violence")
countryPlot_violence

CMHtest(country_table) # Cochrane-Mantel-Haenzl test
coin::cmh_test(country_table)   # same but focusing on general associaion
GKgamma(country_table) # Goodman-Kruskal statistic

# To do:
# 1. Analyse Skandinavian countries - Denmark, Norway, Sweden (Faroe Islands,
#    Finland, Iceland)

## create a new variables for Skandinavia
alldata$isSkandinavian <- grepl("FI|DK|NO|SE", alldata$country)
(tab1 <-table(alldata$concern.stress, alldata$isSkandinavian))
round(prop.table(tab1, 2), 2)
barplot(prop.table(tab1, 2))


alldata$skand_OTHERS <- gsub("FI|DK|NO|SE", "SKND", alldata$country)
theCountries <- levels(as.factor(alldata$skand_OTHERS))
theCountries <- theCountries[-grep("SKND", theCountries)]
theCountries <- c(theCountries, "SKND")
alldata$skand_OTHERS <- factor(alldata$skand_OTHERS, levels = theCountries)
str(alldata$skand_OTHERS)
levels(alldata$skand_OTHERS)

tab2 <- table(alldata$concern.bullying, alldata$skand_OTHERS)
round(prop.table(tab2, 2), 2)
barplot(prop.table(tab2, 2))   # nothing unusual seen

# check out Skandinavia data
Skandinavia_data <- alldata[alldata$isSkandinavian == T, ]
Skandinavia_data <- droplevels(Skandinavia_data)
dim(Skandinavia_data)

summary(Skandinavia_data)
(tab_stress <- table(Skandinavia_data$concern.stress, Skandinavia_data$country))
barplot(tab_stress)
(tab_bullying <- table(Skandinavia_data$concern.bullying,
                      Skandinavia_data$country))
barplot(tab_bullying)
(tab_violence <- table(Skandinavia_data$concern.stress,
                       Skandinavia_data$country))
barplot(tab_violence)
