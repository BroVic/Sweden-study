# country-comparisons
library(ggplot2)
library(vcdExtra)
library(coin)

alldata <- readRDS("data-cleaning/clean_x.rds")

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


countryPlot_stress <- ggplot(alldata, aes(country, fill = concern.stress)) +
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
cmh_test(country_table)   # same but focusing on general associaion
GKgamma(country_table) # Goodman-Kruskal statistic


## create a new colunmn for Skandinavia
alldata$isSkandinavian <- grepl("FI|DK|NO|SE", alldata$country)
alldata$isSkandinavian <- factor(alldata$isSkandinavian,
                                 levels = c(TRUE, FALSE),
                                 labels = c("Skandinavia", "Other EU"))
(tab1 <-table(alldata$isSkandinavian, alldata$concern.stress))
(pcent_tab <- round(prop.table(tab1, 1), 2))

# Function for rendering a bar chart for the different vaiables
skand_bar <- function(table, persp = c(1, 2), pos = "topleft",
                      vname = character(), added = NULL) {
  if (length(persp > 1))
    persp <- persp[1]
 barplot(prop.table(table, persp),
         beside = T,
         main = paste("Concern over", sQuote(vname), added),
         ylim = c(0.0, 0.6),
         legend = T,
         args.legend = list(x = pos))
}

tab <- table(alldata$isSkandinavian, alldata$concern.stress)
skand_bar(tab, vname = "stress")
tab <- table(alldata$isSkandinavian, alldata$concern.bullying)
skand_bar(tab, pos = "topright", vname = "bullying")
tab <- table(alldata$isSkandinavian, alldata$concern.violence)
skand_bar(tab, pos = "topright", vname = "violence")

# Focus on Skandinavia data
Skandinavia_data <- alldata[alldata$isSkandinavian == "Skandinavia", ]
Skandinavia_data <- droplevels(Skandinavia_data)
dim(Skandinavia_data)
summary(Skandinavia_data)

tab_stress <- table(Skandinavia_data$concern.stress, Skandinavia_data$country)
skand_bar(tab_stress, vname = "stress")
chisq.test(tab_stress)
CMHtest(tab_stress)
cmh_test(tab_stress)
GKgamma(tab_stress)

(tab_bullying <- table(Skandinavia_data$concern.bullying,
                      Skandinavia_data$country))
skand_bar(tab_bullying, vname = "bullying")
chisq.test(tab_bullying)
CMHtest(tab_bullying)
cmh_test(tab_bullying)
GKgamma(tab_bullying)

(tab_violence <- table(Skandinavia_data$concern.stress,
                       Skandinavia_data$country))
skand_bar(tab_violence, vname = "violence")
chisq.test(tab_violence)
CMHtest(tab_violence)
cmh_test(tab_violence)
GKgamma(tab_violence)
