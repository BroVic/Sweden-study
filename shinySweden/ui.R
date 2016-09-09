# ui.R

main_data <- readRDS("~/5-Personal/Consulting/Sweden-study/clean_x.rds")


shinyUI(fluidPage(
  titlePanel(title = "Explore"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "scope", label = "Analysis type", 
                  choices = c("Univariate", "Bivariate"),
                  selected = NULL),
      
      hr(),
      
      conditionalPanel(
        condition = "input.scope == 'Univariate'",
        selectInput(inputId = "var", label = "Variable",
                    choices = colnames(main_data), selected = NULL)
      ),
      
      
      conditionalPanel(
        condition = "input.scope == 'Bivariate'",
        selectInput(inputId = "predictor", label = "Independent variable",
                    choices = colnames(main_data[, -c(1:3, 9:11)]),
                    selected = colnames(main_data[1])),
        selectInput(inputId = "outcome", label = "Dependent variable",
                    choices = names(main_data[, c(1:3, 9:11)]),
                    selected = colnames(main_data[4]))
      )
    ),
    
    mainPanel(
      plotOutput("chart")
    )
  )
))