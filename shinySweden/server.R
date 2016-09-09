# server.R

library(shiny)

main_data <- readRDS("~/5-Personal/Consulting/Sweden-study/clean_x.rds")

shinyServer(function(input, output) {
  mydata <- reactive({
  if (input$scope == "Univariate")
      tab <- table(main_data[, as.character(input$var)])
  else if (input$scope == "Bivariate")
      tab <- table(main_data[, as.character(input$predictor)],
                   main_data[, as.character(input$outcome)])
  })
  
  output$chart <- renderPlot({
    data_body <- mydata()
    if (input$scope == "Bivariate")
      barplot(data_body, beside = TRUE, legend = TRUE, xlab = names(input$outcome),
            ylab = "Frequency")
    else if (input$scope == "Univariate")
      barplot(data_body, xlab = names(input$var), ylab = "Frequency",
              main = paste("Distribution of", input$var))
    
  })
}
)
