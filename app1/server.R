shinyServer(function(input, output) {
  
  react <- reactiveValues(dat = NULL)
  observe({react$dat <- loadData(input$File2022, input$File2023)})
  output$trackerPlot <- renderPlot({generatePlot2(react$dat, as.numeric(input$week), input$proctor, input$grade, constants)})
})




