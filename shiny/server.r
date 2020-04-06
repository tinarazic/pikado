function(input, output) {

  output$plot1 <- renderPlot(if (input$radio == "1") {zacetnik}
                             else if (input$radio == "2") {rekreativec}
                             else {profesionalec})

  
}