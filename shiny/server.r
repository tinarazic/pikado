function(input, output) {

  output$plot1 <- renderPlot(if (input$radio == "1") {zacetnik}
                             else if (input$radio == "2") {rekreativec}
                             else {profesionalec})
  
  output$tabla <- renderImage({
    filename <- normalizePath(file.path('../slike/shiny_tabla3.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename)}, deleteFile = FALSE)
  
}