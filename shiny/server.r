function(input, output) {
  
  ## tab Strategija 1/Nivo igralca
  # graf
  output$value <- renderPrint(if (input$radio == "1") {odklon.zacetnik}
                              else if (input$radio == "2") {odklon.rekreativec}
                              else {odklon.profesionalec})
  
  output$plot1 <- renderPlot(if (input$radio == "1") {zacetnik}
                             else if (input$radio == "2") {rekreativec}
                             else {profesionalec})
  
  ## tab Strategija 1/Personaliziran barvni graf
  # vnosi
  reactive.slider <- eventReactive(input$goPlot, {input$slider})
  reactive.num <- eventReactive(input$goPlot, {input$num})
  plotInput <- reactive({barvna.tabla(MonteCarlo(tocke, reactive.slider(),reactive.slider(), k = reactive.num()))})
  
  # gumb - izris grafa
  output$plot2 <- renderPlot({
    if (input$goPlot) {
     # Re-run when button is clicked
    withProgress(message = "Rišem graf", value = 0,
                   {plotInput()})}
    else {profesionalec}})
  
  # gumb - shrani
  output$download <- downloadHandler(
    filename = function() {
      paste("Odklon", input$slider, "_iteracij", input$num, ".png", sep ="")
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
  

}