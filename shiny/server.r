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
  
  ## Strategija 2
  #  vnosi
  reactive.radio.str2 <- eventReactive(input$goPlot.str2, {input$radio.str2})
  reactive.numS1 <- eventReactive(input$goPlot.str2,
                                  if(input$numS1> 60) {60} else{input$numS1})
  reactive.numS <- eventReactive(input$goPlot.str2,if(input$numS > 60){60} else {input$numS})
  reactive.select.t <- eventReactive(input$goPlot.str2, {input$select.t})

  # opozorila za S1 numeric input
  textnoteS1<-reactive({
    if(input$numS1<=60){
      paste("Število točk pred začetkom runde: ",input$numS1)
    }else{
      paste0("<font color=\"#e05959\"><b>", "Opozorilo: tvoje število točk je previsoko za
             izračun optimalne točke, izračun bo narejen za 60 točk. Uporabi strategijo 1 v primeru večjega števila točk.", "</b></font>")
    }
  })

  output$noteS1 <-renderText({
    HTML(textnoteS1())
  })

  # opozorila za S numeric input
  textnoteS<-reactive({
    if(input$numS<=60){
      paste("Trenutno število točk: ",input$numS)
    }else{
      paste0("<font color=\"#e05959\"><b>", "Opozorilo: tvoje število točk je previsoko za
             izračun optimalne točke, izračun bo narejen za 60 točk. Uporabi strategijo 1 v primeru večjega števila točk.", "</b></font>")
    }
    })

  output$noteS <-renderText({
    HTML(textnoteS())
  })

  # output
  stanje <- reactive(sprintf("%s-%s-%s", reactive.numS(),reactive.select.t(),reactive.numS1()))
  polje <- reactive(
    if (reactive.radio.str2() == "1") {stanje.in.polje.zacetnik[stanje.in.polje.zacetnik$stanje == stanje(), 4]}
    else if (reactive.radio.str2() == "2") {stanje.in.polje.rekreativec[stanje.in.polje.rekreativec$stanje == stanje(), 4]}
    else {stanje.in.polje.profesionalec[stanje.in.polje.profesionalec$stanje == stanje(), 4]})
  tocka <- reactive(
    if (reactive.radio.str2() == "1") {stanje.in.polje.zacetnik[stanje.in.polje.zacetnik$stanje == stanje(), 2:3]}
    else if (reactive.radio.str2() == "2") {stanje.in.polje.rekreativec[stanje.in.polje.rekreativec$stanje == stanje(), 2:3]}
    else {stanje.in.polje.profesionalec[stanje.in.polje.profesionalec$stanje == stanje(), 2:3]})
  plotOutput.str2 <- reactive({narisi.tocke(tocka())})

  # gumb - izracun optimalne točke
  output$optimalno.polje <- renderText(
    if (input$goPlot.str2) {
      paste("Optimalno polje: ", polje())
    })
  
  output$plot.str2 <- renderPlot({
    if (input$goPlot.str2) {
      plotOutput.str2()
      }
    else {drawBoard(new =TRUE)}})

}