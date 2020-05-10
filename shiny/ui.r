dashboardPage(
  skin = "red",
  ################################## HEADER ################################### 
  dashboardHeader(title = tags$div(tags$img(src='logo.png', height="30%", width="30%", align="left"),"Pikado"),
                  
                  tags$li(a(href = 'https://github.com/tinarazic/pikado',
                            icon("github", "fa-2x"),
                            title = "GitHub"),
                          class = "dropdown"),
                  tags$li(class = "dropdown",
                  tags$style(".main-header {max-height: 60px}"),
                  tags$style(".main-header .logo {height: 60px;}"),
                  tags$style(".sidebar-toggle {height: 60px; padding-top: 1px !important;}"),
                  tags$style(".navbar {min-height: 60px !important}"))),
  
  ################################## SIDEBAR ##################################
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
    sidebarMenuOutput("menu"),
    sidebarMenu(
      
      menuItem("Domov", tabName = "domov", icon = icon("home")),
      menuItem("Pravila igre 301", tabName = "pravila", icon = icon("list-alt")),
      menuItem("Matematično ozadje", tabName = "ozadje", icon = icon("book"), startExpanded = FALSE,
                 menuSubItem("Predpostavke", tabName = 'predpostavke', icon=icon("book-open")),
                 menuSubItem("Optimalna strategija 1", tabName = "ozadje_strategija1", icon=icon("book-open")),
                 menuSubItem("Optimalna strategija 2", tabName = "ozadje_strategija2", icon=icon("book-open"))),
      menuItem("Strategija 1", tabName = "strategija1", icon = icon("trophy")),
      menuItem("Strategija 2", tabName = "strategija2", icon = icon("trophy"))
    )),
  
  ################################## BODY ####################################
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "domov",
              img(src='2.png', height="60%", width="60%", align="center")),

      tabItem(tabName = "pravila",
              h1("Igra"),
              includeMarkdown(knit("www/shiny_pravila_igre.Rmd", output = "www/shiny_pravila_igre.md"))),
      
      tabItem(tabName = "predpostavke",
              h1("Matematično ozadje"), 
              withMathJax(includeMarkdown(knit("www/shiny_matematicno_ozadje.Rmd", output = "www/shiny_matematicno_ozadje.md")))),
    
      tabItem(tabName = "ozadje_strategija1",
              h1("Optimalna strategija 1"),
              withMathJax(includeMarkdown(knit("www/shiny_strategija1.Rmd", output = "www/shiny_strategija1.md")))),
      
      tabItem(tabName = "ozadje_strategija2",
              h1("Optimalna strategija 2"),
              withMathJax(includeMarkdown(knit("www/shiny_strategija2.Rmd", output = "www/shiny_strategija2.md")))),
      
      tabItem(tabName = "strategija1",
              h1("Kam naj ciljam, da zadanem največ točk?"),
              navbarPage("Barvni graf",
                         tabPanel("Nivo igralca",
                                  sidebarLayout(
                                    sidebarPanel(
                                      radioButtons("radio", label = h3("Nivo igralca"),
                                                   choices = list("Začetnik" = 1, "Rekreativec" = 2, "Profesionalec" = 3), 
                                                   selected = 1),
                                      verbatimTextOutput("value")),
                                    mainPanel(plotOutput('plot1')))),
                         tabPanel("Personaliziran barvni graf",
                                  sidebarLayout(
                                    sidebarPanel(
                                      sliderInput("slider", label = h3("Izberi odklon puščice"), min = 0, 
                                                  max = 170, value = 5),
                                      numericInput("num", label = h3("Vpiši število iteracij v vsaki točki:"), value = 100),
                                      h6("Število točk je 1961, zato pričakuj časovno zahteven algoritem, če izbereš veliko iteracij."),
                                      actionButton('goPlot', 'Pokaži graf!')),
                                    mainPanel(plotOutput("plot2"),
                                              hr(),
                                              downloadButton("download", "Shrani barvni graf.")))))),
  
      
      tabItem(tabName = "strategija2",
              h1("Kam naj ciljam, da čimprej končam igro?"))
      
    )
    
  )
)