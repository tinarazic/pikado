title <- tags$a(tags$img(src="../slike/shiny_tabla.png", height='30', width='30'), 'Pikado', style = "color: white;")

dashboardPage(
  dashboardHeader(title=title),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Domov", tabName = "domov", icon = icon("home")),
      menuItem("Pravila igre 301", tabName = "pravila", icon = icon("list-alt")),
      menuItem("Matematično ozadje", tabName = "ozadje", icon = icon("book")),
      menuItem("Strategija 1", tabName = "strategija1", icon = icon("trophy")),
      menuItem("Strategija 2", tabName = "strategija2", icon = icon("trophy")),
      menuItem("GitHub", icon = icon("github"), href = "https://github.com/tinarazic/pikado")
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "domov",
              imageOutput("tabla")),

      tabItem(tabName = "pravila",
              h1("Pravila igre 301"),
              includeMarkdown("shiny_pravila_igre.md")),
      
      tabItem(tabName = "ozadje",
              h1("Matematično ozadje"),
              includeMarkdown("shiny_matematicno_ozadje.Rmd")),
      
      tabItem(tabName = "strategija1",
              h1("Kam naj ciljam, da zadanem največ točk?"),
              fluidPage(
                
                radioButtons("radio", label = h3("Nivo igralca"),
                             choices = list("Začetnik" = 1, "Rekreativec" = 2, "Profesionalec" = 3), 
                             selected = 1), 
                
                plotOutput('plot1')
                
              )),
      
      tabItem(tabName = "strategija2",
              h1("Kam naj ciljam, da čimprej končam igro?"))
      
    )
    
  )
)