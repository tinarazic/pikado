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
              img(src='logo2.png', height="50%", width="50%", align="center")),

      tabItem(tabName = "pravila",
              h1("Igra"),
              includeMarkdown("shiny_pravila_igre.md")),
      
      tabItem(tabName = "predpostavke",
              h1("Matematično ozadje"), 
              withMathJax(includeMarkdown(knit("shiny_matematicno_ozadje.Rmd")))),
    
      tabItem(tabName = "ozadje_strategija1",
              h1("Optimalna strategija 1"),
              withMathJax(includeMarkdown(knit("shiny_strategija1.Rmd")))),
      
      tabItem(tabName = "ozadje_strategija2",
              h1("Optimalna strategija 2"),
              withMathJax(includeMarkdown(knit("shiny_strategija2.Rmd")))),
      
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