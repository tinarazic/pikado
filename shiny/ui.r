fluidPage(
  
  # Copy the line below to make a set of radio buttons
  radioButtons("radio", label = h3("Nivo igralca"),
               choices = list("Začetnik" = 1, "Rekreativec" = 2, "Profesionalec" = 3), 
               selected = 1), 
  
  plotOutput('plot1')
  
)