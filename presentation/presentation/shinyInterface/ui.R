# ui.R

shinyUI(fluidPage( 
  
  titlePanel("AGH, Systemy Glosowe"),
  
  sidebarLayout( 
    sidebarPanel(
      p("Aplikacja umożliwia nagranie własnych komend
               głosowych oraz ich identyfikacje na podstawie 
               wbudowanego słownika"),
      
      helpText("Dostepne komendy to :"),
      helpText("-Zapal swiatlo"),
      helpText("-Nagrzej piekarnik..."),
      helpText("-Scisz telewizje"), 
      helpText("-Wlacz pranie"),
      helpText("-Zrob kawe"),
      helpText("-Otworz okna") 
    ),
    mainPanel(
      fluidRow(
        helpText("Uzyj przycisku \"Nagrywanie\", aby rozpoczac 5-sekundowe nagrywanie komendy"),
        actionButton("nagraj", label = "Nagrywanie"),
        plotOutput("plot")
 
      ),

      fluidRow(
        helpText("Aby zidentyfikowac podana komende uzyj przycisku \"Porownaj\""),
        actionButton("porownaj", label = "Porownaj"),
        h1(textOutput("text"))      
      )
    )
    )
))