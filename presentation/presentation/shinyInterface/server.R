#server.R 
#library(datasets)
library(audio)

source("./../backEnd/functions.R")

#savePath<-"./../recordings/smaple.wav"
print("INIT")
#print("duration:")
#print(duration)
#print("rate: ")
#print(rate)
#print("savePath: ")
#print(savePath)
#print("")

#print(length(timeSpan))
#print(length(soundSample))

soundSample<<- rep(0, 440320)
timeSpan<- 1:440320

shinyServer(function(input, output) {


  iks <- eventReactive(input$nagraj,{
    
    print("CLICK!")
    #print(getwd())
    shell("recordInit.bat")
    wait(6)
    soundSample <- load.wave("./../records/sample.wav")
	timeSpan <- 1:length(soundSample)
    print(length(soundSample))
    print(length(timeSpan))
    #print("Zapisano plik")
    return(soundSample)
  })

  output$plot <- renderPlot({
   # input$nagraj
print(length(soundSample))
    print(length(timeSpan))
    print("RENDER-START")
    plot(timeSpan,iks())
    print("RENDER-STOP")
  })
  
  output$text <- reactive({
    input$porownaj
    switch(myfunction(),
           "1" = "Zapal swiatlo",
           "2" = "Nagrzej piekarnik...",
           "3" = "Scisz telewizje",
           "4" = "Wlacz pranie",
           "5" = "Zrob kawe", 
           "6" = "Nie rozpoznano komendy")
  })
  
})