

library(shiny)
library(shinydashboard)



ui <- fluidPage(
  titlePanel("Dice Graphics") ,
  imageOutput("die",height="410px",width="410px",click="clickdie",inline=TRUE),
  p("Die face images by Nein Arimasen, CC BY-SA 3.0 <http://creativecommons.org/licenses/by-sa/3.0/>, via Wikimedia Commons")
                )

server <- function(input, output, session) {
  boardstate <- reactiveValues(dieNumber=2)
  
  output$die <- renderImage({
    #select the icon appropriate for this die
    imageid <- boardstate$dieNumber
    imgsrc=switch(imageid,"www/Die1.png","www/Die2.png","www/Die3.png","www/Die4.png","www/Die5.png","www/Die6.png")
    # Unfortunately, we are not able to re-size the image and still have the click event work. 
    # So the image files must have exactly the size we want.
    # Also, z-order works only if 'position' is set.
    list(src=imgsrc,style="position:relative;z-order:999;") 
  },deleteFile=FALSE)
  
  observeEvent(input$clickdie,{
    # generate a random number between 1 and 7 and then truncate to an integer to get a random die number
    boardstate$dieNumber = as.integer(runif(1,1,7)) 
    
  })
  
  
  
}n 
  
shinyApp(ui = ui, server = server)
