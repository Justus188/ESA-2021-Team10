#References:
#https://stackoverflow.com/questions/40305908/how-to-show-different-images-according-to-selectinput-in-shiny
#https://shiny.rstudio.com/reference/shiny/1.6.0/conditionalPanel.html
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/
#--------------------------------------- Restaurant menu-----------------------------------------
library(shiny)


ui <- fluidPage(
  sidebarLayout(
    mainPanel(titlePanel("CARElorie"),
    img(src='FantasyMap.jpg',style="position:absolute;z-order:0",width="800px",height="550px"),
    imageOutput("cell11",height="100px",width="100px",click="click11",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell12",height="100px",width="100px",click="click12",inline=TRUE),  # height and width are for the containing div, not the image itself
    imageOutput("cell13",height="100px",width="100px",click="click13",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell14",height="100px",width="100px",click="click14",inline=TRUE),  # height and width are for the containing div, not the image itself
    tags$br(),
    imageOutput("cell21",height="100px",width="100px",click="click21",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell22",height="100px",width="100px",click="click22",inline=TRUE),  # height and width are for the containing div, not the image itself
    imageOutput("cell23",height="100px",width="100px",click="click23",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell24",height="100px",width="100px",click="click24",inline=TRUE),  # height and width are for the containing div, not the image itself
    tags$br(),
    imageOutput("cell31",height="100px",width="100px",click="click31",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell32",height="100px",width="100px",click="click32",inline=TRUE),  # height and width are for the containing div, not the image itself
    imageOutput("cell33",height="100px",width="100px",click="click33",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell34",height="100px",width="100px",click="click34",inline=TRUE),  # height and width are for the containing div, not the image itself
    tags$br(),
    imageOutput("cell41",height="100px",width="100px",click="click41",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell42",height="100px",width="100px",click="click42",inline=TRUE),  # height and width are for the containing div, not the image itself
    imageOutput("cell43",height="100px",width="100px",click="click43",inline=TRUE), # height and width are for the containing div, not the image itself
    imageOutput("cell44",height="100px",width="100px",click="click44",inline=TRUE),  # height and width are for the containing div, not the image itself
    ),
    
    sidebarPanel(
      selectInput("r_or_e", label = "Player lands on:", c("movement","event","restaurant")),
      # only show this panel if player is rolling the die
    conditionalPanel(
      condition = "input.r_or_e == 'movement'",
      uiOutput("hunger_scale"),
      imageOutput("die",height="410px",width="410px",click="clickdie",inline=TRUE)
    ),
      
    # Only show this panel if land on restaurant tile
    conditionalPanel(
      condition = "input.r_or_e == 'restaurant'",
                    tags$h1("Menu"), # change the food names to random 5 food names from database
                    selectInput( inputId = "Chosenfood", label = "Please choose something:",   
                                 c( Filet_O_Fish = "food1", McSpicy = "food2", McChicken = "food3", CheeseBurger = "food4", Mcwrap = "food5" )
                    ), uiOutput("img1"), textOutput("foodingredients"), textOutput("fillinglevel"),actionButton("choosefood_yes","ok"),actionButton("choosefood_no","cancel")
    )
,
      # Only show this panel if land on event
      conditionalPanel(
        condition = "input.r_or_e == 'event'",
      fluidRow(column(4, offset = 3,
                      tags$h1("Event")),
              )
        
      )
)))

# Put all food images in www folder, extract url of the 5 chosen food from database and replace src with url
server <- function(input, output,session) {
  
  val <- reactiveValues(food1_ingredients=NULL, food2_ingredients= NULL, food3_ingredients= NULL, food4_ingredients= NULL, food5_ingredients= NULL)
  val$food1_ingredients = "Fish fillet, cheese, buns"   # for testing
  val$food2_ingredients = "Spicy Chicken fillet, lettuce, buns"   # for testing
  val$food3_ingredients = "Chicken fillet, lettuce, buns"   # for testing
  val$food4_ingredients = "Beef patty, cheese, pickles, buns"   # for testing
  val$food5_ingredients = "Grilled chicken, lettuce, tortilla"   # for testing
  
    output$img1 <- renderUI({
        if(input$Chosenfood == "food1"){
            img(src = "FOF.png",height = 240, width = 300)}
        else if(input$Chosenfood == "food2"){
          img(src = "food_main_mcspicy.png", height = 240, width = 300)}
        else if(input$Chosenfood == "food3"){
          img(src = "McChicken.png",height = 240, width = 300)}
        else if(input$Chosenfood == "food4"){
          img(src = "cheeseburger.png",height = 240, width = 300)}
        else if(input$Chosenfood == "food5"){
          img(src = "mcwrapgrilled.png",height = 240, width = 300)}
       })
    
    output$foodingredients <- renderText({
      if(input$Chosenfood == "food1"){
        paste("Main ingredients:", val$food1_ingredients)}
      else if(input$Chosenfood == "food2"){
        paste("Main ingredients:", val$food2_ingredients)}
      else if(input$Chosenfood == "food3"){
        paste("Main ingredients:", val$food3_ingredients)}
      else if(input$Chosenfood == "food4"){
        paste("Main ingredients:", val$food4_ingredients)}
      else if(input$Chosenfood == "food5"){
        paste("Main ingredients:", val$food5_ingredients)}
    })
    
    output$fillinglevel <- renderText({
      if(input$Chosenfood == "food1"){
        paste("Fullness factor:??")}
      else if(input$Chosenfood == "food2"){
        paste("Fullness factor:??")}
      else if(input$Chosenfood == "food3"){
        paste("Fullness factor:??")}
      else if(input$Chosenfood == "food4"){
        paste("Fullness factor:??")}
      else if(input$Chosenfood == "food5"){
        paste("Fullness factor:??")}
    
    })
    
    output$hunger_scale <- renderUI({
    # write if/if-else statements to replace the hunger_level image, depending on the updated hunger level calculations
      img(src = "hunger_level.jpg",height = 200, width = 320)
      
    })
    
    output$die <- renderImage({
      #select the icon appropriate for this die
      imageid <- sample.int(6,1)
      imgsrc=switch(imageid,"www/Die1.png","www/Die2.png","www/Die3.png","www/Die4.png","www/Die5.png","www/Die6.png")
      # Unfortunately, we are not able to re-size the image and still have the click event work. 
      # So the image files must have exactly the size we want.
      # Also, z-order works only if 'position' is set.
      list(src=imgsrc,style="position:relative;z-order:999;") 
    },deleteFile=FALSE)
      
      
      
    
      
}
shinyApp(ui, server)

