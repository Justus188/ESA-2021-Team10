source("lastTile.R")
source("event.R")
source("restaurant+movement.R")
library(shiny)
library(shinydashboard)

# retrieve 5 random food from database, store their respective names, images, ingredient list,
# and filling levels in dataframe
food1name = "Filet-O-fish"    # for testing
food2name = "McSpicy"         # for testing
food3name = "McChicken"       # for testing
food4name = "Cheese Burger"   # for testing
food5name = "McWrap"          # for testing

food1_image = "FOF.png"                                      # for testing
food2_image = "food_main_mcspicy.png"                        # for testing
food3_image = "McChicken.png"                                # for testing
food4_image = "cheeseburger.png"                             # for testing
food5_image = "mcwrapgrilled.png"                            # for testing
food1_ingredients = "Fish fillet, cheese, buns"              # for testing
food2_ingredients = "Spicy Chicken fillet, lettuce, buns"    # for testing
food3_ingredients = "Chicken fillet, lettuce, buns"          # for testing
food4_ingredients = "Beef patty, cheese, pickles, buns"      # for testing
food5_ingredients = "Grilled chicken, lettuce, tortilla"     # for testing

### Helpers ####################################################################
createTile <- function(j, i=0) return(imageOutput(paste0("cell", i, j), height="50px", width = "50px", inline = T))
createRow <- function(i) return(lapply(0:9, createTile, i = i))
genCellIds <- function(){
  initIds = as.character(0:99)
  initIds[1:10] <- paste0("0", initIds[1:10])
  initIds
}
listofcells = lapply(genCellIds(), function(x) return(paste0("cell", x)))
renderCell <- function(playerpos, cellid, input, isEvent){
  # Renders token if occupied, else empty
  row <- as.integer(substr(cellid, 1,1))
  col <- as.integer(substr(cellid, 2,2))
  
  renderImage({
    imgsrc <- "www/Blank.png" #Blank
    if ((row %in% c(0,9)) || (col %in% c(0,9))) {
      if(playerpos[1] == row && playerpos[2] == col) {
        imgsrc <- paste0("www/",getTokenSrc(input))
      } else if (isEvent[row+1, col+1]){
        #some event picture
        imgsrc <- "www/token3.png"
      }
    }
    # Unfortunately, we are not able to re-size the image and still have the click event work.
    # So the image files must have exactly the size we want.
    # Also, z-order works only if 'position' is set.
    list(src=imgsrc,style="position:relative;z-order:999;")
  },deleteFile=FALSE)
}

ui <- dashboardPage( ###########################################################
    dashboardHeader(title = "CARElorie"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabSelect",
            menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
            menuItem("Gameboard", tabName = "gameboard", icon = icon("chess-board")),
            menuItem("Leaderboard",tabName = "leaderboard", icon=icon("trophy"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "welcome",
                    h1("Welcome to CARElorie!"),
                    h2("Start the game by choosing your player token and clicking on the Start button!"),
                    tags$br(),
                    selectInput("playertoken", "Select your player token:",c("Burger","Fries","Apple")),
                    uiOutput("playertokenimg"),
                    tags$h4("Instructions"),
                    tags$p(" INSERT INSTRUCTIONS "), #TODO: Update this
                    actionButton("start_welcome", "Start the Game!"),
                    
                    ### Temp Stuff
                    actionButton("temp.lastTileTrigger", "Last Tile"),
                    actionButton("temp.eventTrigger", label="Land on Event"),
                    actionButton("testmenu","TestMenu"),
                    actionButton("teststarving","TestStarving"),
                    textOutput("testvar")
            ),
            tabItem(tabName = "gameboard",
                    h2("CARElorie"),
                    h2("Start the game by clicking the die on the right"),
                    sidebarLayout( 
                        # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
                        # Then, to superimpose images, style them to be 'position:relative;z-order:999'
                        mainPanel(width=10,
                                  img(src='board.png',style="position:absolute;z-order:0",width="525px",height="505px"),
                                  createRow(0), tags$br(),
                                  createRow(1), tags$br(),
                                  createRow(2), tags$br(),
                                  createRow(3), tags$br(),
                                  createRow(4), tags$br(),
                                  createRow(5), tags$br(),
                                  createRow(6), tags$br(),
                                  createRow(7), tags$br(),
                                  createRow(8), tags$br(),
                                  createRow(9), tags$br()),
                        sidebarPanel( width = 2,
                                      selectInput("r_or_e", label = "Player lands on:", c("movement","event","restaurant")),
                                      # only show this panel if player is rolling the die
                                      conditionalPanel(
                                          condition = "output.dice == true",
                                          uiOutput("hunger_scale"),
                                          imageOutput("die",height="410px",width="410px",click="clickdie",inline=TRUE)
                                      ),
                                      
                                      # Only show this panel if land on restaurant tile
                                      conditionalPanel(
                                        condition = "output.restaurant == true",
                                        tags$h1("Menu"), # change the food names to random 5 food names from database
                                        selectInput(inputId = "Chosenfood", 
                                                    label = "Please choose something:",   
                                                    getMenu()[,"Food"]
                                        ),
                                        uiOutput("foodimg"), textOutput("foodingredients"), 
                                        textOutput("fillinglevel"),
                                        actionButton("choosefood_yes","ok"),
                                        actionButton("choosefood_no","cancel")
                                      ),
                                      # Only show this panel if land on event
                                      conditionalPanel(
                                          condition = "output.event == true",
                                          fluidRow(column(4, offset = 3,
                                                          tags$h1("Event")
                                                   ),
                                          )
                                      )
                        )
                    )
            ),
            tabItem(tabName = "leaderboard", 
                    h2("Publish Your Score"),
                    box(
                      title = "See where you stand!",width=12,
                      tableOutput("leaderboard"),
                      "Not here? Play again!",
                      actionButton("start_leaderboard", "Start the Game!")
                    )
            )
        )
    ))

server <- function(input, output, session) {####################################
  ### TEMPORARY Triggers #######################################################
  observeEvent(input$teststarving, showModal(starvingModal()))
  observeEvent(input$starvingok, removeModal())
  
  observeEvent(input$testmenu, showModal(menuModal()))
  
  observeEvent(input$temp.eventTrigger, {showModal(QuestionModal(question_no=rand_qn))})
  observeEvent(input$continuebutton, {
    #Close the current modal
    removeModal()
  })
  
  observeEvent(input$temp.lastTileTrigger, showModal(endModal()))

  output$testvar <- NULL
  
  ### Init #####################################################################

  allmenu <- getallMenu() #df with all menu items
  
  ### Regenerate on new game ###################################################
  isEvent <- matrix(runif(100, 0,1) <0.2, byrow=T, nrow=10) # matrix[row+1, col+1] of isEvent tile

  vals <- reactiveValues(calories = 9,
                         hunger = 10,
                         dieNumber = 2,
                         QuestionNo = NULL,
                         boardstate = -1, # Move -1, Event 0, Restaurant 1, End 2
                         playerpos = c(9, 9), # (row,col) track token location; each edge has length 10 starting from (10,10), ends(9,10)
                         action.log = data.frame(Food="Burger", Calories=as.integer(10), Hunger = "+10"))
  
  ### WORK IN PROGRESS: TO SHIFT DOWN EVENTUALLY ###############################
  output$img1 <- renderUI({
    if(input$Chosenfood == food1name){
      img(src = food1_image,height = 200, width = 200)}
    else if(input$Chosenfood == food2name){
      img(src = food2_image, height = 200, width = 200)}
    else if(input$Chosenfood == food3name){
      img(src = food3_image,height = 200, width = 200)}
    else if(input$Chosenfood == food4name){
      img(src = food4_image,height = 200, width = 200)}
    else if(input$Chosenfood == food5name){
      img(src = food5_image,height = 200, width = 200)}
  })
  
  output$foodingredients <- renderText({
    if(input$Chosenfood == food1name){
      paste("Main ingredients:", food1_ingredients)}
    else if(input$Chosenfood == food2name){
      paste("Main ingredients:", food2_ingredients)}
    else if(input$Chosenfood == food3name){
      paste("Main ingredients:", food3_ingredients)}
    else if(input$Chosenfood == food4name){
      paste("Main ingredients:", food4_ingredients)}
    else if(input$Chosenfood == food5name){
      paste("Main ingredients:", food5_ingredients)}
  })
  
  output$fillinglevel <- renderText({
    if(input$Chosenfood == food1name){
      paste("Fullness factor:??")}
    else if(input$Chosenfood == food2name){
      paste("Fullness factor:??")}
    else if(input$Chosenfood == food3name){
      paste("Fullness factor:??")}
    else if(input$Chosenfood == food4name){
      paste("Fullness factor:??")}
    else if(input$Chosenfood == food5name){
      paste("Fullness factor:??")}
    
  })
  
  output$hunger_scale <- renderUI({
    #select the appropriate hunger level image from www
    image_hunger <- vals$hunger 
    imgsource = switch(image_hunger, "www/hunger1.png","www/hunger2.png","www/hunger3.png","www/hunger4.png","www/hunger5.png","www/hunger6.png")
    
  })
  
  ### RENDERING FUNCTIONS ######################################################
  output$leaderboard <- renderTable({ 
    numclicks=input$name.done
    getLeaderBoard()
  })
  
  observe({vals$playerpos
    mapply(function(x, y) {output[[x]] <- renderCell(vals$playerpos, y, input, isEvent)}, x=listofcells, y=genCellIds())
  })
  
  output$die <- renderImage({
    #select the icon appropriate for this die
    imageid <- vals$dieNumber
    imgsrc=switch(imageid,"www/Die1.png","www/Die2.png","www/Die3.png","www/Die4.png","www/Die5.png","www/Die6.png")
    # Unfortunately, we are not able to re-size the image and still have the click event work.
    # So the image files must have exactly the size we want.
    # Also, z-order works only if 'position' is set.
    list(src=imgsrc,style="position:relative;z-order:999;")
  },deleteFile=FALSE)
  
  # Code for displaying of the playertoken image in the mainpage, update & store the chosen token image for the rest of the game in line code 148 (after pressing start button)
  output$playertokenimg <- renderUI(img(src = getTokenSrc(input), height=50, width=50))
  
  ### GAME LOGIC ###############################################################
  observeEvent(input$start_welcome,{
    removeModal()
    updateTabItems(session, "tabSelect", "gameboard")
    print("Starting game")
  }, ignoreInit = T)
  
  observeEvent(input$clickdie,{
    vals$dieNumber = as.integer(runif(1,1,7))
    
    vals$playerpos <- updateBoardState(vals$playerpos,vals$dieNumber)
    
    vals$hunger <- vals$hunger - 100*vals$dieNumber
    
    if (vals$hunger<0) { #check for starving
      showModal(starvingModal())
      vals$calories <- vals$calories + 1500
      vals$hunger <- vals$hunger + 1000
    }
    
    currentrow <- vals$playerpos[1]
    currentcol <- vals$playerpos[2]
    
    vals$boardstate <- checktile(currentrow, currentcol, isEvent) # Boardstates: -1: Dice, 0 Event, 1 Restaurant, 2 End
    if(vals$boardstate ==2) showModal(endModal())
  })
  
  output$dice <- reactive(vals$boardstate == -1)
  output$event <- reactive(vals$boardstate == 0)
  output$restaurant <- reactive(vals$boardstate == 1)
  for (outputpanel in c("dice", "event", "restaurant")) outputOptions(output, outputpanel, suspendWhenHidden=F)

  observeEvent(input$choosefood_yes,{
    allMenu <- getallMenu()
    vals$calories <- vals$calories + allmenu[allmenu$Food == input$Chosenfood,"Calories"]
    vals$hunger <- vals$hunger + allmenu[allmenu$Food == input$Chosenfood,"Hunger"]
    
    vals$boardstate <- -1
  })

  ### Event logic
  observeEvent(input$checkbutton,{
    # DEBUG - Print statements
    print(vals$QuestionNo)
    print(input$selectedAns)
    #Check the user's ans
    outcome <- checkAnswer(vals$QuestionNo, input$selectedAns)

    if (outcome==TRUE){
      #Tell them that they selected the correct answer
      #Change the button from Check to Proceed to Event
      #Close the current modal
      removeModal()
      showModal(QuestionModal(correct = TRUE, wrong=FALSE, question_no=vals$QuestionNo))
      #Able to Proceed to Random Events page

    }else{
      #Tell them that they selected the wrong answer
      #Change the button from Check to Proceed to Event
      #Close the current modal
      removeModal()
      showModal(QuestionModal(correct = FALSE, wrong=TRUE, question_no=vals$QuestionNo))
      #Able to Proceed to Random Events page
    }
  })

  observeEvent(input$proceedbutton, {
    #Close the current modal
    removeModal()

    #Open up a new modal that shows the randomization part
    show_modal_gif(src = "EventRandom.gif",width="200px",height="200px",modal_size = "s")
    Sys.sleep(5)
    remove_modal_gif()

    #Show the end result
    ##Assume there are 10 events, randomly choose 1 event,
    EventNo <- sample(1:as.numeric(getMaxNumberOfEvents()), 1)

    showModal(EventsModal(EventNo))

    #Should add in parts to modify Hunger, Calories and Board Position values.
    if (getEventType(EventNo) == 1){
      #Got a good event, decrease Calories, increase Hunger, PLEASE ADVISE how much
      vals$calories =vals$calories - 5
      vals$hunger =vals$hunger + 5
    }else{
      vals$calories =vals$calories + 5
      vals$hunger =vals$hunger - 5
    }
  }
  )

  ### endModal renders
  output$actionlog <- renderTable(vals$action.log)
  output$leaderboard_endModal <- renderTable({ 
    numclicks=input$name.done
    getLeaderBoard()
  })

  ### Publish
  observeEvent(input$publish, showModal(nameModal())) # Get player name
  observeEvent(input$name.done, {
    publishScore(input$name.name, vals$calories)
    showModal(publishedModal())
  })

  ### Quit correctly
  # observeEvent(c(input$quit_endModal, input$quit_endModal, input$quit_pubishedModal), stopApp(), ignoreInit = T)
  

}

shinyApp(ui, server)
