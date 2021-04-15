source("lastTile.R")
source("event.R")
source("restaurant+movement.R")
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "CARElorie"),
    dashboardSidebar(
        sidebarMenu(
            #https://fontawesome.com/icons?d=gallery
            id = "tabSelect",
            menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
            menuItem("Gameboard", tabName = "gameboard", icon = icon("chess-board")),
            menuItem("Leaderboard",tabName = "leaderboard", icon=icon("trophy"))
        )
        
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "welcome",
                    h1("Welcome to CARElorie!"),
                    h2("Start the game by choosing your player token and clicking on the Start button!"),
                    tags$br(),
                    selectInput("playertoken", "Select your player token:",c("burger","fries","apple")),
                    uiOutput("playertokenimg"), # image output code line 355-ish
                    tags$h4("Instructions"),
                    tags$p(" INSERT INSTRUCTIONS "),
                    actionButton("start", "Start the Game!"),
                    actionButton("temp.lastTileTrigger", "Last Tile"),
                    actionButton("temp.eventTrigger", label="Land on Event"),
                    actionButton("testmenu","TestMenu"),
                    actionButton("teststarving","TestStarving")
                    
            ),
            
            # Second tab content
            tabItem(tabName = "gameboard",
                    #h2("CARElorie"),
                    #h2("Start the game by clicking the die on the right"),
                    fluidRow(
                        #title = "A Very Simple Board Game",width=12,
                        #htmlOutput("playercolorchoice"),
                        # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
                        # Then, to superimpose images, style them to be 'position:relative;z-order:999'
                        
                        sidebarLayout( 
                            mainPanel(width=9,box(img(src='board.png',style="position:absolute;z-order:0",width="920px",height="550px"),
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
                            )),
                            
                            sidebarPanel( width = 3,
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
                            ))
                        
                    )
            ),
            
            tabItem(tabName = "leaderboard", 
                    h2("Publish Your Score"),
                    fluidRow(
                        box(
                            title = "See where you stand!",width=12,
                            tableOutput("leaderboard"),
                            "Not here? Play again!",
                            actionButton("start", "Start the Game!")
                        ))
                    
                    
            )
        )
        
        
    ))

server <- function(input, output, session) {
  ### Global variables (ewww)
  # VARIANTMONOPOLY <- 1
  # REDTURN <- 0
  # BLUETURN <- 1
  CELLEMPTY <- 1
  CELLRED <- 2
  # CELLBLUE <- 3
  # CELLBOTH <- 4
  GRIDSIZE <- 4

  pieces <- matrix(rep(CELLEMPTY,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  #starting position of player
  pieces[2,1] <- CELLRED
  #df containing all menuitems
  allmenu <- getallMenu()
  #list containing coordinates in the form of paste(row,col) to check if player is on event tile
  eventlist <- list()

  ### Initialise reactive values
  #boardstate represents movement,event,restaurant,last tile using NULL,0,1,2 respectively
  vals <- reactiveValues(calories = 9,
                         hunger = 10,
                         dieNumber = 2,
                         QuestionNo = NULL,
                         boardstate = NULL,
                         action.log = data.frame(Food="Burger", Calories=as.integer(10), Hunger = "+10"))

  ### MODULES TO BE ADDED - Start the game
  observeEvent(input$start, {
    updateTabItems(session, "tabSelect", "gameboard")
    print("Starting game")
  }) #Insert start game code here
    ## update the playertoken image that the player chose 
    
  ### MODULES TO BE ADDED - Game body goes here
  output$gameboard <- renderImage(NULL)

  ### TEMPORARY - Starving test
  observeEvent(input$teststarving, showModal(starvingModal()))
  observeEvent(input$starvingok, removeModal())

  ### TEMPORARY - Menu test
  observeEvent(input$testmenu, showModal(menuModal()))
  observeEvent(input$menuok,{
    if (input$menuitem == "Choose Something") {showModal(menuModal(failed = TRUE))} else {
      vals$calories <- vals$calories + restaurantmenu[restaurantmenu$menuitem == input$menuitem,2]
      vals$hunger <- vals$hunger + restaurantmenu[restaurantmenu$menuitem == input$menuitem,3]
      print(vals$calories)
      print(vals$hunger)
      removeModal()
    }
  })
  observeEvent(input$menuitem,{
    # if item is chosen, alter calories and hunger based on item else dont do anything
    if (input$menuitem != "Choose Something") {

    } else {}
  })

  ### TEMPORARY - Event trigger
  observeEvent(input$temp.eventTrigger, {
    #Show MCQ Question
    showModal(QuestionModal(question_no=rand_qn))
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
  observeEvent(input$continuebutton, {
    #Close the current modal
    removeModal()
  })

  ### TEMPORARY - End of game trigger
  observeEvent(input$temp.lastTileTrigger, showModal(endModal()))

  ### endModal renders
  output$actionlog <- renderTable(vals$action.log)
  output$leaderboard <- renderTable({
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
  observeEvent(input$quit, stopApp())


  ### Future reference stuff - GET IMAGES
  renderCell <- function(gridrow,gridcol){
    renderImage({
      #select the icon appropriate for this cell
      imageid <- getImageId(gridrow,gridcol,vals)
      # style the image (add a border) to convey other information such as allowable moves
      imgstyle <-  getImageStyle(gridrow,gridcol,vals)
      imgsrc=switch(imageid,"www/emptycell.png","www/RedStoneSmall.png")
      # Unfortunately, we are not able to re-size the image and still have the click event work.
      # So the image files must have exactly the size we want.
      # Also, z-order works only if 'position' is set.
      list(src=imgsrc,style=paste0("position:relative;z-order:999;",imgstyle))
    },deleteFile=FALSE)
  }

  output$cell11 <- renderCell(1,1)
  output$cell12 <- renderCell(1,2)
  output$cell13 <- renderCell(1,3)
  output$cell14 <- renderCell(1,4)
  output$cell21 <- renderCell(2,1)
  output$cell22 <- renderCell(2,2)
  output$cell23 <- renderCell(2,3)
  output$cell24 <- renderCell(2,4)
  output$cell31 <- renderCell(3,1)
  output$cell32 <- renderCell(3,2)
  output$cell33 <- renderCell(3,3)
  output$cell34 <- renderCell(3,4)
  output$cell41 <- renderCell(4,1)
  output$cell42 <- renderCell(4,2)
  output$cell43 <- renderCell(4,3)
  output$cell44 <- renderCell(4,4)

  processClickEvent <- function(gridrow,gridcol){

  }

  observeEvent(input$click11,{processClickEvent(1,1)})
  observeEvent(input$click12,{processClickEvent(1,2)})
  observeEvent(input$click13,{processClickEvent(1,3)})
  observeEvent(input$click14,{processClickEvent(1,4)})
  observeEvent(input$click21,{processClickEvent(2,1)})
  observeEvent(input$click22,{processClickEvent(2,2)})
  observeEvent(input$click23,{processClickEvent(2,3)})
  observeEvent(input$click24,{processClickEvent(2,4)})
  observeEvent(input$click31,{processClickEvent(3,1)})
  observeEvent(input$click32,{processClickEvent(3,2)})
  observeEvent(input$click33,{processClickEvent(3,3)})
  observeEvent(input$click34,{processClickEvent(3,4)})
  observeEvent(input$click41,{processClickEvent(4,1)})
  observeEvent(input$click42,{processClickEvent(4,2)})
  observeEvent(input$click43,{processClickEvent(4,3)})
  observeEvent(input$click44,{processClickEvent(4,4)})


  output$die <- renderImage({
    #select the icon appropriate for this die
    imageid <- vals$dieNumber
    imgsrc=switch(imageid,"www/Die1.png","www/Die2.png","www/Die3.png","www/Die4.png","www/Die5.png","www/Die6.png")
    # Unfortunately, we are not able to re-size the image and still have the click event work.
    # So the image files must have exactly the size we want.
    # Also, z-order works only if 'position' is set.
    list(src=imgsrc,style="position:relative;z-order:999;")
  },deleteFile=FALSE)

  observeEvent(input$clickdie,{
    # change turn using mod operator
    # vals$playerturn <- (vals$playerturn+1) %% 2
    # generate a random number between 1 and 7 and then truncate to an integer to get a random die number
    vals$dieNumber = as.integer(runif(1,1,7))
    # For testing set the dieNumber to 1
    #vals$dieNumber <- 1
    vals$pieces <- updateBoardState(vals$pieces,vals$dieNumber)
    #omitted hunger alteration for testing
    #vals$hunger <- vals$hunger - vals$dieNumber
    if (vals$hunger<0) {
      showModal(starvingModal())
      vals$calories <- vals$calories + 3000
      vals$hunger <- vals$hunger + 30
    }
    locationindex <- which(pieces == CELLRED)
    currentcol <- as.integer((locationindex-1)/GRIDSIZE)+1
    currentrow <- locationindex - (gridcol-1)*GRIDSIZE
    # checktile(row,col) checks what kind of tile you are in, returns 1 when restaurant, 2 when last tile, 0 when event
    vals$boardstate <- checktile(currentrow, currentcol) # r creates copies, not pointers, so this should be fine
    switch(vals$boardstate+1,
           #Code for boardstate=0
           ,
           showModal(endModal()) # boardstate = 2
    )
  })
    
      # Code for displaying of the playertoken image in the mainpage, update & store the chosen token image for the rest of the game in line code 148 (after pressing start button)
    output$playertokenimg <- renderUI({
     if(input$playertoken == "burger"){
        img(src = "token1.png",height = 50, width = 50)}
      else if(input$playertoken == "fries"){
        img(src = "token2.png", height = 50, width = 50)}
      else if(input$playertoken == "apple"){
       img(src = "token3.png",height = 50, width = 50)}
    
  })
  
    
        # retrieve 5 random food from database, store their respective images, ingredient lista and filling levels in reactive_value
    reactive_value <- reactiveValues(food1_image = NULL,
                                     food2_image = NULL,
                                     food3_image = NULL,
                                     food4_image = NULL,
                                     food5_image = NULL,
                                     food1_ingredients=NULL, 
                                     food2_ingredients= NULL, 
                                     food3_ingredients= NULL, 
                                     food4_ingredients= NULL, 
                                     food5_ingredients= NULL)
    
    reactive_value$food1_image = "FOF.png"                                      # for testing
    reactive_value$food2_image = "food_main_mcspicy.png"                        # for testing
    reactive_value$food3_image = "McChicken.png"                                # for testing
    reactive_value$food4_image = "cheeseburger.png"                             # for testing
    reactive_value$food5_image = "mcwrapgrilled.png"                            # for testing
    reactive_value$food1_ingredients = "Fish fillet, cheese, buns"              # for testing
    reactive_value$food2_ingredients = "Spicy Chicken fillet, lettuce, buns"    # for testing
    reactive_value$food3_ingredients = "Chicken fillet, lettuce, buns"          # for testing
    reactive_value$food4_ingredients = "Beef patty, cheese, pickles, buns"      # for testing
    reactive_value$food5_ingredients = "Grilled chicken, lettuce, tortilla"     # for testing
    
    output$img1 <- renderUI({
        if(input$Chosenfood == "food1"){
            img(src = reactive_value$food1_image,height = 200, width = 200)}
        else if(input$Chosenfood == "food2"){
            img(src = reactive_value$food2_image, height = 200, width = 200)}
        else if(input$Chosenfood == "food3"){
            img(src = reactive_value$food3_image,height = 200, width = 200)}
        else if(input$Chosenfood == "food4"){
            img(src = reactive_value$food4_image,height = 200, width = 200)}
        else if(input$Chosenfood == "food5"){
            img(src = reactive_value$food5_image,height = 200, width = 200)}
    })
    
    output$foodingredients <- renderText({
        if(input$Chosenfood == "food1"){
            paste("Main ingredients:", reactive_value$food1_ingredients)}
        else if(input$Chosenfood == "food2"){
            paste("Main ingredients:", reactive_value$food2_ingredients)}
        else if(input$Chosenfood == "food3"){
            paste("Main ingredients:", reactive_value$food3_ingredients)}
        else if(input$Chosenfood == "food4"){
            paste("Main ingredients:", reactive_value$food4_ingredients)}
        else if(input$Chosenfood == "food5"){
            paste("Main ingredients:", reactive_value$food5_ingredients)}
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
        #select the appropriate hunger level image from www
        image_hunger <- vals$hunger 
        imgsource = switch(image_hunger, "www/hunger1.png","www/hunger2.png","www/hunger3.png","www/hunger4.png","www/hunger5.png","www/hunger6.png")
        
    })
}

shinyApp(ui, server)
