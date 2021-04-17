source("lastTile.R")
source("event.R")
source("restaurant+movement.R")
library(shiny)
library(shinydashboard)
#library(reactlog)
#reactlog::reactlog_enable()

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
                         selectInput("playertoken", "Select your player token:",c("Burger","Fries","Apple")),
                         uiOutput("playertokenimg", align="center"),
                         menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
                         menuItemOutput("gameboard_gated"),
                         menuItem("Leaderboard",tabName = "leaderboard", icon=icon("trophy")),
                         menuItem("Credits", tabName = "credits", icon=icon("align-justify"))                        
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "welcome",
                                 h1("Welcome to CARElorie!"),
                                 h2("Start the game by choosing your player token and clicking on the Start button!"),
                                 tags$br(),
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
                                                 "Hunger Meter:",
                                                 imageOutput("hunger_scale", height = "100px", width = "100px"),
                                                 # only show this panel if player is rolling the die
                                                 conditionalPanel(
                                                   condition = "output.dice == true",
                                                   "Click on the dice to roll for your movement this turn!",
                                                   imageOutput("die",height="100px",width="100px",click=clickOpts("clickdie", clip=F),inline=TRUE)
                                                 ),
                                                 
                                                 # Only show this panel if land on restaurant tile
                                                 conditionalPanel(
                                                   condition = "output.restaurant == true",
                                                   tags$h1("Menu"), # change the food names to random 5 food names from database
                                                   #selectInput(inputId = "Chosenfood", 
                                                   #            label = "Please choose something:",   
                                                   #            getMenu()[,"Food"]
                                                   #),
                                                   uiOutput("choosemenu"),
                                                   uiOutput("foodimg"), textOutput("foodingredients"), 
                                                   textOutput("fillinglevel"),
                                                   actionButton("choosefood_yes","ok"),
                                                   actionButton("choosefood_no","cancel")
                                                 ),
                                                 
                                                 # Only show this panel if land on event
                                                 conditionalPanel(
                                                   condition = "output.event == true",
                                                   fluidPage(     
                                                     uiOutput("EventPage1"),
                                                     uiOutput("EventPage2"),
                                                     uiOutput("EventPage3")
                                                   )
                                                 )
                                   )
                                 )
                         ),
                         tabItem(tabName = "leaderboard", 
                                 h2("Hall of Fame"),
                                 # conditionalPanel(
                                 #   condition="output.recent_game==true",
                                 #   textOuput("recent_score"),
                                 #   actionButton("publish", "Publish Your Score!")
                                 # ),
                                 # conditionalPanel(
                                 #   condition="output.recent_publish==true",
                                 #   textOutput("pubilish_success")
                                 # ),
                                 box(
                                   title = "See where you stand!",width=12,
                                   tableOutput("leaderboard"),
                                   "Not here? Play again!",
                                   actionButton("start_leaderboard", "Start the Game!")
                                 )
                         ),
                         #image sources tab         
                         tabItem(tabName = "credits",
                                 h1("Image Sources:"),
                                 h2("Dice"),
                                 h5("http://creativecommons.org/licenses/by-sa/3.0/"),
                                 h2("Tokens"),
                                 h5("https://www.freepik.com"),
                                 h2("Restaurant"),
                                 tags$div("https://www.cookist.com/youtiao-recipe-a-delicious-chinese-fried-dough/", tags$br(),
                                          "https://singaporelocalfavourites.com/yong-tau-foo-meat-paste-stock-and-method.htmlhttps://tasteasianfood.com/taro-cake/", tags$br(),
                                          "https://www.angsarap.net/2016/08/26/watercress-pork-rib-soup/", tags$br(),
                                          "https://asianinspirations.com.au/recipes/cantonese-wonton-noodle-soup/", tags$br(),
                                          "https://www.nyonyacooking.com/recipes/wonton-noodles~SJ-yOPiDzqW7", tags$br(),
                                          "https://www.lemonblossoms.com/blog/thai-tom-yum-soup-hot-and-sour-soup/", tags$br(),
                                          "https://whattocooktoday.com/tau-suan.html", tags$br(),
                                          "https://www.foodiecrush.com/my-moms-homemade-spaghetti-and-meat-sauce/", tags$br(),
                                          "https://mcdonalds.com.au/menu/spicy-chicken-mcwrap", tags$br(),
                                          "https://eatwhattonight.com/2016/08/soon-kueh/", tags$br(),
                                          "https://www.recipetineats.com/siu-mai-shumai-steamed-dumplings/", tags$br(),
                                          "https://www.hungrygowhere.com/singapore/the_roti_prata_house/photo/c8760000", tags$br(),
                                          "https://www.todayonline.com/singapore/jail-third-man-involved-attack-prata-stall-cook", tags$br(),
                                          "https://www.kuali.com/recipe/hari-raya/roti-john/")
                                 
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
  leaderBoard <- renderTable({input$name.done; getLeaderBoard()}) #pregenerate leaderboard for use in multiple screens
  
  ### Regenerate on new game # Shift to start() later ##########################
  vals <- reactiveValues(calories = 0,
                         hunger = 2000,
                         dieNumber = 6,
                         boardstate = -1,
                         isEvent = NA, # generated on start
                         playerpos = c(9, 9), # (row,col) track token location; each edge has length 10 starting from (10,10), ends(9,10)
                         QuestionNo = NULL,
                         event_no = NULL,
                         action.log = NA, # generated on start
                         ### Trackers for conditional statements
                         turndiff = 0,
                         startbuttons = rep(0,4) #TODO: Update if changing number of start buttons
          )
  
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
  
  
  ### RENDERING FUNCTIONS ######################################################
  # Code for displaying of the playertoken image in the mainpage, update & store the chosen token image for the rest of the game in line code 148 (after pressing start button)
  output$playertokenimg <- renderUI(img(src = getTokenSrc(input), height=50, width=50))
  
  observeEvent(c(vals$playerpos, vals$isEvent),{
    mapply(function(x, y) {output[[x]] <- renderCell(vals$playerpos, y, input, vals$isEvent)}, x=listofcells, y=genCellIds())
  })
  
  output$hunger_scale <- renderImage(list(src = paste0("www/hunger", min(as.integer(7-vals$hunger/1000),1), ".png")), deleteFile=F)
  output$die <- renderImage({
    #select the icon appropriate for this die
    imageid <- vals$dieNumber
    imgsrc=switch(imageid,"www/Die1.png","www/Die2.png","www/Die3.png","www/Die4.png","www/Die5.png","www/Die6.png")
    # Unfortunately, we are not able to re-size the image and still have the click event work.
    # So the image files must have exactly the size we want.
    # Also, z-order works only if 'position' is set.
    list(src=imgsrc,style="position:relative;z-order:999;")
  },deleteFile=FALSE)
  
  output$leaderboard <- leaderBoard
  
  ### GAME LOGIC ###############################################################
  output$gameboard_gated <- renderMenu({
    if(any(c(input$start_welcome, input$start_leaderboard)>0)) menuItem("Gameboard", tabName = "gameboard", icon = icon("chess-board"))
  })
  
  observe({ # Start game button
    curr_start_buttons <- c(input$start_welcome, input$start_leaderboard, input$start_endModal, input$start_publishedModal)
    curr_start_buttons[is.na(curr_start_buttons)] <- 0
    diff <- which(curr_start_buttons - vals$startbuttons == 1)
    
    if (length(diff) != 0) { # if button that was pressed is bigger than 0
      removeModal()
      # Reinitialise variables; TODO: Implement once gameboard is conditionally hidden by start_welcome
      
      vals$isEvent <- matrix(runif(100, 0,1) <0.2, byrow=T, nrow=10) # matrix[row+1, col+1] of isEvent tile 
      vals$isEvent[9,10] <- F
      vals$calories <- 0
      vals$hunger <- 2000
      vals$dieNumber <- 6
      vals$boardstate <- -1
      vals$playerpos <- c(9, 9)
      
      updateTabItems(session, "tabSelect", "gameboard")
      print("Starting game")
    }
    
    isolate(vals$startbuttons <- curr_start_buttons)
  })
  
  observeEvent(input$clickdie,{ #TODO: Implement a counter to click spam
    vals$menu <- getMenu()
    if (vals$turndiff == 0){
      vals$turndiff = vals$turndiff +1
      vals$dieNumber = as.integer(runif(1,1,7))
      vals$playerpos <- updateBoardState(vals$playerpos,vals$dieNumber)
      vals$hunger <- vals$hunger - 100*vals$dieNumber
      vals$action.log <- add_row(vals$action.log, Event=paste("Travelled", vals$dieNumber, "tiles"), Calories = 0, Hunger = -100*vals$dieNumber)
      
      if (vals$hunger<0) { #check for starving
        showModal(starvingModal())
        vals$calories <- vals$calories + 1500
        vals$hunger <- vals$hunger + 1000
        vals$action.log <- add_row(vals$action.log, Event="Binge ate due to hunger", Calories=1500, Hunger=1000)
      }
      
      currentrow <- vals$playerpos[1]
      currentcol <- vals$playerpos[2]
      
      print(paste0("Die rolled: ", vals$dieNumber, ", Player Pos: ", vals$playerpos[1], vals$playerpos[2])) ### DEBUG
      
      vals$boardstate <- checktile(currentrow, currentcol, vals$isEvent) # Boardstates: -1: Dice, 0 Event, 1 Restaurant, 2 End
      if(vals$boardstate ==2) showModal(endModal())
    }
  })
  
  output$dice <- reactive(vals$boardstate == -1)
  output$event <- reactive(vals$boardstate == 0)
  output$restaurant <- reactive(vals$boardstate == 1)
  output$choosemenu <- renderUI({
    selectInput(
      "Chosenfood",
      "Choose food",
      vals$menu[,"Food"]
    )
  })
  observeEvent(input$start_welcome,{
    updateTabItems(session,"tabSelect","gameboard")
  })
  for (outputpanel in c("dice", "event", "restaurant")) outputOptions(output, outputpanel, suspendWhenHidden=F)
  
  ### Restaurant Logic
  observeEvent(input$choosefood_yes,{
    allMenu <- rbind(getallMenu(),c("Eat Nothing",0,0,0,"Blank.png",-1))
    
    newCalories <- as.integer(allmenu[allmenu$Food == input$Chosenfood,"Calories"])
    newHunger <- as.integer(allmenu[allmenu$Food == input$Chosenfood,"Hunger"])
    
    vals$calories <- vals$calories + newCalories
    vals$hunger <- vals$hunger + newHunger
    vals$action.log <- add_row(vals$action.log, Event=paste("Ate", input$Chosenfood), Calories=newCalories, Hunger=newHunger)
    vals$boardstate <- -1
    vals$turndiff <- vals$turndiff -1
  })
  
  ### Event logic
  observeEvent(vals$boardstate,{if (vals$boardstate==0){ # Should observe for the user's position in the game == event tile's position
    vals$QuestionNo <- getRandQuestionNo()
    
    output$EventPage1 <- renderUI({
      tagList(
        tags$h1("Event"),
        #Show randomly generated question 
        radioButtons(inputId="selectedAns",
                     label=getQuestionStatement(vals$QuestionNo),
                     choices = c("TRUE", "FALSE")),
        actionButton(inputId="checkbutton", label="Check!"))})
  }})
  
  observeEvent(input$checkbutton, { #Pressed the check button
    #Check the user's ans
    outcome <- checkAnswer(vals$QuestionNo, input$selectedAns)
    print(paste0("QuestionNo: ", vals$QuestionNo, 
                 ", SelectedAns: ", input$selectedAns,
                 ", Outcome: ",outcome))
    
    if (outcome==TRUE){
      #Tell them that they selected the correct answer
      #Change the button from Check to Proceed to Event
      
      #Clear EventPage1
      output$EventPage1 <- renderUI(NULL)
      
      #Set up EventPage2
      output$EventPage2 <- renderUI({
        tagList(
          tags$h1("Event"),
          #Show randomly generated question 
          radioButtons(inputId="selectedAns",
                       label=getQuestionStatement(vals$QuestionNo),
                       choices = c("TRUE", "FALSE")),
          tags$b(getQuestionExplanation(vals$QuestionNo)),
          br(),
          tags$b("Answer is Correct! Press proceed to continue"),
          actionButton(inputId="proceedbutton", label="Proceed!"))})
      #Able to Proceed to Random Events page
    } else {
      #Tell them that they selected the wrong answer
      #Change the button from Check to Proceed to Event
      #Close the current modal
      
      #Clear EventPage1
      output$EventPage1 <- renderUI(NULL)
      
      #Set up EventPage2
      output$EventPage2 <- renderUI({
        tagList(
          tags$h1("Event"),
          #Show randomly generated question 
          radioButtons(inputId="selectedAns",
                       label=getQuestionStatement(vals$QuestionNo),
                       choices = c("TRUE", "FALSE")),
          tags$b(getQuestionExplanation(vals$QuestionNo)),
          br(),
          tags$b("Answer is Incorrect! Press proceed to continue"),
          actionButton(inputId="proceedbutton", label="Proceed!"))})
      #Able to Proceed to Random Events page    
    }
  })  
  
  observeEvent(input$proceedbutton, {
    ##Randomly choose 1 event,
    vals$event_no <- sample(1:getMaxNumberOfEvents(), 1)
    
    #Should add in parts to modify Hunger, Calories.
    newCalories <- getEventCalories(vals$event_no)
    newHunger <- getEventHunger(vals$event_no)
    
    vals$calories = vals$calories + newCalories
    vals$hunger = vals$hunger + newHunger
    vals$action.log <- add_row(vals$action.log, Event=getEventName(vals$event_no), Calories=newCalories, Hunger=newHunger)
    
    #Clear Page2
    output$EventPage2 <- renderUI(NULL)
    
    #Set up Page3
    output$EventPage3 <- renderUI({
      tagList(
        tags$h2(getEventName(vals$event_no)),
        tags$h2(getEventDescription(vals$event_no)),
        actionButton(inputId="continuebutton", label="Continue")
      )}) 
  }
  )
  
  observeEvent(input$continuebutton,{
    output$EventPage3 <- renderUI(NULL)
    vals$boardstate <- -1
    vals$turndiff <- vals$turndiff -1
  })
  
  ### endModal renders
  output$actionlog <- renderTable(vals$action.log)
  observeEvent(input$goto_leaderboard, updateTabItems(session, "tabSelect", "leaderboard"))
  output$leaderboard_publishedModal <- leaderBoard
  
  ### Publish
  observeEvent(input$publish, showModal(nameModal())) # Get player name
  observeEvent(input$name.done, {
    publishScore(input$name.name, vals$calories)
    showModal(publishedModal())
  })
  
  ### Quit correctly
  observe(if(any(c(input$quit_endModal, input$quit_nameModal, input$quit_publishedModal)>0)) stopApp())
}

shinyApp(ui, server)
