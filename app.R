source("lastTile.R")
source("event.R")
source("restaurant+movement.R")
library(shiny)
library(shinydashboard)

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
                     tags$p(HTML('Objective: To go around the gameboard making food choices that you think will give you the lowest calorie count and at the same time learning more about nutrition by answering random nutritional-related questions. 
<br/>Instructions:
<br/>1. Get ready at starting position
<br/>2. Roll the dice on the right side of the screen to move forward
<br/>3. If you land on a restaurant tile (yellow tile) :
<br/>The menu of a random restaurant will appear on the right side, you can browse through the menu, look through the main ingredients and filling level of each food and make a choice to either pick a food you think has the lowest calorie on the menu to eat or not eat anything.
<br/>4. When you consume a food item, your hunger level will decrease but if you choose not to consume anything, your hunger level will increase. If your hunger level reaches too high (as indicated by the red portion on the hunger meter on the top right-hand corner of the gameboard), you will be forced to eat unhealthy food which will result in a large increase in your calorie count!
<br/>5. If you land on an event tile (blue tile) :
<br/>You will have to answer a nutritional-related question. If you answer correctly, a random "good" event will occur, which will help you to lower your total calorie count, however, if you answer incorrectly, a random "bad" event will occur, which will result in an increase in your total calorie count.
<br/>6. Try to reach the endpoint with the lowest calorie count!')),
                     actionButton("start_welcome", "Start the Game!")
             ),
             tabItem(tabName = "gameboard",
                     h2("CARElorie"),
                     h4("Start the game by clicking the die on the right"),
                     sidebarLayout( 
                       # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
                       # Then, to superimpose images, style them to be 'position:relative;z-order:999'
                       mainPanel(width=7,
                                 img(src='board.png',style="position:absolute;z-order:0",width="525px",height="500px"),
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
                       sidebarPanel( width = 5,
                                     "Hunger Meter:",
                                     imageOutput("hunger_scale", height = "100px", width = "100px"),
                                     # only show this panel if player is rolling the die
                                     conditionalPanel(
                                       condition = "output.dice == true",
                                       "Click on the dice to roll for your movement this turn!",
                                       tags$br(),
                                       imageOutput("die",height="100px",width="100px",click=clickOpts("clickdie", clip=F),inline=TRUE)
                                     ),
                                     
                                     # Only show this panel if land on restaurant tile
                                     conditionalPanel(
                                       condition = "output.restaurant == true",
                                       tags$h1("Menu"),
                                       uiOutput("choosemenu"),
                                       uiOutput("foodimg"),
                                       uiOutput("foodingredients"),
                                       uiOutput("fillinglevel"),
                                       actionButton("choosefood_yes","ok")
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
                     conditionalPanel(
                       condition="output.recent_game_toggle ==true",
                       box(
                         "Your most recent score is",
                         uiOutput("recent_game"),
                         actionButton("publish", "Publish Your Score!")
                       )
                     ),
                     conditionalPanel(
                       condition="output.recent_publish_toggle ==true",
                       box(textOutput("pubilish_success"))
                     ),
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
  ### Init variables ###########################################################
  # Static variables
  allmenu <- rbind(getallMenu(),c(-1,"Nothing","Your weighing scale weighs heavily on your mind...",0,0,"Blank.png","Air","A tiny bit")) #df with all menu items
  cellIds <- genCellIds()
  listofcells <- lapply(cellIds, function(x) return(paste0("cell", x)))
  
  # Reactive variables
  vals <- reactiveValues(calories = 0,            ### Game variables, generated on start()
                         hunger = 2000,
                         dieNumber = 6,
                         boardstate = -1,
                         isEvent = NA, 
                         playerpos = c(9, 9),     # (row,col) track token location; each edge has length 10 starting from (9,9), ends(8,9)
                         QuestionNo = NULL,
                         event_no = NULL,
                         action.log = NA,
                         ### Trackers for conditional statements
                         turndiff = 0,            # Protection from click spam - tracks sync between movement and action phases
                         startbuttons = rep(0,3), # Tracks latest logged start button states - to identify which button is pressed
                         in_progress = F,         # Tracks whether game is in progress
                         recent_score = NA,
                         recent_publish = NA
          )
  
  ### RENDERING FUNCTIONS ######################################################
  # Code for displaying of the playertoken image in the mainpage, update & store the chosen token image for the rest of the game in line code 148 (after pressing start button)
  output$playertokenimg <- renderUI(img(src = getTokenSrc(input), height=50, width=50))
  
  observeEvent(c(vals$playerpos, vals$isEvent),{
    mapply(function(x, y) {output[[x]] <- renderCell(vals$playerpos, y, input, vals$isEvent)}, x=listofcells, y=cellIds)
  })
  
  output$hunger_scale <- renderImage(list(src = paste0("www/hunger", as.integer(7-vals$hunger/1000), ".png")), deleteFile=F)
  output$die <- renderImage({
    #select the icon appropriate for this die
    imageid <- vals$dieNumber
    imgsrc=switch(imageid,"www/Die1.png","www/Die2.png","www/Die3.png","www/Die4.png","www/Die5.png","www/Die6.png")
    # Unfortunately, we are not able to re-size the image and still have the click event work.
    # So the image files must have exactly the size we want.
    # Also, z-order works only if 'position' is set.
    list(src=imgsrc,style="position:relative;z-order:999;")
  },deleteFile=FALSE)
  
  output$leaderboard <- renderTable({input$name.done; getLeaderBoard()})
  
  ### GAME LOGIC ###############################################################
  observeEvent(vals$in_progress, { # Hide gameboard if game not in progress
    if(vals$in_progress) output$gameboard_gated <- renderMenu(menuItem("Gameboard", tabName = "gameboard", icon = icon("chess-board")))
    else output$gameboard_gated <- NULL
  })
  
  observe({ # Start game
    curr_start_buttons <- c(input$start_welcome, input$start_leaderboard, ifelse(is.null(input$start_endModal), 0, input$start_endModal))
    # endModal is only initialised later, ifelse is needed to maintain list length at 3 
    
    if (any(curr_start_buttons > vals$startbuttons)) {               # To prevent misfire when any button is initialised
      removeModal()                                                  # Remove end of game summary modal if applicable
      
      vals$in_progress <- T                                          # Set game to be in progress
      
      # (Re)Initialise game variables
      vals$isEvent <- matrix(runif(100, 0,1) <0.2, byrow=T, nrow=10) # isEvent[row+1, col+1] gives bool of whether tile isEvent
      vals$isEvent[9:10,10] <- F                                     # Don't allow start/endpoint to be event
      vals$calories <- 0
      vals$hunger <- 2000
      vals$dieNumber <- 6
      vals$boardstate <- -1
      vals$turndiff <- 0
      vals$playerpos <- c(9, 9)
      vals$action.log <- data.frame(Event="Start", Calories=0, Hunger=2000)
      
      updateTabItems(session, "tabSelect", "gameboard") # Shift active window to gameboard
      # print("Starting game") ###DEBUG
    }
    
    vals$startbuttons <- curr_start_buttons
  })
  
  observeEvent(input$clickdie,{               # When die is clicked
    if (vals$turndiff == 0){
      vals$turndiff = vals$turndiff +1        # Prevents spam clicking causing multiple move phases before one action phase
      
      vals$dieNumber = as.integer(runif(1,1,7))
      vals$playerpos <- updateBoardState(vals$playerpos,vals$dieNumber)
      
      vals$hunger <- vals$hunger - 100*vals$dieNumber - 100
      vals$action.log <- add_row(vals$action.log, Event=paste("Travelled", vals$dieNumber, "tiles"), Calories = 0, Hunger = -100*vals$dieNumber)
      
      if (vals$hunger<0) {                     # Check for starving
        showModal(starvingModal())
        vals$calories <- vals$calories + 1500
        vals$hunger <- vals$hunger + 1000
        vals$action.log <- add_row(vals$action.log, Event="Binge ate due to hunger", Calories=1500, Hunger=1000)
      }
      
      currentrow <- vals$playerpos[1]
      currentcol <- vals$playerpos[2]
      
      # print(paste0("Die rolled: ", vals$dieNumber, ", Player Pos: ", vals$playerpos[1], vals$playerpos[2])) ### DEBUG
      
      
      vals$boardstate <- checktile(currentrow, currentcol, vals$isEvent) # Boardstates: -1 Dice, 0 Event, 1 Restaurant, 2 End
      if(vals$boardstate == 1) vals$menu <- getMenu() # Generate random menu
      if(vals$boardstate ==2) {
        vals$action.log <- add_row(vals$action.log, Event="Total", Calories=vals$calories, Hunger=vals$hunger) # Add final tally
        vals$recent_score <- vals$calories                                                                     # Activate publish button
        showModal(endModal())                                                                                  # Show game summary
      }
    }
  })
  
  observeEvent(input$starvingok, removeModal())
  
  # Conditional Panels for action phase
  output$dice <- reactive(vals$boardstate == -1)
  output$event <- reactive(vals$boardstate == 0)
  output$restaurant <- reactive(vals$boardstate == 1)
  
  # Conditional Panels for leaderboard tab
  output$recent_game_toggle <- reactive(!is.na(vals$recent_score))
  output$recent_game <- renderUI(h1(vals$recent_score))
  
  output$recent_publish_toggle <- reactive(!is.na(vals$recent_publish))
  output$publish_success <- renderText(paste("Successfully published", vals$recent_publish, "to leaderboard!"))
  
  # Keep them reactive even when hidden
  for (outputpanel in c("dice", "event", "restaurant", "recent_game_toggle", "recent_publish_toggle")) outputOptions(output, outputpanel, suspendWhenHidden=F)
  
  ### Restaurant Logic
  output$choosemenu <- renderUI({
    selectInput(
      "Chosenfood",
      vals$menu[1, "Foodtype"],
      vals$menu[,"Food"]
    )
  })
  
  output$foodimg <- renderUI(img(src = allmenu[allmenu$Food == input$Chosenfood,"FoodImage"],height = 200,width = 200))
  
  output$foodingredients <- renderUI(HTML(paste(strong("Main ingredients:"),
                                                str_to_title(allmenu[allmenu$Food == input$Chosenfood,"Ingredients"]))))
  
  output$fillinglevel <- renderUI(HTML(paste(strong("Filling level :"),
                                             str_to_title(allmenu[allmenu$Food == input$Chosenfood,"Filling"]))))
  
  observeEvent(input$choosefood_yes,{ # Choose food -> update stats
    newCalories <- as.integer(allmenu[allmenu$Food == input$Chosenfood,"Calories"])
    newHunger <- as.integer(allmenu[allmenu$Food == input$Chosenfood,"Hunger"])
    
    vals$calories <- vals$calories + newCalories
    vals$hunger <- vals$hunger + newHunger
    vals$action.log <- add_row(vals$action.log, Event=paste("Consumed", input$Chosenfood), Calories=newCalories, Hunger=newHunger)
    
    vals$turndiff <- vals$turndiff -1 # End of action phase
    vals$boardstate <- -1             # Set state to movement
  })
  
  ### Event logic
  observeEvent(vals$boardstate,{if (vals$boardstate==0){
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
    outcome <- checkAnswer(vals$QuestionNo, input$selectedAns) # Check the user's ans
    # print(paste0("QuestionNo: ", vals$QuestionNo,", SelectedAns: ", input$selectedAns,", Outcome: ",outcome)) ###DEBUG
    
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
          getQuestionExplanation(vals$QuestionNo),
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
          getQuestionExplanation(vals$QuestionNo),
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
    
    vals$calories = vals$calories + newCalories
    vals$action.log <- add_row(vals$action.log, Event=getEventName(vals$event_no), Calories=newCalories, Hunger=0)
    
    #Clear Page2
    output$EventPage2 <- renderUI(NULL)
    
    #Set up Page3
    output$EventPage3 <- renderUI({
      tagList(
        tags$h2(str_to_title(getEventName(vals$event_no))),
        tags$h4(getEventDescription(vals$event_no)),
        actionButton(inputId="continuebutton", label="Continue")
      )}) 
  })
  
  observeEvent(input$continuebutton,{
    output$EventPage3 <- renderUI(NULL)
    vals$boardstate <- -1
    vals$turndiff <- vals$turndiff -1
  })
  
  ### End of game
  output$actionlog <- renderTable(vals$action.log)       # Render summary
  observeEvent(input$goto_leaderboard, {                 
    removeModal()                                        # Clear summary modal
    vals$in_progress <- F                                # Game is over - hide the gameboard
    updateTabItems(session, "tabSelect", "leaderboard")  # Move to leaderboard tab
  })

  ### Publish
  observeEvent(input$publish, showModal(nameModal()))    # Get player name
  
  observeEvent(input$name.done, {
    removeModal()
    publishScore(input$name.name, vals$calories)         # We aren't checking for repeats because there isn't a password / name booking system
                                                         # The idea is oldschool arcade style leaderboards
    vals$recent_score <- NA                              # No double publishing the same game
    vals$recent_publish <- input$name.name               # Update success confirmation
    updateTabItems(session, "tabSelect", "leaderboard")  # Bring them back to the leaderboard
  })
  
  ### Quit via button
  observe(if(any(c(input$quit_endModal, input$quit_nameModal, input$quit_publishedModal)>0)) stopApp())
}

shinyApp(ui, server)
