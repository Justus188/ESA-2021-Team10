#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

getallMenu <- function(){
  #extract a df of all menu items
  
  restaurantmenu <- data.frame(menuitem = c("Choose Something","fish","chicken","beef","None"),calories = c(NA,100,200,300,0), hungereffect = c(NA,1,1,1,1))
  restaurantmenu
}

getMenu <- function(){
  # Extract random menu from database
  
  # output will be dataframe containing menuitem,calories,hungereffect and append a none option and null option, and taken from a random food type of the total menu
  #test menu
  restaurantmenu <- data.frame(menuitem = c("Choose Something","fish","chicken","beef","None"),calories = c(NA,100,200,300,0), hungereffect = c(NA,1,1,1,1))
  restaurantmenu
}


starvingModal <- function(){
  modalDialog(
    title = "You are starving. You have no choice but to stuff your face with food.",
    footer = tagList(
      actionButton("starvingok", "OK")
   )
  )
}


menuModal <- function(failed = FALSE){
  modalDialog(
    title = "Menu",
    selectInput("menuitem","Select Menu item:",getMenu()[,1]),
    if (failed)
      div(tags$b("Please choose an item", style = "color: red;")),
    footer = tagList(
      actionButton("menuok", "OK")
   )
  )
}

checktile <- function(row,col){
  tile <- -1
  playerpos <- paste(row,col)
  if (playerpos %in% eventlist) {tile <- 0} else if (playerpos == paste(1,1)) {tile <- 2} else {tile <- 1}
  #tile 0 means event, tile 1 means restaurant, tile 2 means end game
  tile
}


getSingleStepLocation <- function(gridrow,gridcol,gridsize){
  
  # We assume that the direction is clockwise
  newgridrow <- gridrow
  newgridcol <- gridcol
  if (gridrow==gridsize){
    if (gridcol>1){
      newgridcol <- gridcol-1
    } else newgridrow <- gridrow-1
  }else{if (gridrow==1){
    if (gridcol<gridsize){
      newgridcol <- gridcol+1
    } else newgridrow <- gridrow+1
  }else {# gridrow is neither 1 nor GRIDSIZE
    if (gridcol==gridsize){
      newgridrow <- gridrow+1
    } else { # Assume gridcol==1
      newgridrow <- gridrow-1
    }
  }
  }
  newlocation <- list(row=newgridrow,col=newgridcol)
  newlocation
}



updateBoardState <- function(pieces,dieNumber,gridsize){
  # For each game variant, there is exactly one piece of each color (red and blue) on the board.
  # Find the cell with the piece whose turn is next
  #targetcontent <- CELLRED
  #misscontent <- CELLBLUE
  #if (playerturn==BLUETURN) {
  #  targetcontent <- CELLBLUE
  #  misscontent <- CELLRED
  #}
  locationindex <- which(pieces == 2)
  gridcol <- as.integer((locationindex-1)/gridsize)+1
  gridrow <- locationindex - (gridcol-1)*gridsize
  # Now we know the gridrow and gridcol where the piece is located
  # Remove the piece from that location
  pieces[gridrow,gridcol] <- 1
  newlocation <- list(row=gridrow,col=gridcol)
    # MONOPOLY
  while (dieNumber>0) {
    newlocation <- getSingleStepLocation(newlocation$row,newlocation$col)
    dieNumber <- dieNumber -1
    }
  # Update the newlocation with the pieces that are there
  gridrow <- newlocation$row
  gridcol <- newlocation$col
  pieces[gridrow,gridcol] <- 2
  # Return the pieces matrix
  pieces
}

getImageId <- function(gridrow,gridcol,vals){
  imageid <- vals$pieces[gridrow,gridcol]
  imageid
}
getImageStyle <- function(gridrow,gridcol,vals){
  imgstyle <- "border: 2px solid blue;"
  # If the cell should be highlighted draw a blue border around it
  #if (boardstate$highlights[gridrow,gridcol])imgstyle <- "border: 2px solid blue;"
  imgstyle
}
