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
library(tidyverse)

source("lastTile.R")
getallMenu <- function(){
  #extract a df of all menu items
  query <- "SELECT * FROM CarelorieMenu"
  getQuery(query)
}



getMenu <- function(){
  # Extract random menu from database
  allmenu <- getallMenu()
  foodtype <- unique(allmenu$Foodtype)
  selectedfoodtype <- sample(foodtype,1)
  query <- paste0("SELECT * FROM CarelorieMenu WHERE Foodtype = '" ,selectedfoodtype,"' ORDER BY RAND() LIMIT 5")
  selectedmenu <- getQuery(query)
  
  selectedmenu <- rbind(selectedmenu,c(0,"Nothing","Nothing",0,0,"Blank.png"))
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

checktile <- function(row, col, isEvent) if (isEvent[row+1, col+1]) 0 else if (row==8 && col == 9) 2 else 1


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



updateBoardState <- function(playerpos,dieNumber){
  row <- playerpos[1]
  col <- playerpos[2]

  if (row == 9 && col != 0){
    col <- col - dieNumber
    if (col < 0) {row <- row + col; col <- 0}
  } else if (col == 0 && row != 0) {
    row <- row - dieNumber
    if (row<0) {col <- col - row; row <- 0}
  } else if (row == 0 && col != 9){
    col <- col + dieNumber
    if (col > 9) {row <- row + col - 9; col <- 9}
  } else if(col == 9) {
    row <- row + dieNumber
    if (row > 8) {row <- 8}
  } else stop("DiceError: Something went wrong in restaurant+movement.R/updateBoardState")
  
  c(row, col)
}

getTokenSrc <- function(input) return(paste0("token", switch(input$playertoken, "Burger"=1, "Fries"=2, "Apple"=3), ".png"))
