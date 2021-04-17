library(shiny)
library(shinybusy)
library(tidyverse)

source("lastTile.R")

getMaxNumberOfEvents <- function(){
  query <- "SELECT MAX(EventNumber) FROM CarelorieEvents"
  result <- getQuery(query)
  as.numeric(result[[1]])
}

getEventName <- function(event_no){
  query <- str_c("SELECT EventName FROM CarelorieEvents WHERE EventNumber=",event_no)
  result <- getQuery(query)
  as.character(result[[1]])
}

getEventDescription <- function(event_no){
  query <- str_c("SELECT EventDescription FROM CarelorieEvents WHERE EventNumber=",event_no)
  result <- getQuery(query)
  as.character(result[[1]])
}

getEventType <- function(event_no){
  query <- str_c("SELECT EventType FROM CarelorieEvents WHERE EventNumber=", event_no)
  result <- getQuery(query)
  as.numeric(result[[1]])
}

getRandQuestionNo <- function(){
  query <- "SELECT QuestionNo FROM CarelorieQuestions ORDER BY RAND() LIMIT 1"
  result <- getQuery(query)

  #Return the result
  as.numeric(result[[1]])
}


getQuestionStatement <- function(qn_no){
  query <- str_c("SELECT QuestionStatement FROM CarelorieQuestions WHERE QuestionNo=", qn_no)
  result <- getQuery(query)
  
  as.character(result[[1]])
}


checkAnswer <- function(qn_no,selected_ans){
  query <- str_c("SELECT QuestionAnswer FROM CarelorieQuestions WHERE QuestionNo=", qn_no)
  result <- getQuery(query)
  result <- as.character(result[[1]])

  if (selected_ans==result){
    print("Check Result: Correct")
    TRUE
  }else if (selected_ans!=result){
    print("Check Result: Wrong")
    FALSE
  }
}


getQuestionExplanation <- function(qn_no){
  query <- str_c("SELECT QuestionExplanation FROM CarelorieQuestions WHERE QuestionNo=", qn_no)
  
  result <- getQuery(query)
  
  as.character(result[[1]])
}

getEventCalories <- function(event_no){
  query <- str_c("SELECT Calories FROM CarelorieEvents WHERE EventNumber=", event_no)
  
  result <- getQuery(query)
  
  as.numeric(result[[1]])
}

getEventHunger <- function(event_no){
  query <- str_c("SELECT Hunger FROM CarelorieEvents WHERE EventNumber=", event_no)
  
  result <- getQuery(query)
  
  as.numeric(result[[1]])
}

getEventType <- function(event_no){
  query <- str_c("SELECT EventType FROM CarelorieEvents WHERE EventNumber=", event_no)
  
  result <- getQuery(query)
  
  as.numeric(result[[1]])
}