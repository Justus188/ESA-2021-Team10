library(shiny)
library(shinybusy)
library(DBI)
library(tidyverse)

source("lastTile.R")
# source("setAWSPassword.R") #5FqGmSBe
# getOption("AWSPassword")
# 
# getAWSConnection <- function(){
#   conn <- dbConnect(
#     drv = RMySQL::MySQL(),
#     dbname = "student081",
#     host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
#     username = "student081",
#     password = getOption("AWSPassword"))
#   conn
# }


getMaxNumberOfEvents <- function(){
  query <- "SELECT EventNumber FROM CarelorieEvents ORDER BY EventNumber DESC LIMIT 1"
  result <- getQuery(query)
  result[[1]]
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

getRandQuestionNo <- function(){
  query <- "SELECT QuestionNo FROM CarelorieQuestions ORDER BY RAND() LIMIT 1"
  result <- getQuery(query)
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


QuestionModal <- function(correct = FALSE, wrong=FALSE, question_no){
  modalDialog(
    #Show randomly generated question  
    title = "Event",
    radioButtons(inputId="selectedAns",
    label=getQuestionStatement(question_no),
    choices = c("TRUE", "FALSE")),
   
  if (correct==TRUE) {div(tags$b("Answer is Correct! Press proceed to continue"))},
  
  if (wrong==TRUE) {div(tags$b("Answer is Incorrect! Press proceed to continue"))},
    
  footer = tagList(
    if(wrong==FALSE & correct==FALSE){actionButton(inputId="checkbutton", label="Check!")}else{actionButton(inputId="proceedbutton", label="Proceed!")}
)
)
}


EventsModal <- function(event_no){
  event_name <- getEventName(event_no)
  event_desc <- getEventDescription(event_no)
  image_name <- str_c('Event',event_no,'.jpg')
  modalDialog(
      title = event_name,
      tags$h2(event_desc),
      img(src=image_name,width="200px",height="200px"),
      
      footer = tagList(
    actionButton(inputId="continuebutton", label="Continue")
      )
  )
}