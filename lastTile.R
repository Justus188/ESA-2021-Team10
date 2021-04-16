### Last Tile v0.2

### Packages:
for (pkg in c("shiny", "DBI", "shinydashboard")) library(pkg, character.only=T)

### Functions:
## AWS Functions:
getAWSConnection <- function(){
  dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student061",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student061",
    password = "Uuzk84Yf")
}

getQuery <- function(query) {
  conn <- getAWSConnection()
  result <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  result
}

getLeaderBoard <- function() getQuery("SELECT Player, Calories FROM CarelorieLeaderboard ORDER BY calories DESC LIMIT 10")

getRandomPlayerName <- function() getQuery("SELECT * FROM LeaderRandomName")$playername[1]

publishScore <- function(name, calories){
  conn <- getAWSConnection()
  query <- paste0("INSERT INTO CarelorieLeaderboard (Player, Calories) VALUES (?id1, ", calories, ")")
  query <- sqlInterpolate(conn, query, id1=name)
  print(query)
  
  nrows <- dbExecute(conn, query)
  if(nrows!=1) print("writeLeaderBoard() inserted != 1 row. Something went wrong.")
  
  dbDisconnect(conn)
}

## Modals:
endModal <- function() {modalDialog(
  title = "The End!",
  "Congratulations! Here is a summary of what you ate this run!",
  tableOutput("actionlog"),
  "Here's how other players did!",
  tableOutput("leaderboard_endModal"),
  footer = tagList(
    actionButton("quit_endModal", "Quit"),
    actionButton("start_endModal", "Restart"),
    actionButton("publish", "Publish Your Score!")
  )
)}

nameModal <- function(){modalDialog(
  title = "Input your name!",
  textInput("name.name", "Your name is: ", getRandomPlayerName()),
  "(You can change it!)",
  footer= tagList(
    actionButton("quit_nameModal", "Quit"),
    actionButton("name.done", "That's my name!")
  )
)}

publishedModal <- function(){modalDialog(
  title = "Your score has been published!",
  "Your score has been published! Do you see it in the leaderboard?",
  tableOutput("leaderboard,"),
  footer = tagList(
    actionButton("quit_publishedModal", "Quit"),
    actionButton("start_publishedModal", "Restart")
  )
)}