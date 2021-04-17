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

getLeaderBoard <- function() getQuery("SELECT Player, Calories FROM CarelorieLeaderboard ORDER BY calories ASC LIMIT 10")

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
  "Congratulations! Here is a summary of what you did this run!",
  tableOutput("actionlog"),
  footer = tagList(
    actionButton("quit_endModal", "Quit"),
    actionButton("start_endModal", "Restart"),
    actionButton("goto_leaderboard", "Check out the Leaderboard!")
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

### Helper Functions
createTile <- function(j, i=0) return(imageOutput(paste0("cell", i, j), height="50px", width = "50px", inline = T))
createRow <- function(i) return(lapply(0:9, createTile, i = i))
genCellIds <- function(){
  initIds = as.character(0:99)
  initIds[1:10] <- paste0("0", initIds[1:10])
  initIds
}
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
        imgsrc <- "www/eventTilepic.png"
      }
    }
    # Unfortunately, we are not able to re-size the image and still have the click event work.
    # So the image files must have exactly the size we want.
    # Also, z-order works only if 'position' is set.
    list(src=imgsrc,style="position:relative;z-order:999;")
  },deleteFile=FALSE)
}