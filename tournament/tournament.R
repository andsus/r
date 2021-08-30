tournament <- function(input) {
  # structure using data.frame
  df <- function(team, win = 0, loss = 0, draw = 0) {
    data.frame(Team = team, W = win, L = loss, D = draw)
  }
  
  # initialize the data.frame with a dummy team
  standings <- df("__scratch__", 0, 0, 0)
  
  # and then remove the scratch team from the data.frame
  standings <- standings[standings$Team != "__scratch__", ]
  
  for (record in strsplit(input, ";")) {
    if (length(record) != 3) next
    
    teamA   <- record[1]
    teamB   <- record[2]
    result <- record[3]
    
    if (result == "win") {
      standings <- rbind(standings, df(teamA, win  = 1),
                         df(teamB, loss = 1))
    }
    if (result == "loss") {
      standings <- rbind(standings, df(teamA, loss = 1),
                         df(teamB, win  = 1))
    }
    if (result == "draw") {
      standings <- rbind(standings, df(teamA, draw = 1),
                         df(teamB, draw = 1))
    }
  }
  
  # sum up all the individual items in the standings
  standings <- aggregate(. ~ Team, standings, sum)
  
  # calculate the MP and P columns
  attach(standings)
  standings$MP <- W + D + L
  standings$P  <- 3 * W + D
  detach(standings)
  
  # sort
  standings <- standings[with(standings, order(-P, Team)), ]
  
  # reset row.names
  row.names(standings) <- NULL
  
  # return, setting the desired column order
  standings[, c("Team", "MP", "W", "D", "L", "P")]
}

