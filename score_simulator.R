
#This function loops and repeatedly calls the function full_drive until someone scores.
#One of the main things this function does is keep track of which team has the ball.
game_simulator <- function(fp, ytg, down, strat4th=NA) {
  team <- 0 #start with team 0
  scoreless <- TRUE
  
  while(scoreless){
    
    result_drive <- full_drive(fp, ytg, down, strat4th)
    
    if(!is.na(result_drive$score)) {
      scoreless <- FALSE
      # break
    }
  }
    
    if(!is.na(result_drive$score)) {
      # print(paste0("team ", team, " scored!")) 
      score[team+1] <- as.numeric(score[team+1]) + result_drive$score
      
      # other team gets the ball
      team <- (team+1)%%2
      
      A <- 75 # assume every kickoff is a touchback
      B <- result_drive$end_time - .2 # assume kickoff takes 12 seconds
      C <- 10
      D <- 1
      F <- ifelse(team == 1, 0, F) # Team 1 is always non-aggressive
      
    } else {
      # give the other team the ball where the previous team left off
      team <- (team+1)%%2 
      
      A <- 100 - result_drive$end_yard
      B <- result_drive$end_time
      C <- 10
      D <- 1
      F <- ifelse(team == 1, 0, F_0)
      
  
  print(paste0("The final score is: Team 0 - ", score[1], ", Team 1 - ",
               score[2]))
  
  return(c("Team_0" = score[[1]], "Team_1" = score[[2]]))
}