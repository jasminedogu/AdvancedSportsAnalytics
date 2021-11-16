play <- function() {
  rnorm(1, 3.5, 1)
}

### posession function

full_drive <- function(fp, ytg, down){
  
  # fp is starting field position of play (ranges from 0 to 100 (so your own 25 would be 25. 100 would be a touchdown))
  # C is yards to first down
  # D is down number
  
  is_not_done <- TRUE
  k <- 0 #this allows us to gracefully kick out of a bad while loop. Not needed in production
  #but in case you miscode something while running a while loop, it's always good to not have to kill the kernel.
  
  drive_result <- list(event = NA, score = NA, end_yard = NA)
  
  while (is_not_done == TRUE) {
    
    yards_gained <- play()
        
        fp = fp + yards_gained
        ytg = ytg - yards_gained
        down = ifelse(ytg <= 0, 1, down + 1)
        
        if (down == 1) { #if first down, reset YTG to 10
          ytg = 10
        }
        
        k <- k + 1
        
      if (down == 4) {  
        if (fp >= 70) { #if within 30 yards of end zone, simulate field goal
          prob <- field_goal_probability(fp)
          field_goal <- sample.int(2, size = 1, prob = c(prob, 1 - prob))
          if (field_goal == 1) {
            drive_result$score <- 3
            drive_result$event <- "FG"
          } else {
            drive_result$end_yard <- fp
            drive_result$event <- "Missed FG"
          }
        } else { #or else simulate punt
          punt_yards <- rnorm(1, 40.5, 9.7)
          drive_result$end_yard <- ifelse(fp + punt_yards <= 0,
                                          25,
                                          fp + punt_yards)
          drive_result$event <- "Punt"
        }
          is_not_done <- FALSE
        }
        
        # if (D == 5) { # turnover on downs
        #   drive_result$end_yard <- A
        #   drive_result$end_time <- B
        #   drive_result$event <- "Turnover on Downs"
        #   print("TOD")
        #   is_not_done <- FALSE
        # } 
        # 
        # if (D == 0) { # interception
        #   drive_result$end_yard <- A
        #   drive_result$end_time <- B
        #   drive_result$event <- "Interception"
        #   print("Interception")
        #   is_not_done <- FALSE
        # }
        
     if (fp >= 100) {
        drive_result$score <- 7
        drive_result$event <- "Touchdown"
        print("Touchdown")
        is_not_done <- FALSE
      }
      
      if(k > 100){
        drive_result$end_yard <- fp
        is_not_done <- FALSE
      }
        
  }
  
  print(drive_result)
}
