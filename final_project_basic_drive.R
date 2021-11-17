fp <- 25
ytg <- 10
down <- 1
strat4th <- 0
pos <- 0

full_drive <- function(fp, ytg, down, strat4th, pos){
  
  # fp is starting field position of play (ranges from 0 to 100 (so your own 25 would be 25. 100 would be a touchdown))
  # ytg is yards to first down
  # down is down number
  
  is_not_done <- TRUE
  pos <- pos # start with "our team" on offense
  # k <- 0 #this allows us to gracefully kick out of a bad while loop. Not needed in production
  # #but in case you miscode something while running a while loop, it's always good to not have to kill the kernel.
  # 
  drive_result <- list(event = NA, score = NA, end_yard = NA, pos = pos)
  
  while (is_not_done) {
    ### 1st or 2nd down
    if (down %in% c(1,2)) {
      
      ### If in back redzone
      if (fp <= 20) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.439, .561))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.594, .328, .078))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.4612, rate = 0.1145) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.5707, scale = 5.3136)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.809, .191))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.4612, sdlog = .1145) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
        
      } else if (between(fp, 20, 80)) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.507, .493))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.567, .362, .071))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.533, rate = 0.1178)
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.532, scale = 5.516)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.813, .187))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.509, sdlog = .895) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
        
      } else if (fp > 80) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.371, .629))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.487, .449, .064))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = ifelse(
              ytg <= 3,
              rlnorm(1, meanlog = 1.092, sdlog = .82),
              rgamma(1, shape = 2.603, rate = 0.301)
            )
          } else if (pos_neg == "NEG") {
            yards_gained = -rgamma(1, shape = 1.992, rate = .4133)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.813, .187))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.509, sdlog = .895) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
      }
    }
    
    ### 3rd down
    if (down == 3) {
      
      ### If in back redzone
      if (fp <= 20) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.747, .253))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.488, .405, .106))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.676, rate = 0.1212) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.644, scale = 5.988)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.815, .185))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.2056, rate = 0.1612) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rlnorm(1, meanlog = .7313, sdlog = .6045)
          }
          
        }
        
        
      } else if (between(fp, 20, 80)) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.708, .292))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.5107, .39, .0989))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.7538, rate = .1367) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.7498, scale = 6.6857)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.793, .207))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rweibull(1, shape = 1.014, scale = 6.905)
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .410)
          }
          
        }
        
        
      } else if (fp > 80) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.601, .399))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.452, .473, .075))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = ifelse(
              ytg <= 3,
              rlnorm(1, meanlog = 1.383, sdlog = .8102),
              rgamma(1, shape = 2.643, rate = 0.310)
            ) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rlnorm(1, meanlog = 1.5396, sdlog = .7784)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.762, .238))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.1019, sdlog = .818) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
      }
    }
    
    ### 4th down
    if (down == 4) {
      if (fp < 65) {
        print("Out of FG range")
        # Add punt distributions (this is a placeholder)
        punt_yards <- rnorm(1, 40.5, 9.7)
        print(punt_yards)
        drive_result$end_yard <- ifelse(fp + punt_yards >= 100,
                                        25,
                                        fp + punt_yards)
        drive_result$event <- "Punt"
        print(is_not_done)
        is_not_done <- FALSE
        break
        print(paste("is_not_done is now", is_not_done))
        
      }
      else if (fp >= 65) {
        # Always kick field goal
        if (strat4th == 0) {
          prob <- field_goal_probability(fp)
          field_goal <- sample.int(2, size = 1, prob = c(prob, 1 - prob))
          if (field_goal == 1) {
            drive_result$score <- 3
            drive_result$event <- "FG"
          } else {
            drive_result$score <- 0
            drive_result$end_yard <- fp
            drive_result$event <- "Missed FG"
          }
          is_not_done <- FALSE
          break
        }
        else if (strat4th == 1) {
          
          # Add run/pass distributions (this is a placeholder)
          yards_gained <- rnorm(1, 3.5, 1)
          
        }
      } 
    }
    
    # fp = fp + yards_gained
    # print(paste("New fp is:", fp))
    # ytg = ytg - yards_gained
    # print(paste("New ytg is:", ytg))
    # down = ifelse(ytg <= 0, 1, down + 1)
    # print(paste("New down:", down))
    
    if (fp >= 100) {
      drive_result$score <- 7
      drive_result$event <- "Touchdown"
      print("Touchdown")
      is_not_done <- FALSE
      break
    }
    
    if (down == 1) { #if first down, reset YTG to 10
      ytg = 10
      print("First down")
    }
    
    if (down == 5) { # turnover on downs
      drive_result$end_yard <- fp
      drive_result$event <- "Turnover on Downs"
      print("TOD")
      is_not_done <- FALSE
    }
        
  }
  
  print(drive_result)
}
