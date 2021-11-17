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
          yards_gained = -rweibull(1, Shape = 1.5707, Scale = 5.3136)
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
        yards_gained = -rexp(rate = .482)
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
        yards_gained = -rweibull(1, Shape = 1.7498, Scale = 6.6857)
      } else {
        yards_gained = 0 
      }
      
    } else if (play_type == "R") {
      int <- sample.int(2, size = 1, 
                        prob = c(.793, .207))
      pos_neg <- c("POS", "NEG")[int]
      if (pos_neg == "POS") {
        yards_gained = rweibull(1, Shape = 1.014, Scale = 6.905)
      } else if (pos_neg == "NEG") {
        yards_gained = -rexp(rate = .410)
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
        yards_gained = -rexp(rate = .482)
      }
      
    }
    
  }
}

### 4th down
if (down == 4) {
  # If in FG range, determine scenario
  if (fp < 65) {
    
    # Add punt distributions (this is a placeholder)
    punt_yards <- rnorm(1, 40.5, 9.7)
    drive_result$end_yard <- ifelse(fp - punt_yards <= 0,
                                    25,
                                    fp - punt_yards)
    drive_result$event <- "Punt"
    is_turnover <- TRUE
    
  }
  if (fp >= 65) {
  # Always kick field goal
    if (fds == 0) {
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
    }
    else if (fds == 1) {
      
      # Add run/pass distributions (this is a placeholder)
      yards_gained <- rnorm(1, 3.5, 1)
      
    }
  } 
}

fp = fp - yards_gained
ytg = ytg - yards_gained
down = ifelse(ytg <= 0, 1, down + 1)

if (fp >= 100) {
  drive_result$score <- 7
  drive_result$event <- "Touchdown"
  print("Touchdown")
  is_not_done <- FALSE
}

if (down == 1) { #if first down, reset YTG to 10
  ytg = 10
}

if (down == 5) { # turnover on downs
    drive_result$end_yard <- A
    drive_result$event <- "Turnover on Downs"
    print("TOD")
    is_turnover <- TRUE
  }