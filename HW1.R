library(tidyverse)

setwd("~/Fall 2021 Courses/STAT 4800")

data <- read.csv("2019 PFF All Plays.csv")

# Create empirical expected points model
data %>%
  mutate(
    # Re-configure field position - (0 to 100 where 100 is touchdown)
    FIELDPOSITION = case_when(
      pff_FIELDPOSITION < 0 ~ pff_FIELDPOSITION * -1,
      pff_FIELDPOSITION > 0 ~ 100 - pff_FIELDPOSITION
    ),
    # Create distance (YTG for first down) bins
    DISTANCE_BIN = case_when(
      pff_DISTANCE >= 8 ~ "Long",
      between(pff_DISTANCE, 5, 7) ~ "Med",
      between(pff_DISTANCE, 3, 4) ~ "Short",
      pff_DISTANCE < 3 ~ "Shortest"
    ),
    # Create field position bins
    # Before crossing midfield, teams are focused on getting first downs, rather than scoring touchdowns or field goals.
    # Once teams pass the 50 yard line, they become more focused on scoring points, so game strategy may change. We split up the Redzone, 
    # because once teams are within 10 yards of a touchdown, game strategies may change in order to score a touchdown rather than a field goal. 
    FIELDPOSITION_BIN = case_when(
      between(FIELDPOSITION, 0, 20) ~ "Own Side - Far",
      between(FIELDPOSITION, 21, 50) ~ "Own Side",
      between(FIELDPOSITION, 51, 79) ~ "Opponent Side",
      between(FIELDPOSITION, 80, 89) ~ "Redzone - Far",
      FIELDPOSITION >= 90 ~ "Redzone - Close"
    ),
    # Calculate change in score
    POINTS_SCORED = case_when(
      # pff_DRIVEPLAY == pff_DRIVEENDPLAYNUMBER ~ lead(pff_DEFSCORE) - pff_OFFSCORE,
      # TRUE ~ lead(pff_OFFSCORE) - pff_OFFSCORE
      (pff_DRIVEPLAY == pff_DRIVEENDPLAYNUMBER) & (pff_DRIVEENDEVENT == "FIELD GOAL") ~ 3,
      (pff_DRIVEPLAY == pff_DRIVEENDPLAYNUMBER) & (pff_DRIVEENDEVENT == "FUMBLE-TD") ~ -6,
      (pff_DRIVEPLAY == pff_DRIVEENDPLAYNUMBER) & (pff_DRIVEENDEVENT == "INTERCEPTION-TD") ~ -6,
      (pff_DRIVEPLAY == pff_DRIVEENDPLAYNUMBER) & (pff_DRIVEENDEVENT == "SAFETY") ~ -2,
      (pff_DRIVEPLAY == pff_DRIVEENDPLAYNUMBER) & (pff_DRIVEENDEVENT == "TOUCHDOWN") ~ 6,
      TRUE ~ 0
  )) %>% 
  filter(
    # Remove garbage time, lingering drives, and 2/4 quarters
    pff_GARBAGETIME == 0,
    pff_QUARTER %in% c(1,3),
    # Remove no-score scenarios
    pff_DRIVEENDEVENT %in% c("FIELD GOAL", "FUMBLE-TD", "INTERCEPTION-TD",
                             "SAFETY", "TOUCHDOWN")
  ) %>%
  select(pff_QUARTER, FIELDPOSITION, FIELDPOSITION_BIN, pff_DISTANCE, DISTANCE_BIN,
         pff_DRIVEPLAY, pff_DRIVEENDPLAYNUMBER, pff_DRIVEENDEVENT, pff_DOWN,
         POINTS_SCORED) %>%
  group_by(pff_DOWN, FIELDPOSITION_BIN, DISTANCE_BIN) %>%
  summarize(expected_points = mean(POINTS_SCORED)) -> empirical_model

### Write function to return expected points for given situation
expected_points <- function(down, fp, ytg, pos) {
    
    fp_converted <- ifelse(
      tolower(pos) == "own", fp, 100 - fp
    )
    # print(fp_converted)
  
    fp_bin <- case_when(
      between(fp_converted, 0, 20) ~ "Own Side - Far",
      between(fp_converted, 21, 50) ~ "Own Side",
      between(fp_converted, 51, 79) ~ "Opponent Side",
      between(fp_converted, 80, 89) ~ "Redzone - Far",
      fp_converted >= 90 ~ "Redzone - Close"
    )
    # print(fp_bin)
    
    ytg_bin = case_when(
      ytg >= 8 ~ "Long",
      between(ytg, 5, 7) ~ "Med",
      between(ytg, 3, 4) ~ "Short",
      ytg < 3 ~ "Shortest"
    )
    # print(ytg_bin)
    
    empirical_model %>%
      filter(
        pff_DOWN == down,
        FIELDPOSITION_BIN == fp_bin,
        DISTANCE_BIN == ytg_bin
      ) %>%
      pull(expected_points)
  }
  
