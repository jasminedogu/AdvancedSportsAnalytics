setwd("~/Fall 2021 Courses/STAT 4800")
library(tidyverse)
library(Rcpp)
library(markovchain)

data <- read.csv("SB_box_scores_2019_without_rank.csv") %>%
  mutate(point_diff = Pts_winner - Pts_loser)

data %>% select(data=Winner) %>% as.vector() -> Winner
data %>% select(data=Loser) %>% as.vector() -> Loser

rbind(Winner, Loser) %>% unique() %>% pull() -> teams_list

team_matrix <- matrix(0 ,nrow=217, ncol = 217) 
rownames(team_matrix) <- teams_list
colnames(team_matrix) <- teams_list

for (i in 1:nrow(data)) {
  winner <- data$Winner[i]
  loser <- data$Loser[i]
  point_diff <- data$point_diff[i]
  
  win_prob <- case_when(
    point_diff <= 7 ~ .6,
    between(point_diff, 8, 14) ~ .7,
    between(point_diff, 15, 21) ~ .8,
    point_diff >= 21 ~ .9
  )
  
  team_matrix[loser, winner] <- win_prob
  team_matrix[winner, loser] <- (1-win_prob)
  
}

for (i in 1:nrow(team_matrix)) {
  
  total <- sum(team_matrix[i,]) / 13
  team_matrix[i,i] <- (1-total)
  
}

team_matrix[upper.tri(team_matrix) | lower.tri(team_matrix)] <- team_matrix[upper.tri(team_matrix) | lower.tri(team_matrix)] / 13

markov_chain <- new("markovchain", states = teams_list, transitionMatrix = team_matrix)

steadyStates(markov_chain) %>%
  t() %>%
  as.data.frame() %>%
  rename(steadyStateProb=V1) %>%
  arrange(desc(steadyStateProb)) %>%
  mutate(ranking = row_number()) -> rankings
