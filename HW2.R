setwd("~/Fall 2021 Courses/STAT 4800")
library(tidyverse)
library(Rcpp)
library(markovchain)

#reading in the dataset and creating point_diff column
data <- read.csv("SB_box_scores_2019_without_rank.csv") %>%
  mutate(point_diff = Pts_winner - Pts_loser)

#selecting winner and loser columns as a vector
data %>% select(data=Winner) %>% as.vector() -> Winner 
data %>% select(data=Loser) %>% as.vector() -> Loser

#finding unique values of team names in both the winner and loser vectors
rbind(Winner, Loser) %>% unique() %>% pull() -> teams_list

#creating an 217x217 matrix with 0's and assigning the unique team names as the rownames and as the colnames
team_matrix <- matrix(0 ,nrow=217, ncol = 217) 
rownames(team_matrix) <- teams_list
colnames(team_matrix) <- teams_list

#constructing the transition matrix using different probabilites depending on the point_diff magnitude
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

#finding the values for the diagonal of the matrix 
#each row has to add up to one 
#used N = 13 since teams normally play 12 regular season games and few teams also play a conference championship game

for (i in 1:nrow(team_matrix)) {
  
  total <- sum(team_matrix[i,]) / 13
  team_matrix[i,i] <- (1-total)
  
}

#dividing the lower and upper triangles (not including diagonal) by 13 to get them scaled and have all rows sums the same
team_matrix[upper.tri(team_matrix) | lower.tri(team_matrix)] <- team_matrix[upper.tri(team_matrix) | lower.tri(team_matrix)] / 13

#creating a markovchain object to use for steadystate
markov_chain <- new("markovchain", states = teams_list, transitionMatrix = team_matrix)

#finding the steady state vector for the above Markov chain
steadyStates(markov_chain) %>%
  t() %>%
  as.data.frame() %>%
  rename(steadyStateProb=V1) %>%
  arrange(desc(steadyStateProb)) %>%
  mutate(ranking = row_number()) -> rankings
