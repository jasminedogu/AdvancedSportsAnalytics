setwd("~/Fall 2021 Courses/STAT 4800")
library(tidyverse)

data <- read.csv("SB_box_scores_2019_without_rank.csv") %>%
  mutate(point_diff = Pts_winner - Pts_loser)

data %>% select(data=Winner) %>% as.vector() -> Winner
data %>% select(data=Loser) %>% as.vector() -> Loser

rbind(Winner, Loser) %>% unique() %>% pull() -> teams_list

M1 <- matrix(0 ,nrow=217, ncol = 217) 
rownames(M1) <- teams_list
colnames(M1) <- teams_list
