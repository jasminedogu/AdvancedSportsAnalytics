library(tidyverse)
library(fitdistrplus)

setwd("~/Fall 2021 Courses/STAT 4800")
read.csv("2019 PFF All Plays.csv") -> data

data %>%
  mutate(
  FIELDPOSITION = case_when(
    pff_FIELDPOSITION < 0 ~ pff_FIELDPOSITION * -1,
    pff_FIELDPOSITION > 0 ~ 100 - pff_FIELDPOSITION
  ),
  SIDE_OF_FIELD = ifelse(
    FIELDPOSITION >= 50, "OPPOSING", "OWN"
  )) -> data

### Punt yards based on field position
data %>%
  filter(pff_SPECIALTEAMSTYPE == "PUNT",
         SIDE_OF_FIELD == "OPPOSING",
         !is.na(pff_KICKYARDS)) %>%
  summarize(mean(pff_KICKYARDS),
            sd(pff_KICKYARDS))
  
data %>%
  filter(pff_SPECIALTEAMSTYPE == "PUNT",
         SIDE_OF_FIELD == "OPPOSING",
         !is.na(pff_KICKYARDS)) %>%
  ggplot() + geom_histogram(aes(x=pff_KICKYARDS))

data %>%
  filter(pff_SPECIALTEAMSTYPE == "PUNT",
         SIDE_OF_FIELD == "OWN",
         !is.na(pff_KICKYARDS)) %>%
  summarize(mean(pff_KICKYARDS),
            sd(pff_KICKYARDS))
data %>%
  filter(pff_SPECIALTEAMSTYPE == "PUNT",
         SIDE_OF_FIELD == "OWN",
         !is.na(pff_KICKYARDS)) %>%  
  ggplot() + geom_histogram(aes(x=pff_KICKYARDS))

# Represent punt length with linear regression model
lm(pff_KICKYARDS ~ FIELDPOSITION,
    data = data %>% filter(pff_SPECIALTEAMSTYPE == "PUNT")) -> model
summary(model)

# 47.4-.16x

ggplot(data=data %>% filter(pff_SPECIALTEAMSTYPE == "PUNT"),
       aes(x=FIELDPOSITION, y=pff_KICKYARDS)) + geom_point()

### Dist of yards gained based on field position (red zone)
### How to sample run plays -- based on field position and/or pos/neg
data %>%
  filter(pff_RUNPASS == "R",
         SIDE_OF_FIELD == "OWN") %>%  
  ggplot() + geom_histogram(aes(x=pff_GAINLOSS))

data %>%
  filter(pff_RUNPASS == "R",
         SIDE_OF_FIELD == "OPPOSING",
         FIELDPOSITION < 80) %>%  
  ggplot() + geom_histogram(aes(x=pff_GAINLOSS))

data %>%
  filter(pff_RUNPASS == "R",
         SIDE_OF_FIELD == "OPPOSING",
         FIELDPOSITION >= 80) %>%  
  ggplot() + geom_histogram(aes(x=pff_GAINLOSS))

### Run depending on down
data %>%
  filter(pff_RUNPASS == "R") %>%  
  ggplot() + geom_histogram(aes(x=pff_GAINLOSS))

### Deep pass
data %>% 
  filter(pff_RUNPASS == "P") %>%
  group_by(SIDE_OF_FIELD, FIELDPOSITION > 80) %>%
  summarize(mean(pff_DEEPPASS)) 

glm(pff_DEEPPASS ~ FIELDPOSITION, family = "binomial",
    data = data %>% filter(pff_RUNPASS == "P")) -> model
summary(model)

### Kickoff return (how many are touchbacks vs. have a return)
data %>% 
  filter(pff_SPECIALTEAMSTYPE == "KICKOFF") %>%
  summarize(mean(pff_KICKRESULT == "TOUCHBACK")) # 41.8% are touchbacks

data %>%
  filter(pff_SPECIALTEAMSTYPE == "KICKOFF",
         pff_KICKRESULT != "TOUCHBACK",
         pff_RETURNYARDS > 0,
         !is.na(pff_RETURNYARDS)) -> kickoff_returns

plotdist(kickoff_returns$pff_RETURNYARDS, histo = TRUE, demp = TRUE)

fw <- fitdist(kickoff_returns$pff_RETURNYARDS, "weibull")
fg <- fitdist(kickoff_returns$pff_RETURNYARDS, "gamma")
fln <- fitdist(kickoff_returns$pff_RETURNYARDS, "lnorm")

par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")
# qqcomp(list(fw, fln, fg), legendtext = c("Weibull", "lognormal", "gamma"))
# cdfcomp(list(fw, fln, fg), legendtext = c("Weibull", "lognormal", "gamma"))
# ppcomp(list(fw, fln, fg), legendtext = c("Weibull", "lognormal", "gamma"))

summary(fln)

play_type_num <- sample.int(2, size = 1, prob = c(.418, 1-.418))
play_type <- c("T", "R")[play_type_num]

if (play_type == "T") {
  yards_gained <- 25
} else if (play_type == "R") {
  yards_gained <- rlnorm(1, meanlog=2.925, sdlog=.5901)
}

yards_gained


### Safety?

