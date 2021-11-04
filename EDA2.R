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

### 1st or 2nd down ----
data %>%
  filter(pff_DOWN %in% c(1,2)) -> first_second_down

# Own Redzone
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 43.9% pass, 56.1% run

# Pass
plotdist((first_second_down %>%
           filter(FIELDPOSITION <= 20,
                  pff_RUNPASS %in% c("P")
           ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

first_second_down %>%
    filter(FIELDPOSITION <= 20,
           pff_RUNPASS %in% c("P")
    ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 59.4%
    inc = mean(pff_GAINLOSS == 0 | is.na(pff_GAINLOSS)), # 32.8%
    neg = mean(pff_GAINLOSS < 0 & !is.na(pff_GAINLOSS))  # 7.8%
  )

# Positive Pass
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 1.4612, rate = 0.1145)

# Negative Pass
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Weibull(Shape = 1.5707, Scale = 5.3136)

# Run


# Open Field
first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 50.7% pass, 49.3% run

# Opponent Redzone
first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 37.1% pass, 62.9% run


### 3rd down ----
data %>%
  filter(pff_DOWN == 3) -> third_down

# Own Redzone
third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 74.7% pass, 25.3% run

# Open Field
third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 70.8% pass, 29.2% run

# Opponent Redzone
third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 60.1% pass, 39.9% run


### 4th down
