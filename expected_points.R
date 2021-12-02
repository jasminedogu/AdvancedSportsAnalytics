# E[Opp 1st & 10 at 25]
monte_carlo <- replicate(10000,
                         score_simulator(fp = 25,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep

# E[Opp 1st & 10 at FP-7]
monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(60:68), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_60_68

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(69:77), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_69_77

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(78:86), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_78_86

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(87:94), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_87_94

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(95:100), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_95_100

# E[1st & 10 at FP+YTG] (after we convert on 4th down)
# 60-68 FP, 2-3 YTG
replicate(10000,
          score_simulator(fp = sample(60:68, 1) + sample(2:3, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()
