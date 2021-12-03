# E[1st & 10 at FP+YTG] (after we convert on 4th down)

###### FP = 69-77 #######

#ytg = 0-1
replicate(10000,
          score_simulator(fp = sample(69:77, 1) + sample(0:1, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

#ytg = 2-3

replicate(10000,
          score_simulator(fp = sample(69:77, 1) + sample(2:3, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

#ytg = 4-5
replicate(10000,
          score_simulator(fp = sample(69:77, 1) + sample(4:5, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

#ytg = 6-7
replicate(10000,
          score_simulator(fp = sample(69:77, 1) + sample(6:7, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

#ytg = 8-10
replicate(10000,
          score_simulator(fp = sample(69:77, 1) + sample(8:10, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()


###### FP = 87-94 #######
#ytg = 0-1
replicate(10000,
          score_simulator(fp = sample(87:94, 1) + sample(0:1, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

#ytg = 2-3
replicate(10000,
          score_simulator(fp = sample(87:94, 1) + sample(2:3, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()


#ytg = 4-5
replicate(10000,
          score_simulator(fp = sample(87:94, 1) + sample(4:5, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

#ytg = 6-7
replicate(10000,
          score_simulator(fp = sample(87:94, 1) + sample(6:7, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

#ytg = 8-10
replicate(10000,
          score_simulator(fp = sample(87:94, 1) + sample(8:10, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

