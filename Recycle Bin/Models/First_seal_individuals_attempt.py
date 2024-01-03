# Started during meeting with Andrew Oct 20, 2023

import numpy as np

###############################################
#### Set Parameters
###############################################
max_time_steps = 5
num_seals = 20
seal_initial_prob_gauntlet = 0.1
seal_num_neighbours_2_copy = 2
seal_prob_2_copy = 0.5

###############################################
#### Initialize variables
###############################################
seal_prob_gauntlet = np.full(num_seals,seal_initial_prob_gauntlet)
seal_forage_loc = np.zeros(num_seals) #zero means at 'natural' place, 1 means at gauntlet

###############################################
#### Run time loop
###############################################

print(seal_forage_loc)

#loop over time (foraging days)
for t in range(max_time_steps):
  
    #decide (independantly) where each seal goes that day
    for seal in range(num_seals):
        if(np.random.rand()<seal_prob_gauntlet[seal]):
            seal_forage_loc[seal] = 1
        else:
            seal_forage_loc[seal] = 0
           
    #round of copying
    for seal in range(num_seals):
        #if the seal wasn't going to gaunlet
        if(seal_forage_loc[seal]==0):
            #pick random subset of other seals
            seals_2_copy = np.random.choice(num_seals,seal_num_neighbours_2_copy, replace=False) # we should come back to make sure they don't copy themselves
            social_information = np.mean(seal_forage_loc[seals_2_copy])
            #print(seal_forage_loc[seals_2_copy])
            if(np.random.rand() < (seal_prob_2_copy*social_information) ):
                seal_forage_loc[seal] = 1
    print(seal_forage_loc)
    print(sum(seal_forage_loc))

