#### Functions doc
#### Created March 15, 2023

# this doc is for all the functions I intend to use more than once in this exploration
# This is kind of a dumping ground right now, will need better formatting

## Leaving function
# how do seals decide whether they stay at the locks and keep being specialists?
# some threshold of salmon per seal that makes it worthwhile to switch behaviors
# salmon is Ol, seals is S

leaving.rate <- function(salmon, seals, threshold, steepness) {
  if(salmon<0.5) salmon <- 0
  if(seals<0.5) seals <- 0
  perseal <- salmon/seals
  if(perseal == "NaN") {
    leave.rate <- 0
    } else if(perseal == Inf) {
      leave.rate <- 0
      } else {
        leave.rate <- 1/(1 + exp(-steepness * (threshold - perseal)))
      }
  return(leave.rate)
}

# plot(0:250, 1/(1 + exp(-0.2 * (50 - 0:250))))
leaving.rate(1, 1, 50, 0.2)

# Switch function
# how seals determine that they will switch from I to S
# salmon is Ol
# some threshold of salmon that makes it interesting to check out and potentially switch to
# steepness of function
# the salmon per seal or per space piece will go in the leave part not here

switch.rate <- function(salmon, threshold, steepness) {
  return(1-(1/(1 + exp(-steepness * (threshold - salmon)))))
}

switch.rate(0.0000, 200, 0.1)

# Discovery function
# how many seals discover salmon specializing from the normal seal population and try it out? N -> S
# d is some constant that converts salmon available into the motivation to go chase them to the Locks (?)
# seals is N

discovery.rate <- function(seals, salmon, d) {
  return(min(seals, rnorm(n = 1, mean = d * seals * salmon, sd = d*100)))
}

discovery.rate(9000, 100, 0.000005)


# Seal Predation function
# calculate consumed salmon at Locks
# from diet data - update eventually
# or could be lotka-volterra but idk constants?

predation.rate <- function(seals, consumption) {
  return(seals * consumption)
}
