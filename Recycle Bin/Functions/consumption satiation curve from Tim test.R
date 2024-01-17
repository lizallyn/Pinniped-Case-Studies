# messing with the satiation predation equation Tim sent

# terms
days <- 10
s <- 5
h <- 0.05
seals <- 1:25
pd <- 0
salmon <- 1000
Y <- 1


prey_consumed <- (s*h*seals^(1+pd)*salmon)/(s+h*salmon*seals^pd+Y)

plot((s*h*seals^(1+pd)*salmon)/(s+h*salmon*seals^pd+Y))
