## Set Up Parameters ----

## Dates ----

dates_buffer <- 20

## Salmon ----

# run.size is a distribution based on the description Craig sent, 
  # so we can draw a random run size if wanted.
# going to just use average for now though to make troubleshooting easier.

# Chum
chum.run.min <- 2000
chum.run.max <- 62000
chum.run.avg <- 27000
# chum.run.size <- rbeta(n = 1, 4, 5) * chum.run.max

# Green river Chinook
gr.run.min <- 6000
gr.run.max <- 42000
gr.run.avg <- 22000
# gr.run.size <- rnorm(1, gr.run.avg, 5500)

# LocNis Chinook
ln.run.min <- 300
ln.run.max <- 1400
ln.run.avg <- 630
# ln.run.size <- (rbeta(1, 3.5, 6)+0.1) * ln.run.max


