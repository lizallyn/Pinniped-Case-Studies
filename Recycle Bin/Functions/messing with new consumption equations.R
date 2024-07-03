
# seal consumption parameters
alpha <- 0.05
Cmax <- 5
Ns <- 5000
gamma <- -0.5
Nseal <- 50

q <- seq(0, 1, 0.01)
P <- 100

plot(x = q, y = (Cmax * Nseal *(alpha * Ns * Nseal^gamma) / 
                         (Cmax + alpha * Ns * Nseal^gamma + q * P)), 
     ylab = "Salmon consumed by seals (n = 50)",
     xlab = "q")

q <- 0.5
P <- seq(Nseal, 200, 1)

plot(x = P/Nseal, y = (Cmax * Nseal *(alpha * Ns * Nseal^gamma) / 
           (Cmax + alpha * Ns * Nseal^gamma + q * P)), 
     ylab = "Salmon consumed by seals (n = 50)",
     xlab = "Total predators / num seals (50)")

Nseal <- seq(1, 199, 1)
P <- 100

plot(x = P/Nseal, y = (Cmax * Nseal *(alpha * Ns * Nseal^gamma) / 
                         (Cmax + alpha * Ns * Nseal^gamma + q * P)), 
     ylab = "Salmon consumed by seals (n = 50)",
     xlab = "Total predators / num seals (50)")
