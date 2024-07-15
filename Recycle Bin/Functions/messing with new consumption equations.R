
# seal consumption parameters
alpha <- 0.1
Cmax <- 5
Ns <- 2500
gamma <- -0.5



Nseal <- seq(1, 199, 1)

plot(x = Nseal, y = Cmax * alpha * Nseal^(1+gamma) * Ns / 
       (Cmax + alpha * Ns * Nseal^gamma))

Nseal <- 50
q <- seq(0, 1, 0.01)
P <- 100

plot(x = q, y = (Cmax * alpha * Ns * Nseal^(1+gamma)) / 
                         (Cmax + alpha * Ns * Nseal^gamma + q * P), 
     ylab = "Salmon consumed by seals (n = 50)",
     xlab = "q")

Nseal <- 50
q <- 0.5
P <- seq(Nseal, 200, 1)

plot(x = P/Nseal, y = (Cmax * alpha * Ns * Nseal^(1+gamma)) / 
           (Cmax + alpha * Ns * Nseal^gamma + q * P), 
     ylab = "Salmon consumed by seals (n = 50)",
     xlab = "Total predators / num seals (50)")

Nseal <- seq(1, 199, 1)
P <- 10

plot(x = Nseal, y = (Cmax * alpha * Ns * Nseal^(1+gamma)) / 
                         (Cmax + alpha * Ns * Nseal^gamma + q * P), 
     ylab = "Salmon consumed by seals when P=100",
     xlab = "num seals")
