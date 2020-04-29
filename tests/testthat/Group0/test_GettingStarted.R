cat ("\ntest GettingStarted.R\n")
timeStep   <- 1/30
endTime    <- 30 
ts <- seq(0, endTime, by = timeStep)
nsim <- 50
v <- new("Pedestrian")
ans <- SimulateCrossingScenario(v, ts, nsim)