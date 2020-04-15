cat("\n------ Testing AdaptSpeedToPassBehindCrossingRoadUser ------\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
VTimeStamp <- seq(0, endTime, by = timeStep)

v <- new("SApproachingRoadUserConstants")
ans <- v@fAdaptToCrossingBehaviour(VTimeStamp)
