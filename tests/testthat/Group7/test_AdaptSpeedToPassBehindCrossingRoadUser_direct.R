cat("\n------ Testing test_AdaptSpeedToPassBehindCrossingRoadUser_direct ------\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 
VTimeStamp <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian")

tmp0 <- SimulateCrossingScenario(v, VTimeStamp, 50)
VidxCrossingOnset <- numeric(50)

## Happen in SimulateCrossingScenario
tmp <- tmp0$VCrossingOnsetCDF >= tmp0$VCumulativeCrossingOnsetProbabilities[1]
VidxCrossingOnset[1] <- min( which(tmp) )

SCrossingRoadUserBehaviour <- v@fGetCrossingTrajectory(VTimeStamp, VidxCrossingOnset[1])
SCrossingRoadUserBehaviour@idxCrossingOnsetSample <- VidxCrossingOnset[1]

## get the crossing trajectory
ans <- v@fAdaptToCrossingBehaviour(VTimeStamp, SCrossingRoadUserBehaviour)
ans
str(ans)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/SApproachRoadUserBehaviour.bin"
d <- R.matlab::readMat(pathname)
dd <- d$SApproachingRoadUserBehaviour[,,1]
names(dd)
VY_octave <- as.vector(dd$VY)
VX_octave <- as.vector(dd$VX)
VXdot_octave <- as.vector(dd$VXdot)
VHeading_octave <- as.vector(dd$VHeading)
VbYieldingeHMI_octave <- as.vector(dd$VbYieldingeHMI)

all.equal(ans@VX, VX_octave)
all.equal(ans@VXdot, VXdot_octave)
ans@VbYieldingeHMI

