cat("\nfGetCrossingTrajectory is linked to GetPedestrianCrossingTrajectory\n")
cat("A function stored with a Pedestrian object and linked to
    GetPedestrianCrossingTrajectory.m via a call in GetScenarioConstants.m")

## library(interACT)
timeStep   <- 1/30
endTime    <- 30 
VTimeStamp <- seq(0, endTime, by = timeStep)

v0 <- new("Pedestrian", 
          sCountry = 'UK',
          sScenarioName = 'Pedestrian crossing UK')
idxCrossingOnsetSample <- 1;

## fGetCrossingTrajectory is a function stored in a Pedestrian object in
## model-class.R
## GetPedestrianCrossingTrajectory.m
ans <- v0@fGetCrossingTrajectory(VTimeStamp, idxCrossingOnsetSample)
library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group1/VY.txt"
VY_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans@VY, VY_octave)

idxCrossingOnsetSample <- 10
ans <- v0@fGetCrossingTrajectory(VTimeStamp, idxCrossingOnsetSample)
ans@VY
