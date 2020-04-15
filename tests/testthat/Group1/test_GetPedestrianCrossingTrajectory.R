cat("\n------ Testing GetPedestrianCrossingTrajectory ------\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
VTimeStamp <- seq(0, endTime, by = timeStep)

v0 <- new("SCrossingRoadUserConstants", 
          sCountry = 'UK',
          sScenarioName = 'Pedestrian crossing UK',
          sScenarioType = "Pedestrian")
idxCrossingOnsetSample <- 1;

## GetCrossingTrajectory is Generic method set for SCrossingRoadUserConstants
## class in model-class.R
ans <- GetCrossingTrajectory(v0, VTimeStamp, idxCrossingOnsetSample)
library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group1/VY.txt"
VY_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans@VY, VY_octave)
