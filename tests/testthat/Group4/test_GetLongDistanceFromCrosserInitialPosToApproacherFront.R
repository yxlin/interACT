cat("\n------ Testing GetLongDistanceFromCrosserInitialPosToApproacherFront ------\n")
library(interACT)
v0 <- new("Pedestrian", 
          sCountry = 'UK',
          sScenarioName = 'Pedestrian crossing UK')

rc1 <- new("SApproachingRoadUserConstants")

timeStep   <- 1/30
endTime    <- 30 
VTimeStamp <- seq(0, endTime, by = timeStep)
rb1 <- rc1@fGetApproachBehaviour(VTimeStamp)

ans <- GetLongDistanceFromCrosserInitialPosToApproacherFront(v0, rb1, rc1)
library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/VLongitudinalDistance.txt"
VLongitudinalDistance_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans, VLongitudinalDistance_octave)
all.equal(ans, VLongitudinalDistance_octave)

