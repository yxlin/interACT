cat("\n------ Testing GetLongDistanceFromCrosserInitialPosToApproacherFront ------\n")
## library(interACT)
timeStep   <- 1/30
endTime    <- 30 
VTimeStamp <- seq(0, endTime, by = timeStep)

v0 <- new("Pedestrian", 
          sCountry = 'UK',
          sScenarioName = 'Pedestrian crossing UK')

aB <- v0@fGetApproachBehaviour(VTimeStamp)
ans <- GetLongDistanceFromCrosserInitialPosToApproacherFront(v0, aB)

## library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group2/VLongitudinalDistance.txt"
VLongitudinalDistance_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans, VLongitudinalDistance_octave)
all.equal(ans, VLongitudinalDistance_octave)

