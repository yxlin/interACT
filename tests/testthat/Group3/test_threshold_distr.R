cat("\n------ Testing threshold_distr ------\n")
## library(interACT)
## library(data.table)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
ts <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian")
SNonAdaptedApproachingRoadUserBehaviour <- v@fGetApproachBehaviour(ts)
distance <- GetLongDistanceFromCrosserInitialPosToApproacherFront(
  v, SNonAdaptedApproachingRoadUserBehaviour)
speed <- -SNonAdaptedApproachingRoadUserBehaviour@VXdot
ehmi <- SNonAdaptedApproachingRoadUserBehaviour@VbYieldingeHMI
p <- v@SModelSpecificConstants
VCrossingOnsetTimePDF <- threshold_distr(ts, distance, speed, ehmi, p)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group3/VCrossingOnsetTimePDF.txt"
VCrossingOnsetTimePDF_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VCrossingOnsetTimePDF, VCrossingOnsetTimePDF_octave)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group3/VCrossingOnsetCDF.txt"
VCrossingOnsetCDF_octave <- as.vector(t(fread(pathname)))
VCrossingOnsetCDF <- pracma::cumtrapz(ts, VCrossingOnsetTimePDF)
testthat::expect_equivalent(VCrossingOnsetCDF[,1], VCrossingOnsetCDF_octave)

nSimulations <- 50
VCumulativeCrossingOnsetProbabilities <- seq(0, 1, length=nSimulations+2)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group3/VCumulativeCrossingOnsetProbabilities0.txt"
VCumulativeCrossingOnsetProbabilities0_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VCumulativeCrossingOnsetProbabilities, VCumulativeCrossingOnsetProbabilities0_octave)

VCumulativeCrossingOnsetProbabilities <- VCumulativeCrossingOnsetProbabilities[2:(nSimulations+1)]
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group3/VCumulativeCrossingOnsetProbabilities1.txt"
VCumulativeCrossingOnsetProbabilities1_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VCumulativeCrossingOnsetProbabilities, VCumulativeCrossingOnsetProbabilities1_octave)


