cat("\n------ Testing threshold_distr ------\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
ts <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian")
SNonAdaptedApproachingRoadUserBehaviour <- v@fGetApproachBehaviour(ts)

distance <- GetLongDistanceFromCrosserInitialPosToApproacherFront(v, SNonAdaptedApproachingRoadUserBehaviour)

speed <- -SNonAdaptedApproachingRoadUserBehaviour@VXdot
ehmi <- SNonAdaptedApproachingRoadUserBehaviour@VbYieldingeHMI
p <- v@SModelSpecificConstants
VCrossingOnsetTimePDF <- threshold_distr(ts, distance, speed, ehmi, p)

library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/VCrossingOnsetTimePDF.txt"
VCrossingOnsetTimePDF_octave <- as.vector(t(fread(pathname)))
all.equal(VCrossingOnsetTimePDF, VCrossingOnsetTimePDF_octave)


pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/VCrossingOnsetCDF.txt"
VCrossingOnsetCDF_octave <- as.vector(t(fread(pathname)))

VCrossingOnsetCDF <- pracma::cumtrapz(ts, VCrossingOnsetTimePDF)
all.equal(VCrossingOnsetCDF[,1], VCrossingOnsetCDF_octave)

tmp <- cbind(VCrossingOnsetCDF[,1], VCrossingOnsetCDF_octave)
head(tmp)
tail(tmp)

nSimulations <- 50
VCumulativeCrossingOnsetProbabilities <- seq(0, 1, length=nSimulations+2)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/VCumulativeCrossingOnsetProbabilities0.txt"
VCumulativeCrossingOnsetProbabilities0_octave <- as.vector(t(fread(pathname)))
all.equal(VCumulativeCrossingOnsetProbabilities, VCumulativeCrossingOnsetProbabilities0_octave)


VCumulativeCrossingOnsetProbabilities <- VCumulativeCrossingOnsetProbabilities[2:(nSimulations+1)]
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/VCumulativeCrossingOnsetProbabilities1.txt"
VCumulativeCrossingOnsetProbabilities1_octave <- as.vector(t(fread(pathname)))
all.equal(VCumulativeCrossingOnsetProbabilities, VCumulativeCrossingOnsetProbabilities1_octave)


