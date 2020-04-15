cat("\n------ Testing SimulateCrossingScenario ------\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
ts <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian")
ans <- SimulateCrossingScenario(v, ts, 50)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/VCumulativeCrossingOnsetProbabilities1.txt"
VCumulativeCrossingOnsetProbabilities1_octave <- as.vector(t(fread(pathname)))
all.equal(VCumulativeCrossingOnsetProbabilities, VCumulativeCrossingOnsetProbabilities1_octave)

min(which(lv == TRUE))


tmp <- VCrossingOnsetCDF >= VCumulativeCrossingOnsetProbabilities[1]
min( which(tmp==TRUE) )

find(VCrossingOnsetCDF >= VCumulativeCrossingOnsetProbabilities(iSimulation), 1, 'first');
