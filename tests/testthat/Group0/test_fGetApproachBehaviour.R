cat ("\nfGetApproachTrajectory is linked to GetConstantDecelerationVehicleApproachBehaviour.m\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 
VTimeStamp <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian")
## ans is the variable, called SNonAdaptedApproachingRoadUserBehaviour and 
## SApproachingRoadUserWithoutAdaptation in the output struct
ans <- v@fGetApproachBehaviour(VTimeStamp)


library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group0/VX.txt"
VX_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans@VX, VX_octave)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group0/VY.txt"
VY_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans@VY, VY_octave)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group0/VXdot.txt"
VXdot_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans@VXdot, VXdot_octave )

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group0/VHeading.txt"
VHeading_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans@VHeading, VHeading_octave )

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group0/VbYieldingeHMI.txt"
VbYieldingeHMI_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(ans@VbYieldingeHMI, VbYieldingeHMI_octave)

message("v@fGetApproachBehaviour testing passed")
        
        