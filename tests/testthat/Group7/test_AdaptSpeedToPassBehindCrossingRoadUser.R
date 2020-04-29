cat("\n------ Testing test_AdaptSpeedToPassBehindCrossingRoadUser ------\n")
library(interACT)
library(data.table)
timeStep   <- 1/30
endTime    <- 30 
VTimeStamp <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian")
ans <- SimulateCrossingScenario(v, VTimeStamp, 50)
VidxCrossingOnset <- numeric(50)

tmp <- ans$VCrossingOnsetCDF >= ans$VCumulativeCrossingOnsetProbabilities[1]
VidxCrossingOnset[1] <- min( which(tmp == TRUE) )

## get the crossing trajectory
SCrossingRoadUserBehaviour <- v@fGetCrossingTrajectory(VTimeStamp, VidxCrossingOnset[1])
SCrossingRoadUserBehaviour

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/SCrossingRoadUserBehaviour.bin"
d <- R.matlab::readMat(pathname)
dd <- d$SCrossingRoadUserBehaviour[,,1]
VX_octave <- t(dd$VX)
VHeading_octave <- t(dd$VHeading)
VY_octave <- t(dd$VY)
testthat::expect_equivalent(SCrossingRoadUserBehaviour@VX, VX_octave)
testthat::expect_equivalent(SCrossingRoadUserBehaviour@VHeading, VHeading_octave)
testthat::expect_equivalent(SCrossingRoadUserBehaviour@VY, VY_octave)

SCrossingRoadUserBehaviour@idxCrossingOnsetSample <- VidxCrossingOnset[1]

unadaptedBehaviour <- v@fGetApproachBehaviour(VTimeStamp)
VX <- unadaptedBehaviour@VX;
VXdot <- unadaptedBehaviour@VXdot;
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VXdot0.txt"
VXdot0_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VXdot, VXdot0_octave)


idxCrossingOnsetSample <- SCrossingRoadUserBehaviour@idxCrossingOnsetSample

term0 <- (VX - .5*v@alength) <= (.5*v@cwidth)
term1 <- which(term0 == TRUE)
if(pracma::isempty(term1)) {
  idxConflictPointArrivalSample <- NULL
} else {
  idxConflictPointArrivalSample <- min(term1)
}

if(pracma::isempty(idxConflictPointArrivalSample)) {
  bNeedToAdaptSpeed <- TRUE ## open-loop approach trajectory does not cross conflict point
} else {
  ## open-loop approach trajectory arrives at conflict point after the crossing road user
  bNeedToAdaptSpeed <- idxConflictPointArrivalSample > idxCrossingOnsetSample
}

if (!bNeedToAdaptSpeed) stop("End of function")


distanceGapAtCrossingDecision <- VX[idxCrossingOnsetSample] - .5*v@alength -.5*v@cwidth
speedAtCrossingDecision <- -VXdot[idxCrossingOnsetSample];
if (speedAtCrossingDecision < 0) stop("speedAtCrossingDecision >= 0")

term0 <- SCrossingRoadUserBehaviour@VY - v@clength/2 > v@awidth/2
term1 <- SCrossingRoadUserBehaviour@VY + v@clength/2 < -v@awidth/2
VbCrosserNotInApproachersPath <- term0 | term1;

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VbCrosserNotInApproachersPath.txt"
VbCrosserNotInApproachersPath_octave <- as.logical(t(fread(pathname)))
testthat::expect_equivalent(VbCrosserNotInApproachersPath, VbCrosserNotInApproachersPath_octave)

idxCrosserExitsApproachersPath <- max( which(!VbCrosserNotInApproachersPath) )

if (pracma::isempty(idxCrosserExitsApproachersPath)) stop('Crosser never exits path of approacher.')

timeToCrossWithMargin <- VTimeStamp[idxCrosserExitsApproachersPath] + 
  v@desiredPostEncroachmentTime - VTimeStamp[idxCrossingOnsetSample];

## get the acceleration needed to arrive at a reference point near the conflict point
## at precisely the time the crossing road user reaches its reference point

term0 <- 2 / timeToCrossWithMargin^2
term1 <- (distanceGapAtCrossingDecision - speedAtCrossingDecision * timeToCrossWithMargin)
accelerationToApplyAtCrossingOnset <- min(v@maxAcceleration, term0*term1)

if (accelerationToApplyAtCrossingOnset > 0) {
  term0 <- (v@initialSpeed - speedAtCrossingDecision)/ accelerationToApplyAtCrossingOnset
  durationForAccelerationDuringCrossing <- min(timeToCrossWithMargin, term0)
  durationForStandstill                 <- 0;
} else { 
  timeToStandstill <- speedAtCrossingDecision / -accelerationToApplyAtCrossingOnset;
  if (timeToStandstill < timeToCrossWithMargin) {  ##  decelerating to full stop
  durationForAccelerationDuringCrossing <- timeToStandstill;
  durationForStandstill <- timeToCrossWithMargin - timeToStandstill; ##  this is a bit crude
  } else {                                         ##  not decelerating to full stop
  durationForAccelerationDuringCrossing <- timeToCrossWithMargin
  durationForStandstill <- 0;
  }
}

## get sample indices for the various steps of the process
accelerationDuringCrossingEndTime <- VTimeStamp[idxCrossingOnsetSample] + durationForAccelerationDuringCrossing;
term0 <- VTimeStamp >= accelerationDuringCrossingEndTime
idxAccelerationDuringCrossingEndSample <- min(which(term0))

standstillEndTime <- accelerationDuringCrossingEndTime + durationForStandstill;
idxStandstillEndSample <- min(which(VTimeStamp >= standstillEndTime))

## construct the acceleration signal
GetTwoPointDelta <- function(V) {
  n <- length(V)
  out <- numeric(n)
  out[1] <- V[2] - V[1]
  out[2:(n-1)] <- V[3:n] - V[1:(n-2)]
  out[n] <- V[n] - V[n-1]
  return(out)
}

DoTwoPointNumericalDifferentiation <- function(x, y) {
  ## numerical differentiation
  deltaX <- GetTwoPointDelta(x);
  deltaY <- GetTwoPointDelta(y);
  yPrime <- deltaY / deltaX;
  return(yPrime)
}
VXdotdot <- DoTwoPointNumericalDifferentiation(VTimeStamp, VXdot);
library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VXdotdot0.txt"
VXdotdot0_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VXdotdot, VXdotdot0_octave)


VXdotdot[(idxCrossingOnsetSample+1):idxAccelerationDuringCrossingEndSample] = -accelerationToApplyAtCrossingOnset;
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VXdotdot1.txt"
VXdotdot1_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VXdotdot, VXdotdot1_octave)

idx0 <- (idxAccelerationDuringCrossingEndSample+1)
idx1 <- idxStandstillEndSample
if (idx0 > idx1) {
  cat("VXdotdot stay the same")
} else {
  cat("replace ", idx0, " to ", idx1, " with 0")
  VXdotdot[(idxAccelerationDuringCrossingEndSample+1):idxStandstillEndSample] <- 0;
}

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VXdotdot2.txt"
VXdotdot2_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VXdotdot, VXdotdot2_octave)

VXdotdot[(idxStandstillEndSample+1):length(VXdotdot)] <- -v@maxAcceleration
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VXdotdot3.txt"
VXdotdot3_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VXdotdot, VXdotdot3_octave)

n <- length(VTimeStamp)
term0 <- VXdot[idxCrossingOnsetSample];
term1 <- VXdotdot[(idxCrossingOnsetSample+1):n];

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/term1.txt"
term1_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(term1, term1_octave)

term2 <- VXdotdot[idxCrossingOnsetSample:(n-1)];
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/term2.txt"
term2_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(term2, term2_octave)

term3 <- .5*(term1 + term2);
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/term3.txt"
term3_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(term3, term3_octave)

term4 <- diff( VTimeStamp[idxCrossingOnsetSample:length(VTimeStamp)] )
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/term4.txt"
term4_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(term4, term4_octave)


term5 <- term3*term4;
term6 <- cumsum(term5);
term7 <- term0 + term6
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/term7.txt"
term7_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(term7, term7_octave)

VXdot[(idxCrossingOnsetSample+1):n] <- term7
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VXdot1.txt"
VXdot1_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VXdot, VXdot1_octave)

VXdot <- pmax(VXdot, -v@initialSpeed);
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VXdot2.txt"
VXdot2_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VXdot, VXdot2_octave)


term0 <- VX[idxCrossingOnsetSample];
term1 <- VXdot[(idxCrossingOnsetSample+1):n];
term2 <- VXdot[idxCrossingOnsetSample:(n-1)];
term3 <- .5*(term1 + term2);
term4 <- diff( VTimeStamp[idxCrossingOnsetSample:length(VTimeStamp)] )
term5 <- term3*term4;
term6 <- cumsum(term5);
term7 <- term0 + term6

VX[(idxCrossingOnsetSample+1):n] <- term7
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group7/VX1.txt"
VX1_octave <- as.vector(t(fread(pathname)))
testthat::expect_equivalent(VX, VX1_octave)

unadaptedBehaviour@VX
unadaptedBehaviour@VXdot
adaptedBehaviour <- new("Behaviour")
adaptedBehaviour@VX <- VX
adaptedBehaviour@VXdot <- VXdot
adaptedBehaviour@VbYieldingeHMI[idxCrosserExitsApproachersPath:n] <- FALSE

