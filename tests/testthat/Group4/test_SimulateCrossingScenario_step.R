cat("\n------ Testing SimulateCrossingScenario ------\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
ts <- seq(0, endTime, by = timeStep)
nSimulations <- 50
v <- new("Pedestrian")
res <- SimulateCrossingScenario(v, ts, 50) 
str(res)  

## This should be stored in the output.
SNonAdaptedApproachingRoadUserBehaviour <- v@fGetApproachBehaviour(ts)
str(SNonAdaptedApproachingRoadUserBehaviour)

## get the distribution of crossing onset times, using non adapted approach behaviour
VCrossingOnsetPDF <- GetModelCrossingOnsetTimePDF(v, ts,
                                                  SNonAdaptedApproachingRoadUserBehaviour)
VCrossingOnsetCDF <- pracma::cumtrapz(ts, VCrossingOnsetPDF)
## sample the CDF for differents crossing behaviours, and the resulting
## adapted behaviour of the approaching road user
VCumulativeCrossingOnsetProbabilities <- seq(0, 1, length=nSimulations+2)
VCumulativeCrossingOnsetProbabilities <- VCumulativeCrossingOnsetProbabilities[2:(nSimulations+1)]
## Create a container
VidxCrossingOnset <- numeric(nSimulations)
bCrossing    <- new("Behaviour")
bApproaching <- new("Behaviour")

i <- 1

for (i in 1:nSimulations) {
      ## use CDF to get crossing onset time to use for this simulation
      tmp <- VCrossingOnsetCDF >= VCumulativeCrossingOnsetProbabilities[i]
      VidxCrossingOnset[i] <- min( which(tmp) )
      VidxCrossingOnset[i]
      
      ## get the crossing trajectory
      bCrossing <- v@fGetCrossingTrajectory(ts, VidxCrossingOnset[i])
      
      d <- R.matlab::readMat("/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/SCrossingRoadUserBehaviour.bin")
      VX_octave       <- as.vector( d$SCrossingRoadUserBehaviour[[1]] )
      VY_octave       <- as.vector( d$SCrossingRoadUserBehaviour[[3]] )
      VHeading_octave <- as.vector( d$SCrossingRoadUserBehaviour[[2]] )
      testthat::expect_equivalent(bCrossing@VX, VX_octave)
      testthat::expect_equivalent(bCrossing@VY, VY_octave)
      testthat::expect_equivalent(bCrossing@VHeading, VHeading_octave)
      
      ## store the crossing onset sample for this simulation in a separate vector
      bCrossing@idxCrossingOnsetSample <- VidxCrossingOnset[i];
      ## adapt the behaviour of the approaching road user to the behaviour of the
      ## crossing road user
      bApproaching <- v@fAdaptToCrossingBehaviour(ts, bCrossing)

      d <- R.matlab::readMat("/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/SApproachingRoadUserBehaviour.bin")
      str(d)
      VY_octave <- as.vector( d$SApproachingRoadUserBehaviour[[1]] )
      VX_octave <- as.vector( d$SApproachingRoadUserBehaviour[[2]] )    
      VXdot_octave <- as.vector( d$SApproachingRoadUserBehaviour[[3]] )
      VHeading_octave <- as.vector( d$SApproachingRoadUserBehaviour[[4]] )
      VbYieldingeHMI_octave <- as.vector( d$SApproachingRoadUserBehaviour[[5]] )
      testthat::expect_equivalent(bApproaching@VX, VX_octave)
      testthat::expect_equivalent(bApproaching@VXdot, VXdot_octave)
      testthat::expect_equivalent(bApproaching@VY, VY_octave)
      testthat::expect_equivalent(bApproaching@VHeading, VHeading_octave)
      testthat::expect_equivalent(bApproaching@VbYieldingeHMI, VbYieldingeHMI_octave)

      ## add corner location matrices (for bird's eye view plotting)
      bApproaching@MCornerMatrices <-
        AddRoadUserCornerMatrices(v, bApproaching);
      
      halfLength <- v@alength/2;
      halfWidth <- v@awidth/2;
      MNonTransformedCorners <- matrix(
        c(halfLength, -halfLength, -halfLength, halfLength, halfLength,
          halfWidth,  halfWidth,   -halfWidth,  -halfWidth, halfWidth),
        ncol =5, byrow=TRUE);
      
      nTimeSteps <- length(bApproaching@VX);
      MCornerMatrices <- array(0, dim = c(2,5,nTimeSteps))
      MCornerMatrices_tmp <- array(0, dim = c(2,5,nTimeSteps))
      MPrerotatedCornerMatrix <- Get2DRotationMatrix(bApproaching@VHeading[1]) %*% MNonTransformedCorners;
     
      MCornerMatrices[1,,] <- matrix( rep(bApproaching@VX, each = 5), nrow = 5)
      MCornerMatrices[2,,] <- matrix( rep(bApproaching@VY, each = 5), nrow = 5)
      
      for(i in 1:nTimeSteps) {
        MCornerMatrices_tmp[,,i] <- MPrerotatedCornerMatrix
      }
      tmp <- MCornerMatrices + MCornerMatrices_tmp
      str(tmp)
      

      d <- R.matlab::readMat("/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/MCornerMatrices.bin")
      str(d$MCornerMatrices)
      all.equal(tmp, d$MCornerMatrices)
      
      for(i in 1:nTimeSteps) {
        term0 <- c(bApproaching@VX[i], bApproaching@VY[i])
        term1 <- Get2DRotationMatrix(bApproaching@VHeading[i]) %*% MNonTransformedCorners;
        MCornerMatrices[,,i] <- term0 + term1
      }
      
      d <- R.matlab::readMat("/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/MCornerMatrices1.bin")
      str(d$MCornerMatrices)
      all.equal(MCornerMatrices, d$MCornerMatrices)
      
  
      if all(SRoadUserBehaviour.VHeading == SRoadUserBehaviour.VHeading(1))
      
      


}