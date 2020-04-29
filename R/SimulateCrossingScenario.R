##' Get a rotation matrix
##' 
##' Get a matrix M for rotating a 2D column vector X by the input parameter 
##' angle around the origin
##' 
##' @param x roating angle
##' @return a 2-D matrix
##' @export
Get2DRotationMatrix <- function(x) {
  return ( matrix(c(cos(x), -sin(x), sin(x), cos(x)), nrow=2, byrow=TRUE) )
}

##' AddRoadUserCornerMatrices.m
##' 
##' add a field MCornerMatrices, describing the locations of the four
##' corners of the road user over time in the scenario, to the 
##' input/output structure SRoadUserBehaviour, based on the VX, VY, and
##' VHeading fields of that structure, and the fields length and width of
##' the SRoadUserConstants input structure
##' 
##' @param x a Pedestrian or a Driver object
##' @param b a Behaviour object 
##' @param type either Approaching or Crossing behaviour
##' @importFrom pracma cumtrapz
##' @export
AddRoadUserCornerMatrices <- function(x, b, type = "Approaching") {
  ## get a non-transformed corner matrix
  
  if(type == "Approaching") {
    halfLength <- x@alength/2;
    halfWidth <- x@awidth/2;
  } else if (type == "Crossing") {
    halfLength <- x@clength/2;
    halfWidth <- x@cwidth/2;
  } else {
    stop("Undefined type")
  }
  
  MNonTransformedCorners <- matrix(
    c(halfLength, -halfLength, -halfLength, halfLength, halfLength,
      halfWidth,  halfWidth,   -halfWidth,  -halfWidth, halfWidth),
    ncol = 5, byrow=TRUE);
  
  nTimeSteps <- length(b@VX);
  MCornerMatrices <- array(0, dim = c(2,5,nTimeSteps))
  
  if ( all(b@VHeading == b@VHeading[1]) ){
    ## heading does not change, so calculate rotation just once
    MCornerMatrices_tmp <- array(0, dim = c(2,5,nTimeSteps))
    
    MPrerotatedCornerMatrix <- Get2DRotationMatrix(b@VHeading[1]) %*% 
        MNonTransformedCorners

      MCornerMatrices[1,,] <- matrix( rep(b@VX, each = 5), nrow = 5)
      MCornerMatrices[2,,] <- matrix( rep(b@VY, each = 5), nrow = 5)
      for(i in 1:nTimeSteps) { MCornerMatrices_tmp[,,i] <- MPrerotatedCornerMatrix }
      out <- MCornerMatrices + MCornerMatrices_tmp
  } else {
    ## heading does change, so calculate rotation for each time step
    ## TODO: change to apply
    for(i in 1:nTimeSteps) {
      term0 <- c(b@VX[i], b@VY[i])
      term1 <- Get2DRotationMatrix(b@VHeading[i]) %*% MNonTransformedCorners;
      MCornerMatrices[,,i] <- term0 + term1
    }
    out <- MCornerMatrices
  }
  return(out)
}


##' SimulateCrossingScenario - run interACT road crossing model simulation
##' 
##' @param x a Pedestrian or a Driver
##' @param VTimeStamp a time vector
##' @param nSimulations number of simulations, an integer.
##' @return a list
##' @importFrom pracma cumtrapz
##' @export
SimulateCrossingScenario <- function(x, VTimeStamp, nSimulations) 
{
    ## Get the non-adapted approach behaviour of the approaching road user.
    ## This should be stored in the output.
    SNonAdaptedApproachingRoadUserBehaviour <- x@fGetApproachBehaviour(VTimeStamp)
    
    ## get the distribution of crossing onset times, using non adapted approach behaviour
    VCrossingOnsetPDF <- GetModelCrossingOnsetTimePDF(x, VTimeStamp,
                                  SNonAdaptedApproachingRoadUserBehaviour)
    VCrossingOnsetCDF <- pracma::cumtrapz(VTimeStamp, VCrossingOnsetPDF)
    ## sample the CDF for differents crossing behaviours, and the resulting
    ## adapted behaviour of the approaching road user
    VCumulativeCrossingOnsetProbabilities <- seq(0, 1, length=nSimulations+2)
    VCumulativeCrossingOnsetProbabilities <- VCumulativeCrossingOnsetProbabilities[2:(nSimulations+1)]
    
    ## Create containers
    VidxCrossingOnset <- numeric(nSimulations)
    aBehaviours <- vector("list", nSimulations)
    cBehaviours <- vector("list", nSimulations)

  for (i in 1:nSimulations) {
        ## use CDF to get crossing onset time to use for this simulation
        tmp <- VCrossingOnsetCDF >= VCumulativeCrossingOnsetProbabilities[i]
        VidxCrossingOnset[i] <- min( which(tmp) )
        ## get the crossing trajectory
        cBehaviours[[i]] <- x@fGetCrossingTrajectory(VTimeStamp, VidxCrossingOnset[i])

        ## store the crossing onset sample for this simulation in a separate vector
        cBehaviours[[i]]@idxCrossingOnsetSample <- VidxCrossingOnset[i];

        ## adapt the behaviour of the approaching road user to the behaviour of the
        ## crossing road user
        aBehaviours[[i]] <- x@fAdaptToCrossingBehaviour(VTimeStamp, cBehaviours[[i]])

        ## add corner location matrices (for bird's eye view plotting)
        aBehaviours[[i]]@MCornerMatrices <- 
          AddRoadUserCornerMatrices(x, aBehaviours[[i]]) 
        cBehaviours[[i]]@MCornerMatrices <- 
          AddRoadUserCornerMatrices(x, cBehaviours[[i]], type = "Crossing")
  }

  out <- list(VCrossingOnsetPDF = VCrossingOnsetPDF,
              VidxCrossingOnset = VidxCrossingOnset,
              aBehaviours = aBehaviours,
              cBehaviours = cBehaviours)
  return(out)
}
