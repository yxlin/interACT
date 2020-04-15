##' SimulateCrossingScenario - run interACT road crossing model simulation
##' 
##' @param x a Pedestrian or a Driver
##' @param VTimeStamp a time vector
##' @param nSimulations number of simulations, an integer.
##' @importFrom pracma cumtrapz
##' @export
SimulateCrossingScenario <- function(x, VTimeStamp, nSimulations) 
{
  SNonAdaptedApproachingRoadUserBehaviour <- v@fGetApproachBehaviour(VTimeStamp)
  ## get the distribution of crossing onset times
  VCrossingOnsetTimePDF <- GetModelCrossingOnsetTimePDF(x, VTimeStamp, SNonAdaptedApproachingRoadUserBehaviour)

  ## sample the CDF for differents crossing behaviours, and the resulting adapted
  ## behaviour of the approaching road user
  VCrossingOnsetCDF <- pracma::cumtrapz(VTimeStamp, VCrossingOnsetTimePDF)
  VCumulativeCrossingOnsetProbabilities <- seq(0, 1, length=nSimulations+2)
  VCumulativeCrossingOnsetProbabilities <- VCumulativeCrossingOnsetProbabilities[2:(nSimulations+1)]
  
  VidxCrossingOnset <- numeric(nSimulations)
  
  for (iSimulation in 1:nSimulations) {
    ## use CDF to get crossing onset time to use for this simulation
    tmp <- VCrossingOnsetCDF >= VCumulativeCrossingOnsetProbabilities(iSimulation)
    VidxCrossingOnset[iSimulation] <- min( which(tmp == TRUE) )
    
    ## get the crossing trajectory
    SCrossingRoadUserBehaviour <- x@fGetCrossingTrajectory(VTimeStamp, 
                                                           VidxCrossingOnset[iSimulation])
    
    ## store the crossing onset sample for this simulation in a separate vector
    ## as well
    SCrossingRoadUserBehaviour@idxCrossingOnsetSample <- VidxCrossingOnset(iSimulation);
    
    ## adapt the behaviour of the approaching road user to the behaviour of the
    ## crossing road user
    # SApproachingRoadUserBehaviour = ...
    # SApproachingRoadUserConstants.fAdaptToCrossingBehaviour(...
    #                                                         VTimeStamp, SApproachingRoadUserConstants, SNonAdaptedApproachingRoadUserBehaviour, ...
    #                                                         SCrossingRoadUserConstants, SCrossingRoadUserBehaviour);
    
    


  } 

  return(VCumulativeCrossingOnsetProbabilities)
}
