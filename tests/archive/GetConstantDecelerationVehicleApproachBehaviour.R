######### SApproachingRoadUserBehaviour-----------------------------------
##' Found in GetConstantDecelerationVehicleApproachBehaviour.m
##'
##' A function to get behaviour of an approaching road user yielding with
##' respect to a conflict point, or just driving at constant speed (if the
##' yielding-related scenario constants are NaNs, that is what the function
##' will return)
##'
##' The function takes two inputs. The first input is VTimeStamp, a sequence of 
##' times in SI unit (second). The second is SApproachingRoadUserConstants, a 
##' struct storing:
##' \enumerate{
##'    \item SApproachingRoadUserConstants.initialSpeed
##'    \item SApproachingRoadUserConstants.initialTTA
##'    \item SApproachingRoadUserConstants.decelerationOnsetTTA
##'    \item SApproachingRoadUserConstants.stopAtDistanceFromConflict
##'    \item SApproachingRoadUserConstants.length
##'    \item SApproachingRoadUserConstants.eHMIOnsetTTA
##'  }
##'
##' -- Found in GetBaseApproachingRoadUserConstants.m
##' SApproachingRoadUserConstants.fGetApproachBehaviour = ...
##'     @GetConstantDecelerationVehicleApproachBehaviour;
##' SApproachingRoadUserConstants.length = 4.7; % m
##' SApproachingRoadUserConstants.width = 1.9; % m
##' SApproachingRoadUserConstants.fAdaptToCrossingBehaviour = ...
##'     @AdaptSpeedToPassBehindCrossingRoadUser;
##' 
##' -- Found in SimulateCrossingScenario.m from line 102
##' SNonAdaptedApproachingRoadUserBehaviour = ...
##'   SApproachingRoadUserConstants.fGetApproachBehaviour(...
##'   VTimeStamp, SApproachingRoadUserConstants);
##'
##' @param VTimeStamp a sequence of time in second.
##' @param SApproachingRoadUserConstants a series of constant variables.
##' @return a S4 object, named SApproachingRoadUserBehaviour with five slots: 
##' VX, VY, VXdot, VHeading, and VbYieldingeHMI.
##' @examples
##' ## SSimulationConstants.timeStep
##' ## SSimulationConstants.endTime
##' ## SSimulationResults.VTimeStamp = VTimeStamp;
##' ## VTimeStamp = 0:(1/30):30;
##' timeStep   <- 1/30
##' endTime    <- 30 ## endTime    <- 2
##' VTimeStamp <- seq(0, endTime, by = timeStep)
##'
##' ## SApproachingRoadUserConstants.initialSpeed
##' ## SApproachingRoadUserConstants.initialTTA
##' ## SApproachingRoadUserConstants.decelerationOnsetTTA
##' ## SApproachingRoadUserConstants.stopAtDistanceFromConflict;
##' ## SApproachingRoadUserConstants.length;
##' ## SApproachingRoadUserConstants.eHMIOnsetTTA
##' 
##' ## initialSpeed <- 50/3.6; 
##' ## initialTTA   <- 4.58
##' ## decelerationOnsetTTA <- 4.58;
##' ## stopAtDistanceFromConflict <- 4
##' ## len <- 4.7
##' ## eHMIOnsetTTA <- NaN
##' 
##' SApproachingRoadUserConstants$initialSpeed               <- 50/3.6
##' SApproachingRoadUserConstants$initialTTA                 <- 4.58
##' SApproachingRoadUserConstants$decelerationOnsetTTA       <- 4.58
##' SApproachingRoadUserConstants$stopAtDistanceFromConflict <- 4
##' SApproachingRoadUserConstants$len                        <- 4.7
##' SApproachingRoadUserConstants$eHMIOnsetTTA               <- NaN
##' ans <- GetConstantDecelerationVehicleApproachBehaviour(VTimeStamp, SApproachingRoadUserConstants)
##' @export
GetConstantDecelerationVehicleApproachBehaviour <- function(VTimeStamp,
                                                            SApproachingRoadUserConstants)
{
    ## 1. travelling along the X axis
    n <- length(VTimeStamp);
    VY <- rep(0, n);
    initialSpeed <- SApproachingRoadUserConstants$initialSpeed
    initialTTA   <- SApproachingRoadUserConstants$initialTTA
    decelerationOnsetTTA <- SApproachingRoadUserConstants$decelerationOnsetTTA
    stopAtDistanceFromConflict <- SApproachingRoadUserConstants$stopAtDistanceFromConflict;
    len <- SApproachingRoadUserConstants$len;
    eHMIOnsetTTA <- SApproachingRoadUserConstants$eHMIOnsetTTA;
    ## -------------------------------
    ## get speed signal; 2. -- initial constant speed
    initialDistanceGap <- initialSpeed*initialTTA;  ## Distance
    VSpeed <- initialSpeed * rep(1, n)
    ## 3. -- deceleration onset
    decelerationOnsetDistanceGap <- initialSpeed * decelerationOnsetTTA;
    decelerationOnsetTime <- (initialDistanceGap - decelerationOnsetDistanceGap) / initialSpeed;
    idxDecelerationOnsetSample <- which(VTimeStamp >= decelerationOnsetTime)[1]
    ## 4. -- deceleration
    decelerationDistance <- decelerationOnsetDistanceGap - stopAtDistanceFromConflict;
    deceleration <- initialSpeed^2 / (2*decelerationDistance);
    term0 <- VTimeStamp[idxDecelerationOnsetSample:n] - decelerationOnsetTime
    VSpeed[idxDecelerationOnsetSample:n] <- initialSpeed - deceleration*term0
    VSpeed <- pmax(0, VSpeed);
    
    ## 5. get position signal
    term0 <- (VSpeed[1:(n-1)] + VSpeed[2:n])/2
    term1 <- diff(VTimeStamp) * term0
    term2 <- cumsum(c(0, term1))
    VX <- initialDistanceGap + len/2 - term2;

    ## SApproachingRoadUserBehaviour.VXdot
    ## SApproachingRoadUserBehaviour.VHeading
    ## SApproachingRoadUserBehaviour.VbYieldingeHMI
    VXdot <- -VSpeed           ## save also the speed signal for later use
    VHeading <- rep(1, n) * pi ## always heading West
    ## ------------------------------------
    ## eHMI section
    VbYieldingeHMI <- FALSE * rep(1, n); 
    if (is.nan(eHMIOnsetTTA)) {
        VTTA <- VX / VSpeed;
        idxeHMIOnsetSample <- which(VTTA <= eHMIOnsetTTA)[1]
        if (is.null(idxeHMIOnsetSample)) {
            VbYieldingeHMI[idxeHMIOnsetSample:n] <- TRUE; 
        }
    }
    ## --------------------------------------
    ## SApproachingRoadUserBehaviour = out
    ## --------------------------------------
    out <- new("SApproachingRoadUserBehaviour",
               VY = VY,
               VX = VX,
               VXdot = VXdot,
               VHeading = VHeading,
               VbYieldingeHMI = VbYieldingeHMI)
    return(out)
}

