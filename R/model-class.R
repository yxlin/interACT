### Behaviour  ----------------------------------------------------
##' An S4 class of Approaching/Crossing Road User Behaviour.
##' 
##' A Behaviour object is to describe the behaviour of a road user, his/her
##' speed, direction, and position etc.
##' 
##' @slot VX a vector. Get position signal
##' @slot VY a vector. Travelling along the X axis
##' @slot VXdot a vector. -VSpeed. Save also the speed signal for later use
##' @slot VHeading a vector. Always heading west
##' @slot VbYieldingeHMI a logical vector. From eHMI section.
##' @slot idxCrossingOnsetSample a integer vector.
##' @slot MCornerMatrices a 3-D array
##' @export
setClass("Behaviour", slots = c(
  VX = "numeric",
  VY = "numeric",
  VXdot = "numeric",
  VHeading = "numeric",
  VbYieldingeHMI = "numeric",
  idxCrossingOnsetSample = "numeric",
  MCornerMatrices = "array"))

## Pedestrian --------------------
##' An S4 class for a Pedestrian
##' 
##' A pedestrian has his/her country origin, whether crossing road 
##' ('Pedestrian crossing UK') or driving to turn right ('Driver right turn').
##' 
##' @slot sCountry a character string to represent which country.
##' @slot sScenarioName a character string.  
##' @slot fGetCrossingTrajectory a function. 
##' @slot clength a floating point scalar for crossing.
##' @slot cwidth a floating point scalar for crossing
##' @slot intersectionLaneWidth a floating point scalar.
##' @slot SModelSpecificConstants a list.
##' @slot alength a floating point scalar for approaching.
##' @slot awidth a floating point scalar for approaching.
##' @slot fGetApproachBehaviour a function linking to 
##' GetConstantDecelerationVehicleApproachBehaviour.m. This function seems 
##' independent of countries, pedestrian and driver.
##' @slot fAdaptToCrossingBehaviour a function
##' @slot initialSpeed a floating point scalar.
##' @slot initialTTA a floating point scalar
##' @slot decelerationOnsetTTA a floating point scalar 
##' @slot stopAtDistanceFromConflict a floating point scalar 
##' @slot eHMIOnsetTTA a NaN.
##' @slot desiredPostEncroachmentTime a floating point scalar
##' @slot maxAcceleration a floating point scalar
##' @export
setClass("Pedestrian", slots = c(
  ## Crossing constants
  sCountry      = "character",
  sScenarioName = "character",
  ## fGetCrossingOnsetTimePDF = "function",
  fGetCrossingTrajectory   = "function",
  clength = "numeric",
  cwidth  = "numeric",
  intersectionLaneWidth   = "numeric",
  SModelSpecificConstants = "list",
  ## Approaching constants
  alength = "numeric",
  awidth  = "numeric",
  fGetApproachBehaviour     = "function",
  fAdaptToCrossingBehaviour = "function",
  initialSpeed = "numeric",
  initialTTA  = "numeric",
  decelerationOnsetTTA = "numeric",
  stopAtDistanceFromConflict = "numeric",
  eHMIOnsetTTA = "numeric",
  desiredPostEncroachmentTime = "numeric",
  maxAcceleration = "numeric"))

setMethod(f="initialize", signature="Pedestrian",
          definition = function(.Object,
                                sCountry      = 'UK',  
                                sScenarioName = 'Pedestrian crossing UK', ...) 
{
    message('Create a pedestrian ...')
    ## Crossing constants------------------------
    ## GetScenarioConstants.m; Crossing
    .Object@sCountry      <- sCountry
    .Object@sScenarioName <- sScenarioName
    
    ## GetPedestrianCrossingTrajectory
    .Object@fGetCrossingTrajectory <- function(VTimeStamp, idx) 
    { 
      n <- length(VTimeStamp);
      VX <- rep(0, n);              ## pedestrian stays along y axis
      VHeading <- rep(1, n) * pi/2  ## with North heading
      
      initialDistanceFromCentreOfApproachingVehicleLane <- 
        .Object@SModelSpecificConstants$initialDistanceFromCentreOfApproachingVehicleLane  
      walkingSpeed <- .Object@SModelSpecificConstants$walkingSpeed
      
      ## get movement across road
      term0 <- pmax(0, VTimeStamp - VTimeStamp[idx]);
      VY <- -initialDistanceFromCentreOfApproachingVehicleLane + walkingSpeed*term0;
      
      out <- new("Behaviour",
                 VY = VY,
                 VX = VX,
                 VHeading = VHeading)
      return(out)
    }
            
    parameters <- threshold_distr_params('Pedestrian')
    ## Always type 6 == [[2]], instead of 5 == [[1]]
    .Object@SModelSpecificConstants <- switch(sCountry,
                                              UK = { parameters$UK[[2]] },
                                              JP = { parameters$JP[[2]] },
                                              { list() })
            
    .Object@clength <- .3  # m; crossing
    .Object@cwidth  <- .5  # m; crossing
    .Object@intersectionLaneWidth <- 2.85
    .Object@SModelSpecificConstants$walkingSpeed <- 1.4 # m/s
    .Object@SModelSpecificConstants$initialDistanceFromCentreOfApproachingVehicleLane <- 2 # m
    
    ## Approaching constants------------------------
    ## GetBaseApproachingRoadUserConstants.m
    .Object@alength <- 4.7  # m
    .Object@awidth  <- 1.9  # m
    ## From test_SimulateCrossingScenario.m
    .Object@initialSpeed <- 50/3.6  # m/s
    .Object@initialTTA  <- 4.58  # s
    .Object@decelerationOnsetTTA <- 4.58 # s
    .Object@stopAtDistanceFromConflict <- 4
    .Object@eHMIOnsetTTA <- NaN # no eHMI
    .Object@desiredPostEncroachmentTime <- 2 # s
    .Object@maxAcceleration <- 2 # m/s^2
    
    .Object@fGetApproachBehaviour <- function(VTimeStamp) 
    {
      n <- length(VTimeStamp); ## 1. travelling along the X axis
      VY <- rep(0, n);
      ## get speed signal; 2. -- initial constant speed
      initialDistanceGap <- .Object@initialSpeed*.Object@initialTTA;  ## Distance
      VSpeed <- .Object@initialSpeed * rep(1, n)
      ## 3. -- deceleration onset
      decelerationOnsetDistanceGap <- .Object@initialSpeed * .Object@decelerationOnsetTTA;
      decelerationOnsetTime <- (initialDistanceGap - decelerationOnsetDistanceGap) / .Object@initialSpeed;
      idxDecelerationOnsetSample <- which(VTimeStamp >= decelerationOnsetTime)[1]
      ## 4. -- deceleration
      decelerationDistance <- decelerationOnsetDistanceGap - .Object@stopAtDistanceFromConflict;
      deceleration <- .Object@initialSpeed^2 / (2*decelerationDistance);
      term0 <- VTimeStamp[idxDecelerationOnsetSample:n] - decelerationOnsetTime
      VSpeed[idxDecelerationOnsetSample:n] <- .Object@initialSpeed - deceleration*term0
      VSpeed <- pmax(0, VSpeed);
      ## 5. get position signal
      term1 <- diff(VTimeStamp) * ((VSpeed[1:(n-1)] + VSpeed[2:n])/2)
      term2 <- cumsum( c(0, term1) )
      VX <- initialDistanceGap + .Object@alength/2 - term2;
      VXdot <- -VSpeed   ## save also the speed signal for later use
      VHeading <- rep(1, n) * pi ## always heading West
      ## eHMI ------------------------------------##
      VbYieldingeHMI <- FALSE * rep(1, n);
      if (is.nan(.Object@eHMIOnsetTTA)) {
        VTTA <- VX / VSpeed;
        idxeHMIOnsetSample <- which(VTTA <= .Object@eHMIOnsetTTA)[1]
        if (is.null(idxeHMIOnsetSample)) {
          VbYieldingeHMI[idxeHMIOnsetSample:n] <- TRUE;
        }
      }
      out <- new("Behaviour",
                 VY = VY,
                 VX = VX,
                 VXdot = VXdot,
                 VHeading = VHeading,
                 VbYieldingeHMI = VbYieldingeHMI)
      return(out)
    }

    ## In GetBaseApproachingRoadUserConstants.m 
    ## fAdaptToCrossingBehaviour is linked to
    ## @AdaptSpeedToPassBehindCrossingRoadUser.m;
    .Object@fAdaptToCrossingBehaviour <- function(VTimeStamp, cBehaviour)
    { 
      ## adapt speed (if needed) of the approaching road user to pass 
      ## behind a crossing road user at a minimum post encroachment time or 
      ## later, and then regain original speed
      ## %% assumptions:
      ## - approacher is travelling along X axis, toward more negative 
      ## - conflict point is at origin
      
      n <- length(VTimeStamp)
      ## store unadapted behaviour locally
      aBehaviour <- .Object@fGetApproachBehaviour(VTimeStamp)
      VX    <- aBehaviour@VX;
      VXdot <- aBehaviour@VXdot;
      
      ## is there a need to adapt speed to the crossing road user?
      idxCrossingOnsetSample <- cBehaviour@idxCrossingOnsetSample
      
      term0 <- (VX - .5*.Object@alength) <= (.5*.Object@cwidth)
      term1 <- which(term0 == TRUE)
      if(pracma::isempty(term1)) {
        idxConflictPointArrivalSample <- NULL
      } else {
        idxConflictPointArrivalSample <- min(term1)
      }

      if(is.null(idxConflictPointArrivalSample)) {
        ## message("open-loop approach trajectory does not cross conflict point")
        bNeedToAdaptSpeed <- TRUE 
      } else {
        ## message("open-loop approach trajectory arrives at conflict point after the crossing road user")
        bNeedToAdaptSpeed <- idxConflictPointArrivalSample > idxCrossingOnsetSample
      }

      if (!bNeedToAdaptSpeed) return(NULL)
      
      ## get some basic info
      distanceGapAtCrossingDecision <- VX[idxCrossingOnsetSample] - 
      .5*.Object@alength - .5*.Object@cwidth;
      speedAtCrossingDecision <- -VXdot[idxCrossingOnsetSample];
      if (speedAtCrossingDecision < 0) stop("speedAtCrossingDecision < 0")
      
      ## find samples where crosser is not in approacher's path (for simplicity
      ## assuming that crosser is always perpendicular to approacher's path)
      term0 <- (cBehaviour@VY - .5*.Object@clength) >  .5*.Object@awidth
      term1 <- (cBehaviour@VY + .5*.Object@clength) < -.5*.Object@awidth;
      VbCrosserNotInApproachersPath <-  term0 | term1 

      idxCrosserExitsApproachersPath<- max(which(!VbCrosserNotInApproachersPath))
      if (pracma::isempty(idxCrosserExitsApproachersPath)) {
        stop('Crosser never exits path of approacher.')
      }
      
      ## get the time until the crossing road user has left the
      ## approacher's path, plus a post encroachment time
     timeToCrossWithMargin <- VTimeStamp[idxCrosserExitsApproachersPath] + 
  .Object@desiredPostEncroachmentTime - VTimeStamp[idxCrossingOnsetSample];

      ## get the acceleration needed to arrive at a reference point near the
      ## conflict point at precisely the time the crossing road user reaches 
      ## its reference point
      term0 <- (2 / timeToCrossWithMargin^2) * (distanceGapAtCrossingDecision - speedAtCrossingDecision*timeToCrossWithMargin)
      accelerationToApplyAtCrossingOnset <- min(.Object@maxAcceleration, term0);

      ## get duration for deceleration and possible standstill under different 
      ## possible situations
      if (accelerationToApplyAtCrossingOnset > 0) {
        term0 <- (.Object@initialSpeed - speedAtCrossingDecision)/ accelerationToApplyAtCrossingOnset
        durationForAccelerationDuringCrossing <- min(timeToCrossWithMargin, term0)
        durationForStandstill                 <- 0;
      } else { 
        timeToStandstill <- speedAtCrossingDecision / -accelerationToApplyAtCrossingOnset;
        ## decelerating to full stop
        if (timeToStandstill < timeToCrossWithMargin) {  
          durationForAccelerationDuringCrossing <- timeToStandstill;
          ## this is a bit crude
          durationForStandstill <- timeToCrossWithMargin - timeToStandstill; 
        } else {                 
          ## not decelerating to full stop
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
      VXdotdot[(idxCrossingOnsetSample+1):idxAccelerationDuringCrossingEndSample] <- -accelerationToApplyAtCrossingOnset
      
      idx0 <- (idxAccelerationDuringCrossingEndSample+1)
      idx1 <- idxStandstillEndSample
      if (idx0 > idx1) {
        ## message("VXdotdot stay the same")
      } else {
        cat("replace ", idx0, " to ", idx1, " with 0")
        VXdotdot[(idxAccelerationDuringCrossingEndSample+1):idxStandstillEndSample] <- 0;
      }
      VXdotdot[(idxStandstillEndSample+1):n] <- -v@maxAcceleration
      
      ## integrate to get speed and position
      term0 <- VXdot[idxCrossingOnsetSample];
      term1 <- VXdotdot[(idxCrossingOnsetSample+1):n];
      term2 <- VXdotdot[idxCrossingOnsetSample:(n-1)];
      term3 <- .5*(term1 + term2);
      term4 <- diff( VTimeStamp[idxCrossingOnsetSample:n] )
      term5 <- term3*term4;
      term6 <- cumsum(term5);
      term7 <- term0 + term6
      VXdot[(idxCrossingOnsetSample+1):n] <- term7
      VXdot <- pmax(VXdot, -.Object@initialSpeed);
      
      term0 <- VX[idxCrossingOnsetSample];
      term1 <- VXdot[(idxCrossingOnsetSample+1):n];
      term2 <- VXdot[idxCrossingOnsetSample:(n-1)];
      term3 <- .5*(term1 + term2);
      term4 <- diff( VTimeStamp[idxCrossingOnsetSample:n] )
      term5 <- term3*term4;
      term6 <- cumsum(term5);
      term7 <- term0 + term6
      VX[(idxCrossingOnsetSample+1):n] <- term7
      
      ## store adapted behaviour in output structure
      out <- new("Behaviour")
      out@VX <- VX
      out@VXdot <- VXdot
      out@VY <- aBehaviour@VY              ## Default value
      out@VHeading <- aBehaviour@VHeading  ## Default value  
      out@VbYieldingeHMI <- aBehaviour@VbYieldingeHMI  ## Default value  
      
      ## if there was a yielding eHMI, inactivate it from when the crosser has
      ## cleared the path
      out@VbYieldingeHMI[idxCrosserExitsApproachersPath:n] <- FALSE
      return(out)
    }

    return(.Object)
})

##' An S4 method for GetInitialPositionOfCrossingRoadUser
##' 
##' GetScenarioConstants.m spedifies four different fGetCrossingOnsetTimePDF's,
##' linking to GetModelCrossingOnsetTimePDF.m, which calls 
##' GetLongDistanceFromCrosserInitialPosToApproacherFront.m.
##' 
##' GetLongDistanceFromCrosserInitialPosToApproacherFront.m. then calls
##' GetInitialPositionOfCrossingRoadUser.m. 
##' 
##' @param x a Pedestrian or Driver object
##' @return a two-element list, storing initialX and initialY
##' @examples 
##' v <- new("Pedestrian", 
##'          sCountry = 'UK',
##'          sScenarioName = 'Pedestrian crossing UK')
##' ans <- GetInitialPositionOfCrossingRoadUser(v)
##' print(unlist(ans))
##' @rdname GetInitialPositionOfCrossingRoadUser-methods
##' @export
setGeneric("GetInitialPositionOfCrossingRoadUser", function(x, ... ) {
  warning("Class ", class(x), 
          " not defined for GetInitialPositionOfCrossingRoadUser")
  return(NULL)
})

##' @export
##' @rdname GetInitialPositionOfCrossingRoadUser-methods
setMethod("GetInitialPositionOfCrossingRoadUser", "Pedestrian", function (x) 
{
  ## 0:30 and 1 are dummy VTimeStamp and index, so they are OK.
  SDummyCrosserBehaviour <- x@fGetCrossingTrajectory(0:30, 1)
  out <- list(initialX=SDummyCrosserBehaviour@VX[1], 
              initialY=SDummyCrosserBehaviour@VY[1])
})


##' Found in GetLongDistanceFromCrosserInitialPosToApproacherFront
##' 
##' GetScenarioConstants.m spedifies four different fGetCrossingOnsetTimePDF's,
##' linking to GetModelCrossingOnsetTimePDF.m, which calls 
##' GetLongDistanceFromCrosserInitialPosToApproacherFront.m.
##' 
##' GetLongDistanceFromCrosserInitialPosToApproacherFront.m. then calls
##' GetInitialPositionOfCrossingRoadUser.m. 
##' @param x an object of Pedestrian or Driver
##' @param aBehaviour a Behaviour object storing SApproachingRoadUserBehaviour
##' @return a floating point vector
##' @examples
##' timeStep   <- 1/30
##' endTime    <- 30 ## endTime    <- 2
##' VTimeStamp <- seq(0, endTime, by = timeStep)
##' 
##' v0 <- new("Pedestrian", 
##'           sCountry = 'UK',
##'           sScenarioName = 'Pedestrian crossing UK')
##' 
##' aB <- v0@fGetApproachBehaviour(VTimeStamp)
##' ans <- GetLongDistanceFromCrosserInitialPosToApproacherFront(v0, aB)
##' 
##' @rdname GetLongDistanceFromCrosserInitialPosToApproacherFront-methods
##' @export
setGeneric("GetLongDistanceFromCrosserInitialPosToApproacherFront",  
           function(x, aBehaviour, ... ) 
{
  warning("Class ", class(x), 
          " not defined for GetLongDistanceFromCrosserInitialPosToApproacherFront")
  return(NULL)
})

##' @export
##' @rdname GetLongDistanceFromCrosserInitialPosToApproacherFront-methods
setMethod("GetLongDistanceFromCrosserInitialPosToApproacherFront", "Pedestrian",
          function (x, aBehaviour) 
{
    ## get initial longitudinal position of crossing road user
    initialXY <- GetInitialPositionOfCrossingRoadUser(x);
    ## get the distance to front of approacher
    out <- aBehaviour@VX - .5*x@alength - initialXY$initialX
    return(out)  ## VLongitudinalDistance
})

##' Found in GetModelCrossingOnsetTimePDF.m
##' 
##' Linking to fGetCrossingOnsetTimePDF.
##' 
##' @param x a Pedestrian or a Driver object
##' @param VTimeStampe a time vecotr
##' @param SApproachingRoadUserBehaviour a Behaviour object storing approaching
##' road user behaviour.
##' @return a vector perhaps?
##' @rdname GetModelCrossingOnsetTimePDF-methods
##' @export
setGeneric("GetModelCrossingOnsetTimePDF", function(x, ... ) {
  warning("Class ", class(x), " not defined for GetModelCrossingOnsetTimePDF")
  return(NULL)
})

##' @export
##' @rdname GetModelCrossingOnsetTimePDF-methods
setMethod("GetModelCrossingOnsetTimePDF", "Pedestrian", 
          function (x, VTimeStamp, SApproachingRoadUserBehaviour) 
{
    VLongitudinalDistance <- GetLongDistanceFromCrosserInitialPosToApproacherFront(x, SApproachingRoadUserBehaviour)
    
    speed <- -SApproachingRoadUserBehaviour@VXdot
    ehmi  <- SApproachingRoadUserBehaviour@VbYieldingeHMI
    p     <- x@SModelSpecificConstants  ## parameters
    
    out <- threshold_distr(VTimeStamp, VLongitudinalDistance, speed, ehmi, p)
    return(out)
})

## Driver --------------------



