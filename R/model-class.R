### Approaching Road User ----------------------------------------------------
##' An S4 class of Approaching Road User Behaviour.
##' 
##' Details of the class...
##' 
##' @slot VY a vector. Travelling along the X axis
##' @slot VX a vector. Get position signal
##' @slot VXdot a vector. -VSpeed. Save also the speed signal for later use
##' @slot VHeading a vector. Always heading West
##' @slot VbYieldingeHMI a logical vector. From eHMI section.
##' @slot idxCrossingOnsetSample a integer vector.s
##' @export
setClass("Behaviour", slots = c(
  VY = "numeric",
  VX = "numeric",
  VXdot = "numeric",
  VHeading = "numeric",
  VbYieldingeHMI = "numeric",
  idxCrossingOnsetSample = "numeric"))

##' An S4 method for GetInitialPositionOfCrossingRoadUser
##' 
##' Details of the method
##' 
##' @rdname GetInitialPositionOfCrossingRoadUser-methods
##' @export
setGeneric("GetInitialPositionOfCrossingRoadUser", function(x, idx, ... ) {
  warning("Class ", class(x), " not defined for GetInitialPositionOfCrossingRoadUser")
  return(NULL)
})

##' Found in GetLongDistanceFromCrosserInitialPosToApproacherFront
##' 
##' @param x an object of Pedestrian or Driver
##' @return a floating point scalar
##' @examples
##' timeStep   <- 1/30
##' endTime    <- 30 ## endTime    <- 2
##' VTimeStamp <- seq(0, endTime, by = timeStep)
##' @rdname GetLongDistanceFromCrosserInitialPosToApproacherFront-methods
##' @export
setGeneric("GetLongDistanceFromCrosserInitialPosToApproacherFront", 
           function(x, ... ) 
{
  warning("Class ", class(x), " not defined for GetLongDistanceFromCrosserInitialPosToApproacherFront")
  return(NULL)
})

##' Found in GetModelCrossingOnsetTimePDF.m
##' 
##' Linking to fGetCrossingOnsetTimePDF.
##' 
##' @param SCrossingRoadUserConstants a S4 class declared in model-class.R
##' @return a vector perhaps
##' @rdname GetModelCrossingOnsetTimePDF-methods
##' @export
setGeneric("GetModelCrossingOnsetTimePDF", function(x, ... ) {
  warning("Class ", class(x), " not defined for GetModelCrossingOnsetTimePDF")
  return(NULL)
})

## Pedestrian and Driver --------------------
##' An S4 class for a Pedestrian
##' 
##' Details of the class...
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
##' @slot fGetApproachBehaviour a function
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
  fGetApproachBehaviour = "function",
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
    
    ## fGetApproachBehaviour ---------------------------------
    ## Does not depend on countries or pedestrian or driver
    .Object@fGetApproachBehaviour <- function(VTimeStamp) 
    {
      ## 1. travelling along the X axis
      n <- length(VTimeStamp);
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

    ##
    .Object@fAdaptToCrossingBehaviour <- function(VTimeStamp)
    { 
      ## adapt speed (if needed) of the approaching road user to pass 
      ## behind a crossing road user at a minimum post encroachment time or 
      ## later, and then regain original speed
      ## %% assumptions:
      ## - approacher is travelling along X axis, toward more negative 
      ## - conflict point is at origin
      
      ## store unadapted behaviour locally
      unadaptedBehaviour <- .Object@fGetApproachBehaviour(VTimeStamp)
      VX <- unadaptedBehaviour@VX;
      VXdot <- unadaptedBehaviour@VXdot;
      
      ## is there a need to adapt speed to the crossing road user?
      # idxCrossingOnsetSample <- SCrossingRoadUserBehaviour.idxCrossingOnsetSample;
    }

    return(.Object)
})


##' @export
##' @rdname GetInitialPositionOfCrossingRoadUser-methods
setMethod("GetInitialPositionOfCrossingRoadUser", "Pedestrian", function (x) 
{
    SDummyCrosserBehaviour <- x@fGetCrossingTrajectory(0:30, 1)
    out <- list(initialX=SDummyCrosserBehaviour@VX[1], 
                initialY=SDummyCrosserBehaviour@VY[1])
})

##' @export
##' @rdname GetLongDistanceFromCrosserInitialPosToApproacherFront-methods
setMethod("GetLongDistanceFromCrosserInitialPosToApproacherFront", "Pedestrian",
          function (x, SApproachingRoadUserBehaviour) 
{
    ## get initial longitudinal position of crossing road user
    initialXY <- GetInitialPositionOfCrossingRoadUser(x);
    ## get the distance to front of approacher
    out <- SApproachingRoadUserBehaviour@VX - .5*x@alength - initialXY$initialX
    return(out)  ## VLongitudinalDistance
})

##' @export
##' @rdname GetModelCrossingOnsetTimePDF-methods
setMethod("GetModelCrossingOnsetTimePDF", "Pedestrian", 
          function (x, VTimeStamp, SApproachingRoadUserBehaviour) 
{
    VLongitudinalDistance <- GetLongDistanceFromCrosserInitialPosToApproacherFront(x, SApproachingRoadUserBehaviour)
    speed <- -SApproachingRoadUserBehaviour@VXdot
    ehmi <- SApproachingRoadUserBehaviour@VbYieldingeHMI
    p <- x@SModelSpecificConstants
    out <- threshold_distr(ts, distance, speed, ehmi, p)
    return(out)
})


setClass("Driver", slots = c(
  VY = "numeric",
  VX = "numeric",
  VXdot = "numeric",
  VHeading = "numeric",
  VbYieldingeHMI = "numeric"
))




