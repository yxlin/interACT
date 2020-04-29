cat("\n------ Testing classes ------\n")
library(interACT)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
tvector <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian", VTimeStamp = tvector)
SNonAdaptedApproachingRoadUserBehaviour <- v@fGetApproachBehaviour(tvector)
SDummyCrosserBehaviour <- v@fGetCrossingTrajectory(0:30, 1)

ans <- GetInitialPositionOfCrossingRoadUser(v)
ans <- GetLongDistanceFromCrosserInitialPosToApproacherFront(v, SNonAdaptedApproachingRoadUserBehaviour)



## To be removed
v <- new("SApproachingRoadUserConstants")
ans <- v@fGetApproachBehaviour(VTimeStamp)

v0 <- new("SCrossingRoadUserConstants")
v0
v1 <- new("SCrossingRoadUserConstants", 
         sScenarioName = 'Pedestrian crossing Japan',
         sScenarioType = "Pedestrian")

v1

v2 <- new("SCrossingRoadUserConstants", 
          sScenarioName = 'Pedestrian crossing Japan',
          sScenarioType = "Driver")
v2

v3 <- new("SCrossingRoadUserConstants", 
          sScenarioName = 'Pedestrian crossing Japan',
          sScenarioType = "Animal")
v3

ans <- threshold_distr_params(); ans$thm
ans <- threshold_distr_params(model='d2p_uk_6'); ans$thm

ans <- threshold_distr_params(model='d2d_uk_5'); ans$thm
ans <- threshold_distr_params(model='d2d_uk_6'); ans$thm

ans <- threshold_distr_params(model='d2p_jp_5'); ans$thm
ans <- threshold_distr_params(model='d2p_jp_6'); ans$thm

ans <- threshold_distr_params(model='d2d_jp_5'); ans$thm
ans <- threshold_distr_params(model='d2d_jp_6'); ans$thm

library(interACT)
v4 <- new("SCrossingRoadUserConstants", 
          sCountry = 'TW',
          sScenarioName = 'Pedestrian crossing Japan',
          sScenarioType = "Pedestrian")


v5 <- new("SCrossingRoadUserConstants", 
          sCountry = 'UK',
          sScenarioName = 'Pedestrian crossing Japan',
          sScenarioType = "Pedestrian")
v5

v6 <- new("SCrossingRoadUserConstants", 
          sCountry = 'JP',
          sScenarioName = 'Pedestrian crossing Japan',
          sScenarioType = "Pedestrian")
v6
