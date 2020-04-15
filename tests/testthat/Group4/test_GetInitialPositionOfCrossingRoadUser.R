cat("\n------ Testing GetPedestrianCrossingTrajectory ------\n")
library(interACT)
v0 <- new("SCrossingRoadUserConstants", 
sCountry = 'UK',
sScenarioName = 'Pedestrian crossing UK',
sScenarioType = "Pedestrian")


ans <- GetInitialPositionOfCrossingRoadUser(v0)

v1 <- new("Pedestrian", 
          sCountry = 'UK',
          sScenarioName = 'Pedestrian crossing UK')
v1
ans <- GetInitialPositionOfCrossingRoadUser(v1)
