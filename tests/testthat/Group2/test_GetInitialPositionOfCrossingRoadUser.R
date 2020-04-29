cat("\n------ Testing GetInitialPositionOfCrossingRoadUser ------\n")
## library(interACT)
cat("Pedestrian\n")
v <- new("Pedestrian", 
         sCountry = 'UK',
         sScenarioName = 'Pedestrian crossing UK')
ans <- GetInitialPositionOfCrossingRoadUser(v)
print(unlist(ans))

