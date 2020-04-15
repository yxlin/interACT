Sys.setenv("R_TESTS" = "")
## Workaround for the error,
## "cannot open file 'startup.Rs': No such file or directory" in Windows 10

library(testthat)
library(interACT)

cat("Start running testthat in the directory: ")
cat(getwd(), "\n")

cat("\n========================== Group 0 tests ==========================\n")
test_file(path = "Group0/test_GetConstantDecelerationVehicleApproachBehaviour.R")

cat("\n========================== Group 1 tests ==========================\n")
test_file(path = "Group1/test_GetPedestrianCrossingTrajectory.R")
