Sys.setenv("R_TESTS" = "")
## Workaround for the error,
## "cannot open file 'startup.Rs': No such file or directory" in Windows 10

library(testthat)
library(data.table)
library(R.matlab)
library(interACT)

cat("Start running testthat in the directory: ")
cat(getwd(), "\n")

cat("\n========================== Group 0 tests ==========================\n")
test_file(path = "Group0/test_fGetApproachBehaviour.R")
test_file(path = "Group0/test_GettingStarted.R")

cat("\n========================== Group 1 tests ==========================\n")
test_file(path = "Group1/test_fGetCrossingTrajectory.R")

cat("\n========================== Group 2 tests ==========================\n")
test_file(path = "Group2/test_GetInitialPositionOfCrossingRoadUser.R")
test_file(path = "Group2/test_GetLongDistanceFromCrosserInitialPosToApproacherFront.R")

cat("\n========================== Group 3 tests ==========================\n")
test_file(path = "Group3/test_threshold_distr.R")

cat("\n========================== Group 4 tests ==========================\n")
test_file(path = "Group4/test_SimulateCrossingScenario.R")
