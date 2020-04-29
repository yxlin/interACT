cat("\n------ Testing SimulateCrossingScenario------\n")
timeStep   <- 1/30
endTime    <- 30 
ts <- seq(0, endTime, by = timeStep)
nsim <- 50
v <- new("Pedestrian")
ans <- SimulateCrossingScenario(v, ts, nsim)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/result4.bin"
d <- R.matlab::readMat(pathname)
dd <- as.vector(d$result4)
testthat::expect_equivalent(ts, dd)


xpath <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/xdata.bin"
ypath <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/ydata.bin"
ypath1 <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group4/ydata1.bin"
dx <- R.matlab::readMat(xpath)
dy <- R.matlab::readMat(ypath)
dy1 <- R.matlab::readMat(ypath1)
xd <- as.vector(dx$xdata)
yd <- as.vector(dy$ydata)
yd1 <- as.vector(dy1$ydata1)

c_binWidth <- .5
histogramArea <- c_binWidth * nsim
xdata <- ts
ydata <- ans$VCrossingOnsetPDF * histogramArea

par(mfrow=c(1,2))
plot(xd, yd, type='l')
plot(xdata, ydata, type = 'l')
par(mfrow=c(1,1))


SApproacherBehaviour <- v@fGetApproachBehaviour(ts)
VLongitudinalDistance <- GetLongDistanceFromCrosserInitialPosToApproacherFront(v, SApproacherBehaviour)
VApproacherApparentTTA <- VLongitudinalDistance / (-SApproacherBehaviour@VXdot);

par(mfrow=c(1,2))
plot(xdata, yd1, type = 'l')
plot(xdata, VApproacherApparentTTA, type = 'l')
par(mfrow=c(1,1))
