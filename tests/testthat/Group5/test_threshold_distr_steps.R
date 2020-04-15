cat("\n------ Testing threshold_distr ------\n")
library(interACT)
library(data.table)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/VLongitudinalDistance.txt"
VLongitudinalDistance_octave <- as.vector(t(fread(pathname)))
distance <- VLongitudinalDistance_octave

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/SApproachingRoadUserBehaviour_VXdot.txt"
VXdot_octave <- as.vector(t(fread(pathname)))
speed <- -VXdot_octave

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/SApproachingRoadUserBehaviour_VbYieldingeHMI.txt"
VbYieldingeHMI_octave <- as.vector(t(fread(pathname)))
ehmi <- VbYieldingeHMI_octave

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/SModelSpecificConstants.bin"
p <- R.matlab::readMat(pathname)



timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
ts <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian", VTimeStamp = ts)
SNonAdaptedApproachingRoadUserBehaviour <- v@fGetApproachBehaviour(ts)

distance_R <- GetLongDistanceFromCrosserInitialPosToApproacherFront(v, SNonAdaptedApproachingRoadUserBehaviour)
speed_R <- -SNonAdaptedApproachingRoadUserBehaviour@VXdot
ehmi_R <- SNonAdaptedApproachingRoadUserBehaviour@VbYieldingeHMI
p_R <- v@SModelSpecificConstants

all.equal(distance, distance_R)
all.equal(speed, speed_R)
all.equal(ehmi, ehmi_R)
unlist( p_R )
unlist( p$SModelSpecificConstants[,,1] )

dt <- mean(diff(ts)); ## A hack, the dt must be constant for now
ttcdot <- pracma::gradient(distance_R / speed_R) / dt
## 1. ttcdot
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/ttcdot.txt"
ttcdot_octave <- as.vector(t(fread(pathname)))
all.equal(ttcdot, ttcdot_octave)

cdist <- sign(distance_R) * abs(distance_R)^(p_R$speedexp)
cspeed <- sign(speed_R) *abs(speed_R)^(p_R$speedexp);
ttcs <- cdist/cspeed;

## Make an even more mangled "TTC" by adding a contribution
## of ttcdot and of eHMI
ttcs <- ttcs + (p$dotcoeff[1,1]) * (ttcdot + 1.0);
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/ttcs.txt"
ttcs_octave <- as.vector(t(fread(pathname)))
all.equal(ttcs, ttcs_octave)
tmp <- cbind(ttcs, ttcs_octave)
head(tmp, 100)
tail(tmp)


names(p$SModelSpecificConstants[,,1])

# if !all(!ehmi)
# ttcs = ttcs + double(ehmi) * p.eHMIEffectiveTTAIncrement;
# end
ttcs[ ttcs < p$SModelSpecificConstants[,,1]$passed.ttc[1,1] ] <- Inf
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/ttcs2.txt"
ttcs2_octave <- as.vector(t(fread(pathname)))
all.equal(ttcs, ttcs2_octave)
tmp <- cbind(ttcs, ttcs2_octave)
head(tmp, 100)
tail(tmp)


maxs <- cummax(ttcs)
maxs[ is.nan(maxs) ] <- Inf

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/maxs0.txt"
maxs0_octave <- as.vector(t(fread(pathname)))
all.equal(maxs, maxs0_octave)
tmp <- cbind(maxs, maxs0_octave)
head(tmp, 100)
tail(tmp)

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/maxs1.txt"
maxs1_octave <- as.vector(t(fread(pathname)))
all.equal(maxs, maxs1_octave)
tmp <- cbind(maxs, maxs1_octave)
head(tmp, 100)
tail(tmp)


logncdf_scipy <- function(x, m, s) {
  sigma <- m
  mu <- log(s)
  wtf <- plnorm(x, mu, sigma)
}

done <- logncdf_scipy(maxs, m = p$SModelSpecificConstants[,,1]$thm[1,1],
       s = p$SModelSpecificConstants[,,1]$ths[1,1])
done

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/logncdf_scipy.txt"
logncdf_scipy_octave <- as.vector(t(fread(pathname)))
all.equal(done, logncdf_scipy_octave)
tmp <- cbind(done, logncdf_scipy_octave)
head(tmp)
tail(tmp)

dts <- c(done[1], diff(done)) / dt
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/dts0.txt"
dts0_octave <- as.vector(t(fread(pathname)))
all.equal(dts, dts0_octave)
tmp <- cbind(dts, dts0_octave)
head(tmp, 250)
tail(tmp)

term0 <- logncdf_scipy(ts, p$SModelSpecificConstants[,,1]$rtm[1,1], p$SModelSpecificConstants[,,1]$rts[1,1]);

pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/term0.txt"
term0_octave <- as.vector(t(fread(pathname)))
all.equal(term0, term0_octave)
tmp <- cbind(term0, term0_octave)
head(tmp)
tail(tmp)

term1 <- diff(term0);
latency <- c(term1, 0) / dt 
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/latency0.txt"
latency0_octave <- as.vector(t(fread(pathname)))
all.equal(latency, latency0_octave)
tmp <- cbind(latency, latency0_octave)
head(tmp)
tail(tmp)

latency <- c( rep(0, length(ts) - 1), latency)
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/latency1.txt"
latency1_octave <- as.vector(t(fread(pathname)))
all.equal(latency, latency1_octave)
tmp <- cbind(latency, latency1_octave)
head(tmp)
tail(tmp)

my_conv <- function(a, b, shape = "full") {
  la <- length(a)
  lb <- length(b)
  ly <- la + lb - 1;
  
  out <- pracma::conv(a, b)

  if (shape == "same") {
    idx <- ceiling( (ly-la)/2)
    out <- out[(idx+1):(idx+la)]
  }
  return(out)
}

dts1 <- my_conv(dts, latency, shape = "same")
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/dts1.txt"
dts1_octave <- as.vector(t(fread(pathname)))
all.equal(dts1, dts1_octave)
tmp <- cbind(dts1, dts1_octave)
head(tmp)
tail(tmp)

dts2 <- dts1 * (1- p$SModelSpecificConstants[,,1]$slack[1,1]) + p$SModelSpecificConstants[,,1]$slack[1,1]
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/dts2.txt"
dts2_octave <- as.vector(t(fread(pathname)))
all.equal(dts2, dts2_octave)
tmp <- cbind(dts2, dts2_octave)
head(tmp)
tail(tmp)

dts3 <- dts2 *dt;
pathname <- "/media/yslin/MERLIN/Documents/interACT/tests/testthat/Group5/dts3.txt"
dts3_octave <- as.vector(t(fread(pathname)))
all.equal(dts3, dts3_octave)
tmp <- cbind(dts3, dts3_octave)
head(tmp)
tail(tmp)
