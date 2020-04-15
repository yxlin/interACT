##' Implementation of the threshold distribution model (TDM) described in
##' interACT D2.2
##' 
##' @param ts a time vector with a fixed interval
##' @param distance a double vector storing distance
##' @param speed a double vector storing speed
##' @param ehmi a logical vector 
##' @param p a list storing parameters
##' @importFrom pracma gradient
##' @importFrom pracma conv
##' @importFrom stats plnorm
##' @export
threshold_distr <- function(ts, distance, speed, ehmi, p) {
  # % Matlab/Octave logncdf has different parameterizations
  # % than Scipy's, so let's reimplement here.
  logncdf_scipy <- function(x, m, s) { return(plnorm(x, log(s), m)) }
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

  ## A hack, the dt must be constant for now; ts must have regular interval
  dt <- mean(diff(ts));
  ## Compute ttcdot from non transformed TTC
  ttcdot <- ( pracma::gradient(distance / speed) ) / dt; ## time gradient
  ## Apply exponentiation to speed and distance
  cdist  <- sign(distance) * abs(distance)^p$distexp;
  cspeed <- sign(speed) * abs(speed)^p$speedexp;
  ttcs   <- cdist / cspeed;
  
  # % Make an even more mangled "TTC" by adding a contribution
  # % of ttcdot and of eHMI
  ttcs <- ttcs + p$dotcoeff *(ttcdot + 1.0);
  
  ## TODO
  ## if !all(!ehmi) {ttcs <- ttcs + double(ehmi) * p$eHMIEffectiveTTAIncrement;}
  
  ## Make the passed ttc "inf" and take the cumulative maximum
  ttcs[ ttcs < p$passed_ttc ] <- Inf
  maxs <- cummax(ttcs);
  maxs[ is.nan(maxs) ] <- Inf ## Remove NaN for conv to work

  done <- logncdf_scipy(maxs, p$thm, p$ths)
  dts  <- c(done[1], diff(done)) / dt
  
  ## Convolve with latency
  latency <- c( diff( logncdf_scipy(ts, p$rtm, p$rts) ), 0 ) / dt 
  latency <- c( rep(0, length(ts) - 1), latency )
  dts <- my_conv(dts, latency, shape = "same")
  dts <- dts * (1 - p$slack) + p$slack
  dts <- dts * dt;
  return(dts)
}

## set parameters for threshold distribution models, fitted to data as
## described in interACT D2.2
##' @export
threshold_distr_params <- function(model = 'Pedestrian') {
  d2p_uk_5 <- list('thm' = 0.47886432836153353,
                   'ths' = 3.49498931740459,
                   'rtm' = 0.7693740525755062, 
                   'rts'= 0.9160943663999054,
                   'passed_ttc' = -0.251416426430464, 
                   'dotcoeff' = 0.0,
                   'distexp' = 1.0,
                   'speedexp' = 1.0, 
                   'slack' = 0.0)
  
  d2p_uk_6 <- list('thm' = 0.4219035625019795, 
                   'ths' = 4.604284781402084, 
                   'rtm' = 0.6470487299544856,
                   'rts' = 1.03999398444242, 
                   'passed_ttc' = -0.10503683487561853, 
                   'dotcoeff' = 1.6249806732685348, 
                   'distexp' = 1.0, 
                   'speedexp' = 1.0,
                   'slack' = 0.0)
  
  d2d_uk_5 <- list('thm' = 0.34345340675376046, 
                   'ths' = 6.821717656522221,
                   'rtm' = 0.6470989818015007, 
                   'rts' = 1.0067922845255772, 
                   'passed_ttc' = 0.8666666669091226, 
                   'dotcoeff' = 0.0, 
                   'distexp' = 1.0, 
                   'speedexp' = 1.0,
                   'slack' = 0.0)
  
  d2d_uk_6 <- list('thm' = 0.5141993584822321,
                   'ths' = 8.635758525695698, 
                   'rtm' = 0.5328107685380989,
                   'rts' = 1.1082819328763274,
                   'passed_ttc' = 1.0402530856294006, 
                   'dotcoeff' = 0.9174863569115824, 
                   'distexp' = 1.0, 
                   'speedexp' = 1.0,
                   'slack' = 0.0)
  
  d2p_jp_5 <- list('thm' = 0.5589969462081049, 
                   'ths' = 4.243776907863043, 
                   'rtm' = 1.001957512815376, 
                   'rts' = 1.0278649132796112,
                   'passed_ttc' = -0.34681042271537055, 
                   'dotcoeff' = 0.0, 
                   'distexp' = 1.0,
                   'speedexp' = 1.0, 
                   'slack' = 0.0)
  
  d2p_jp_6 <- list('thm' = 0.3773877967584669,
                   'ths' = 6.146336790661355, 
                   'rtm' = 0.6825412727161024,
                   'rts' = 1.3911814147270432,
                   'passed_ttc' = 0.04877930518571419, 
                   'dotcoeff' = 2.8809582827213043, 
                   'distexp' = 1.0, 
                   'speedexp' = 1.0,
                   'slack' = 0.0)
  
  d2d_jp_5 <- list('thm' = 0.30050473034447395, 
                   'ths' = 8.202226658849227, 
                   'rtm' = 0.7252798263605301,
                   'rts' = 0.9131978916730896,
                   'passed_ttc' = 0.46836784379232466, 
                   'dotcoeff' = 0.0, 
                   'distexp' = 1.0, 
                   'speedexp' = 1.0,
                   'slack' = 0.0)
  
  d2d_jp_6 <- list('thm' = 0.4600046444730229, 
                   'ths' = 11.423740676615315, 
                   'rtm' = 0.5800485325491597, 
                   'rts' = 1.0328847001214858, 
                   'passed_ttc' = 0.5784893641120539, 
                   'dotcoeff' = 0.8902191084796054, 
                   'distexp' = 1.0, 
                   'speedexp' = 1.0,
                   'slack' = 0.0)
  
  ptype <- list(UK = list(d2p_uk_5, d2p_uk_6),
                JP = list(d2p_jp_5, d2p_jp_6))
  
  dtype <- list(UK = list(d2d_uk_5, d2d_uk_6),
                JP = list(d2d_jp_5, d2d_jp_6))
  
  ## after model, the left-hand side is treated as a character string to match
  ## what the user enters to the model variable. The right-hand side is the 
  ## internal variable / function that is passed to the out variable.
  out <- switch(model,
                Pedestrian = ptype,
                Driver     = dtype)
  return(out)
  
}
  