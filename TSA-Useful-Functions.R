################################################################################
##
## File:    TSA-Useful-Functions.R
## 
## Purpose: Some useful functions.
##
## Created: 2020.10.01
##
## Version: 2022.03.15
## 
################################################################################

.circle <- function(win = 1.3)
{
  ##############################################################################
  ## Arguments
  ##  win:     (numeric[1]) Window size.
  ## Value
  ##  Plot a unit radius circle.  
  ##############################################################################

  #### Initialize plot
  plot(x = c(-win, win), c(-win, win), type = "n", xlab = "", ylab = "")
  
  #### Prepare "circle data"
  radius <- 1
  theta <- seq(from = 0, to = 2 * pi, length = 200)
  
  #### Draw the circle
  lines(x = radius * cos(theta), y = radius * sin(theta))
  
  #### Add axes
  abline(h = 0)
  abline(v = 0)
  
  #### Answer
  invisible(NULL)
}
# ------------------------------------------------------------------------------


.acf.theoretical <- function(model, lag.max, 
  ylim = c(-1, 1), pacf = FALSE, plot = TRUE)
{
  #### Compute
  S <- ifelse( NROW(model$s) > 0, model$s[1], 12) 
  model <- .sarma2larma(ar = model$ar, ma = model$ma, 
    sar = model$sar, sma = model$sma, s = S)
  acf <- ARMAacf(ar = model$ar, ma = model$ma, lag.max = lag.max, pacf = pacf)
  #### Plot
  if (plot)
  {
    ind <- (1 + !pacf) : NROW(acf)
    x1 <- acf[ind]  
    main <- paste0("Theoretical ", ifelse(pacf, "PACF", "ACF"))
    ylab <- paste0(ifelse(pacf, "Partial ", ""), "ACF")
    plot(x1, type = "h", main = main, xlab = "Lag", ylab = ylab, 
      ylim = ylim)
    abline(a = 0, b = 0)
  }
  #### Answer
  acf
}
# ------------------------------------------------------------------------------


################################################################################
## Play with polynomials
################################################################################

#### Merge non stationary polynomials 
.diff2poly <- function(d, ds, s)
{
  #### init
  x1 <- 1
  #### d
  ind <- if ( d > 0 ) { 1 : d } else { NULL }
  for (i in ind)
  {
    x2 <- c(1, -1)
    x1 <- pracma::polymul(x1, x2)
  }
  #### ds
  ind <- if ( ds > 0 ) { 1 : ds } else { NULL }
  for (i in ind)
  {
    x2 <- c(1, numeric(s-1), -1)
    x1 <- pracma::polymul(x1, x2)
  }
  #### Answer
  x1
}
# ------------------------------------------------------------------------------


#### Convert seasonal parameters to polynomial
.seas2poly <- function(ps, s)
{
  #### 
  nps <- NROW(ps)
  if ( nps > 0 )
  { 
    ind <- seq(from = s, by = s, length.out = nps)
    x1 <- numeric(s * nps)
    x1[ind] <- ps
    c(1, x1)
  }
  else
  {
    1
  }
}
# ------------------------------------------------------------------------------


#### Merge short and seasonal components 
## (1 + a(1) L + ... + a(p) L^p) * (1 + a*(1) L^s + ... + a*(ps) L^(s * ps))
.long <- function(p, ps, s)
{
  #### Settings
  np  <- NROW( p )
  nps <- NROW( ps )
  
  #### 
  cp  <- if ( np > 0 ) { c(1, p) } else { 1 }
  cps <- .seas2poly(ps, s)
  
  #### Answer
  pracma::polymul(cp, cps)
}
# ------------------------------------------------------------------------------


#### Merge AR(p) and SAR(P); the same for MA(q) and SMA(Q) 
## (parameters are R-style)
.sarma2larma <- function(ar = NULL, ma = NULL, sar = NULL, sma = NULL, s = 12)
{
  #### Adjust
  if ( NROW(ar) > 0 ) { ar <- -ar }
  if ( NROW(sar) > 0 ) { sar <- -sar }
  #### Model
  list(
    ar = -.long(p = ar, ps = sar, s = s)[-1],
    ma = .long(p = ma, ps = sma, s = s)[-1] )
}
# ------------------------------------------------------------------------------


.parm <- function(
  model = list(d = 0, ds = 0, s = 12, 
    ar = NULL, ma = NULL, sar = NULL, sma = NULL))
{
  #### Orders
  p <- NROW(model$ar)
  d <- ifelse( NROW(model$d) > 0, model$d[1], 0) 
  q <- NROW(model$ma)
  ps <- NROW(model$sar)
  ds <- ifelse( NROW(model$ds) > 0, model$ds[1], 0) 
  qs <- NROW(model$sma)
  s <- ifelse( NROW(model$s) > 0, model$s[1], 12) 

  #### Long form of parameters
  ar.ns <- .diff2poly(d = d, ds = ds, s = s)
  if ( NROW(model$ar) > 0 ) { model$ar <- -model$ar }
  if ( NROW(model$sar) > 0 ) { model$sar <- -model$sar }
  ar.s <- .long(p = model$ar, ps = model$sar, s = s)
  ar <- -pracma::polymul(ar.ns, ar.s)[-1]
  ma <- .long(p = model$ma, ps = model$sma, s = s)[-1]
  
  #### Answer
  list(ar = ar, ma = ma)
}
# ------------------------------------------------------------------------------


################################################################################
## Simulations
################################################################################

#### Simulate Stationary ARMA(p,q) with zero mean
.ma.sim <-
function(model, innov, init = NULL)
{
  ##############################################################################
  ## Arguments
  ##  model: (list)
  ##   $ma: (numeric[q])
  ##  innov: (numeric[nobs])
  ##  init: (list)
  ##   $ma: (numeric[q]) if not given, a zero vector is used.
  ##   $ma must be sorted from the oldest to the most recent:
  ##    x[-order+1, ..., 0].
  ## Value
  ##  Simulated time series.
  ## Remarks
  ##  No checks for roots.  
  ##############################################################################

  #### Extract orders
  q <- NROW(model$ma)
  #### Copy
  x <- innov
  if (q > 0)
  {
    #### Initialize
    x1 <- c(rep.int(0, q), init$ma)
    init$ma <- x1[ (NROW(x1)-q+1) : NROW(x1)]
    x <- c(init$ma, x)
    #### Make
    x <- stats::filter(x = x, filter = c(1, model$ma), method = "convolution", 
      sides = 1)[-(1:q)]
  }
  #### Answer
  x
}
# ------------------------------------------------------------------------------


.ar.sim <-
function(model, innov, init = NULL)
{
  ##############################################################################
  ## Arguments
  ##  model: (list)
  ##   $ar: (numeric[p])
  ##  innov: (numeric[nobs])
  ##  init: (list)
  ##   $ar: (numeric[p]) if not given, a zero vector is used.
  ##   $ar must be sorted from the oldest to the most recent:
  ##    x[-order+1, ..., 0].
  ## Value
  ##  Simulated time series.
  ## Remarks
  ##  No checks for roots.  
  ##############################################################################

  #### Extract orders
  p <- NROW(model$ar)  
  #### Copy
  x <- innov
  #### AR
  if (p > 0)
  { 
    x <- stats::filter(x = x, filter = model$ar, method = "recursive", 
      init = rev(init$ar))
  }
  #### Answer
  x
}
# ------------------------------------------------------------------------------


.arma.sim <-
function(model, innov, init = NULL)
{
  ##############################################################################
  ## Arguments
  ##  model: (list)
  ##   $ar: (numeric[p])
  ##   $ma: (numeric[q])
  ##  innov: (numeric[nobs])
  ##  init: (list)
  ##   $ar: (numeric[p]) if not given, a zero vector is used.
  ##   $ma: (numeric[q]) if not given, a zero vector is used.
  ##   $ar and $ma must be sorted from the oldest to the most recent:
  ##    x[-order+1, ..., 0].
  ## Value
  ##  Simulated time series.
  ## Remarks
  ##  No checks for roots.  
  ##############################################################################

  #### Copy
  x <- innov
  #### MA
  x <- .ma.sim(model = model, innov = x, init = init)
  #### AR
  .ar.sim(model = model, innov = x, init = init)
}
# ------------------------------------------------------------------------------


.arima.sim <- function(
  model = list(d = 0, ds = 0, s = 12, 
    constant = NULL, ar = NULL, ma = NULL, sar = NULL, sma = NULL), 
  innov, init = NULL)
{
  #### Orders
  model <- .parm(model = model) 
  constant <- model$constant  

  #### Init
  pp <- NROW(model$ar) 
  qq <- NROW(model$ma)
  if (qq > 0)
  {
    x1 <- c(rep.int(0, qq), init$ma)
    x1 <- x1[ (NROW(x1)-qq+1) : NROW(x1)]
    init$ma <- x1
  }
  if (pp > 0)
  {
    x1 <- c(rep.int(0, pp), init$ar)
    x1 <- x1[ (NROW(x1)-pp+1) : NROW(x1)]
    init$ar <- x1
  }
  
  #### Simulate ARMA
  x <- .arma.sim(model = model, innov = innov, init = init)
  
  #### Add mean
  constant <- constant[1]
  if (NROW(constant) > 0 && d + ds <= 1)
  {
    x <- constant / (1 - sum(model$ar)) + x    
  }
      
  #### Answer
  x
}
# ------------------------------------------------------------------------------


.init <- function(model, y, innov, t)
{
  #### Model
  p <- model$d + NROW(model$ar) + (model$ds + NROW(model$sar)) * model$s 
  q <- NROW(model$ma) + NROW(model$sma) * model$s 
  #### Extract
  ar <- ma <- NULL
  if (p > 0)
  {
    ar <- y[(t-p):t]
  }
  ma <- innov[(t-q):t]
  
  #### Answer
  list(ar = ar, ma = ma)
}
# ------------------------------------------------------------------------------


################################################################################
## Functions to plot AR, MA roots
################################################################################

.arma.roots <- function(fit)
{
  #### Coefficients
  coef <- fit$coef
  ar <- coef[substr(names(coef), 1, 2) == "ar"]
  ma <- coef[substr(names(coef), 1, 2) == "ma"]
  sar <- coef[substr(names(coef), 1, 3) == "sar"]
  sma <- coef[substr(names(coef), 1, 3) == "sma"]
  S <- fit$arma[5]
  
  #### Long form of parameters
  long <- .sarma2larma(ar = ar, ma = ma, sar = sar, sma = sma, s = S)
  #### Roots
  ar.r <- polyroot(c(1, -long$ar))
  ma.r <- polyroot(c(1, long$ma))
  
  #### Answer
  list(coef = list(ar = ar, ma = ma, sar = sar, sma = sma), 
    coef.long = long, 
    root = list(ar = ar.r, ma = ma.r))
}
# ------------------------------------------------------------------------------


.arma.roots.model <- function(model)
{
  #### Coefficients
  ar <- model$ar
  ma <- model$ma
  sar <- model$sar
  sma <- model$sma
  S <- model$s
  
  #### Long form of parameters
  long <- .sarma2larma(ar = ar, ma = ma, sar = sar, sma = sma, s = S)
  #### Roots
  ar.r <- polyroot(c(1, -long$ar))
  ma.r <- polyroot(c(1, long$ma))
  
  #### Answer
  list(coef = list(ar = ar, ma = ma, sar = sar, sma = sma), 
    coef.long = long, 
    root = list(ar = ar.r, ma = ma.r))
}
# ------------------------------------------------------------------------------


################################################################################
## Extract Arima settings
################################################################################

.constant.type <- function(fit)
{
  if (any( names(fit$coef) == "drift")) {"drift"} 
  else if (any( names(fit$coef) == "mean") ) {"mean"} 
  else if (any( names(fit$coef) == "intercept") ) {"intercept"} 
  else {NULL}
}
# -----------------------------------------------------------------------------


.Arima.settings <- function(fit)
{
  ####
  constant.type <- .constant.type(fit = fit)
  list(
    constant.type = constant.type,
    include.constant = NROW(constant.type) > 0,
    include.drift = NROW(constant.type) > 0 && constant.type == "drift", 
    order = fit$arma[c(1,6,2)],
    seasonal = list(order = fit$arma[c(3,7,4)]) )
}
# -----------------------------------------------------------------------------


.Arima.parm <- function(fit)
{
  #### Extract
  d <- fit$arma[6]; D <- fit$arma[7]; S <- fit$arma[5]
  coef <- fit$coef
  ind.ar   <- substr(names(coef), 1, 2) == "ar"
  ind.ma   <- substr(names(coef), 1, 2) == "ma"
  ind.sar  <- substr(names(coef), 1, 3) == "sar"
  ind.sma  <- substr(names(coef), 1, 3) == "sma"
  ind.beta <- !(ind.ar | ind.ma | ind.sar | ind.sma)
  ar  <- coef[ ind.ar ]
  ma  <- coef[ ind.ma ]
  sar <- coef[ ind.sar ]
  sma <- coef[ ind.sma ]
  beta <- coef[ ind.beta ]  
  sig2 <- fit$sigma2
  vcov <- fit$var.coef
  vcov.xreg <- vcov[ind.beta, ind.beta]
  vcov.arma <- vcov[!ind.beta, !ind.beta]
  #### Answer
  list(d = d, D = D, S = S, ar = ar, ma = ma, sar = sar, sma = sma, 
    xreg = beta, sigma2 = sig2, 
    vcov.arma = vcov.arma, vcov.xreg = vcov.xreg)
}
# -----------------------------------------------------------------------------


################################################################################
## Transformation diagnostic
################################################################################

.trsf.test <- function(fit, msg = "")
{
  #### Load
  require(sandwich)
  #### Extract
  mod <- fit
  fit <- fitted(mod)
  res <- residuals(mod)
  #### Fit
  lm1 <- lm( log(res^2) ~ log(fit) )
  
  #### Coef
  coef   <- coef(lm1)
  #### vcov
  vcov    <- vcovHC(x = lm1, type = "const")
  vcovHC  <- vcovHC(x = lm1)
  vcovHAC <- vcovHAC(x = lm1)
  #### Test
  coef  <- coef["log(fit)"]
  se    <- sqrt(vcov["log(fit)", "log(fit)"])   
  seHC  <- sqrt(vcovHC["log(fit)", "log(fit)"]) 
  seHAC <- sqrt(vcovHAC["log(fit)", "log(fit)"])
  tab <- data.frame(
   estimate     = rep.int(coef, 3),  
   se           = rep.int(se, 3),
   H0           = c(          "gamma = 0",          "gamma = 1",        "gamma = 2"   ),  
   tstat        = c(    (coef - 0) / se,    (coef - 1) / se,   (coef - 2) / se  ), 
   "se(HC)"     = rep.int(seHC, 3),
   "tstat(HC)"  = c(  (coef - 0) / seHC,  (coef - 1) / seHC,  (coef - 2) / seHC ), 
   "se(HAC)"    = rep.int(seHAC, 3),
   "tstat(HAC)" = c( (coef - 0) / seHAC, (coef - 1) / seHAC, (coef - 2) / seHAC ), 
   check.names = FALSE )   
  
  #### Print
  if ( msg != "" )
  {
    cat(msg)
    print( tab )
  }
  #### Answer
  tab 
}
# ------------------------------------------------------------------------------


################################################################################
## UR test with RW + drift under H0
################################################################################

.ur.drift <- function(y, 
  lags = 1, selectlags = c("Fixed", "AIC", "BIC")) 
{
  selectlags<-match.arg(selectlags)
  if (ncol(as.matrix(y)) > 1) 
    stop("\ny is not a vector or univariate time series.\n")
  if (any(is.na(y))) 
    stop("\nNAs in y.\n")
  y <- as.vector(y)
  lag <- as.integer(lags)
  if (lag < 0) 
    stop("\nLags must be set to an non negative integer value.\n")
  CALL <- match.call()
  DNAME <- deparse(substitute(y))
  x.name <- deparse(substitute(y))
  lags <- lags + 1
  z <- diff(y)
  n <- length(z)
  x <- embed(z, lags)
  z.diff <- x[, 1]
  z.lag.1 <- y[lags:n]
  tt <- lags:n
  if (lags > 1) {
    if(selectlags!="Fixed"){
      critRes<-rep(NA, lags)
      for(i in 2:(lags)){
        z.diff.lag = x[, 2:i]
        result <- lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)  
        critRes[i]<-AIC(result, k = switch(selectlags, "AIC" = 2, "BIC" = log(length(z.diff))))
      }
      lags<-which.min(critRes)
    }
    z.diff.lag = x[, 2:lags]
    result <- lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)
    tau <- coef(summary(result))[2, 3]
    teststat <- as.matrix(tau)
    colnames(teststat) <- c('t')
  }
  else {
    result <- lm(z.diff ~ z.lag.1 + 1)
    tau <- coef(summary(result))[2, 3]
    teststat <- as.matrix(tau)
    colnames(teststat) <- c('t')
  }
  rownames(teststat) <- 'statistic'
  testreg <- summary(result)
  res <- residuals(testreg)
  cval.t2 <- qt(df = result$df.residual, p = c(0.01, 0.05, 0.10))
  cvals <- rbind(t2 = cval.t2)
  testnames <- 't'
  colnames(cvals) <- c("1pct", "5pct", "10pct")
  rownames(cvals) <- testnames
  
  ####
  new("ur.df", y = y, cval=cvals, lags=lag, teststat = teststat, 
    testreg=testreg, res=res, test.name="UR(drift) Test")
}
# ------------------------------------------------------------------------------

