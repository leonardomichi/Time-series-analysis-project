################################################################################
##
## File:    TSA-Total renewable energy production
## 
## Purpose: produzione mensile di energia rinnovabile 
##          
## 
################################################################################

################################################################################
## Clean
################################################################################

####
rm(list = ls())


################################################################################
## Libraries and functions
################################################################################

library(zoo)
library(forecast)
library(lmtest)       ## For better model print
library(tsoutliers)   ## For outliers
library(urca)         ## For UR tests
# library(uroot) ## For UR test
library(FinTS)      ## For ArchTest (from RForge)

source("/Users/leonardomichi/Desktop/statistica economica/script/TSA-Useful-Functions.R")
source("/Users/leonardomichi/Desktop/statistica economica/script/CalendarEffects-Student-Functions.R")
source("/Users/leonardomichi/Desktop/statistica economica/script/TSA-Predict-Student-Functions.R")


################################################################################
## Read data
################################################################################

#### Settings
file.data <- "/Users/leonardomichi/Desktop/Michi/_Renewable_Energy_Production_and_Consumption_by_Source.xlsx"
var <- "Total Renewable Energy Production"
name <- var
unit <- "Trillion Btu"

#### Read data
data <- openxlsx::read.xlsx(xlsxFile = file.data)
#### Select data
ind <- data$Total.Renewable.Energy.Production
#### Extract variables
date <- as.Date(data$Month, origin = "1899-12-30")
Total.Renewable.Energy.Production <- as.numeric(data$Total.Renewable.Energy.Production)

#### ts() object
start <- as.numeric( c( format(date[1], "%Y"), format(date[1], "%m") ) )
y <- ts( data = Total.Renewable.Energy.Production, start = start, frequency = 12)
#y<- ts( data = log(Total.Renewable.Energy.Production), start = start, frequency = 12)
#y <- ts( data = sqrt(Total.Renewable.Energy.Production), start = start, frequency = 12)

################################################################################
## External variables
################################################################################

#### Calendar effects
cal <- .calendarEffects(time = date, country = "us")
#### Select
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh"), drop = FALSE]
cal <- cal[, c("sh"), drop = FALSE]
# cal <- cal[, c("Mon", "Wed", "Thu", "Sun", "sh"), drop = FALSE]
# cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "eh"), drop = FALSE]
# cal <- cal[, c("wd", "eh"), drop = FALSE]
cal <- as.matrix(cal)

#### Handmade drift (sometimes useful in tso())
drift <- cbind(drift = 1 : NROW(y))

################################################################################
## Preliminary analysis
################################################################################

#### Ts plot, acf, pacf of the original series
par(mfrow = c(3,1))
x <-y
#x<-diff(y)
plot(x, type = "l", main = name, ylab = unit)
Acf(x = x, type = "correlation", na.action = na.pass, lag.max = 60, main = name)
Acf(x = x, type = "partial",     na.action = na.pass, lag.max = 60, main = name)

#### DF/ADF tests
######### DF/ADF tests with trend
cat("\n-----------------------------------------------------------------
  Unit root analysis\n")
#### The following rows show how to make DF test in practice.
##   DF is however not recommended because gives severely biased results 
##   in case the DGP is more complex than the equation estimated by DF.
##   This time series prove this (compare DF with ADF results)
df.1 <- ur.df(y = y, type = "trend", lags = 0, selectlags = "Fixed")
df.2 <- ur.df(y = y, type = "drift", lags = 0, selectlags = "Fixed") 
df.3 <- ur.df(y = y, type = "none",  lags = 0, selectlags = "Fixed") 
#### Cell 4
##   (DGP:   RW + drift; 
##    Model: AR(1) + trend (+ other possible stationary terms))
adf.1 <- ur.df(y =y, type = "trend", lags = 24, selectlags = "AIC")
cat("\n-----\nTest1: ADF with trend\n")
print( adf.1@teststat )
print( adf.1@cval )
##   Outcome: Phi2 A, Phi3 A, Tau3 A => no trend, go to Cell 2   
#### Cell 2
##   (DGP:   RW; 
##    Model: AR(1) + constant (+ other possible stationary terms))
adf.2 <- ur.df(y =y, type = "drift", lags = 24, selectlags = "AIC")
## Following two lines in case one needs to test Cell 3
# adf.2 <- .ur.drift(y = y, lags = 24, selectlags = "AIC") 
# cat("\n-----\nTest2: T with sure presence of drift\n")
cat("\n-----\nTest2: ADF with drift\n")
print( adf.2@teststat )
print( adf.2@cval )
##   Outcome: Phi1 A, tau2 A => no drift, go to cell 1
#### Cell 1
##   (DGP:   RW; 
##    Model: AR(1) (+ other possible stationary terms))
adf.3 <- ur.df(y = y, type = "none", lags = 24, selectlags = "AIC")
cat("\n-----\nTest2: ADF with none\n")
print( adf.3@teststat )
print( adf.3@cval )
##   Outcome: tau1 A => UR!
#### Who is the killer: d = 1 and/or D = 1?
##   Repeat the tests on diff(y) and/or diff(y, 12) and decide.
x <- diff(y, 1)
x <- diff(y, 12)
adf.1 <- ur.df(y = x, type = "trend", lags = 24, selectlags = "AIC")
cat("\n-----\nTest1: ADF with trend\n")
print( adf.1@teststat )
print( adf.1@cval )
##   Outcome: Phi2 R, Phi3 R, Tau3 R => no UR   
#### There is uncertainty between d = 1 or D = 1.
##   Arima() trials reveal that D = 1 is the right choice

#### KPSS tests (to do)
cat("\n-----\nKPSS with tau\n")
kpss.1 <- ur.kpss(y = y, type = "tau", lags = "long", use.lag = NULL)
print( kpss.1@teststat )
print( kpss.1@cval )
cat("\n-----\nKPSS with mu\n")
kpss.2 <- ur.kpss(y = y, type = "mu", lags = "long", use.lag = NULL)
print( kpss.2@teststat )
print( kpss.2@cval )

#### HEGY tests (to do)
hegy.1 <- uroot::hegy.test(x =y, deterministic = c(1, 0, 0),
maxlag = 12, lag.method = "BIC", # lag.method = c("fixed", "AIC", "BIC", "AICc"), 
pvalue = "RS",        # pvalue = c("RS", "bootstrap", "raw"), 
rs.nobsreg = 15)


################################################################################
## ARIMA(p,d,q)x(P,D,Q)_S modeling
################################################################################
#ARIMA(0,0,0)(0,1,0)[12] AIC=6198.59   AICc=6198.59   BIC=6202.96
#ARIMA(0,1,0)(0,1,0)[12] AIC=5664.34   AICc=5664.35   BIC=5668.71
#ARIMA(1,0,0)(0,1,0)[12] AIC=5618.05   AICc=5618.07   BIC=5626.79
#ARIMA(2,0,0)(0,1,0)[12] AIC=5616.11   AICc=5616.15   BIC=5629.21
#ARIMA(3,0,0)(0,1,0)[12] AIC=5599.47   AICc=5599.54   BIC=5616.94
#ARIMA(2,0,1)(0,1,0)[12] AIC=5605.65   AICc=5605.72   BIC=5623.12
#ARIMA(2,0,1)(0,1,1)[12] AIC=5360.18   AICc=5360.28   BIC=5382.02
#ARIMA(2,0,1)(1,1,0)[12] AIC=5479.38   AICc=5479.48   BIC=5501.22
#ARIMA(1,0,2)(0,1,1)[12] AIC=5357.41   AICc=5357.51   BIC=5379.25
#ARIMA(1,1,2)(0,1,1)[12] AIC=5348.74   AICc=5348.84   BIC=5370.57
#ARIMA(0,1,2)(0,1,1)[12] AIC=5346.99   AICc=5347.06   BIC=5364.45
#ARIMA(2,1,0)(0,1,1)[12] AIC=5355.64   AICc=5355.71   BIC=5373.11
#### ARIMA (no external regressors)
xreg <- NULL
fit <- Arima(y = y,
             order = c(0, 1, 2), seasonal = list(order = c(0, 1, 1)),
             xreg = xreg, include.constant = F)
print(summary(fit))
print(lmtest::coeftest(fit))
fit1 <- fit
## Only in case of transformed variable
# llstats.adj1 <- .loglik(fit = fit, g = "sqrt") 

#### ARIMA + external regressors
xreg <- cal
fit <- Arima(y = y, 
             order = c(0, 1, 2), seasonal = list(order = c(0, 1, 1)),
             xreg = xreg, include.constant = F)
print(summary(fit))
print(coeftest(fit))
fit2 <- fit


################################################################################
## ARIMA modeling with anomalies
################################################################################

#####################################
## ARIMA
#####################################

#### Copy model
fit <- fit1
#### Extract settings
settings <- .Arima.settings(fit = fit)
#### Prepare xreg
xreg <- NULL
xreg <- if ( settings$include.drift ) { cbind(drift, xreg) } else { xreg }
#### Fit
fit <- tso(y = y, xreg = xreg,
           types = c("AO", "LS", "TC"), delta = 0.7, cval = 4,
           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
           # tsmethod = "auto.arima",
           # args.tsmethod = list(allowdrift = false, ic = "bic", trace = true) )
           tsmethod = "arima",
           args.tsmethod = list( order = settings$order, seasonal = settings$seasonal) )
fit1.o <- fit
#### Reporting
print(fit)
plot(fit)
#### Extract outlier effects
oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
                         weights = FALSE)
## Plot weighted effects
# plot(x = date, y = rowSums(oeff), type = "l")
#### Estimate again
xreg <- as.matrix( oeff )
fit <- Arima(y = y,
             order = settings$order, seasonal = settings$seasonal,
             include.constant = settings$include.constant,
             xreg = xreg)
## Only in case of transformed variable
# llstats.adj3 <- .loglik(fit = fit, g = "log")
fit3 <- fit
print(summary(fit))
print(coeftest(fit))

#####################################
## ARIMA + external regressors
#####################################

#### Copy model
fit <- fit2
#### Extract settings
settings <- .Arima.settings(fit = fit)
#### Prepare xreg
xreg <- cal
xreg <- if ( settings$include.drift ) { cbind(drift, xreg) } else { xreg }
#### Fit
fit <- tso(y = y, xreg = xreg,
           types = c("AO", "LS", "TC"), delta = 0.7, cval = 4,
           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
           # tsmethod = "auto.arima",
           # args.tsmethod = list(allowdrift = false, ic = "bic", trace = true) )
           tsmethod = "arima",
           args.tsmethod = list( order = settings$order, seasonal = settings$seasonal) )
fit2.o <- fit
### Reporting
print(fit)
plot(fit)
### Extract elements
## Outlier effects
oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
                         weights = FALSE)
## Plot weighted effects
# plot(x = data1$Time, y = rowSums(oeff), type = "l")
#### Estimate again
xreg <- cbind(cal, oeff)
fit <- Arima(y = y,
             order = settings$order, seasonal = settings$seasonal,
             include.constant = settings$include.constant,
             xreg = xreg)
## Only in case of transformed variable
# llstats.adj4 <- .loglik(fit = fit, g = "log")
fit4 <- fit
print(summary(fit))
print(coeftest(fit))

################################################################################
## Diagnostics
################################################################################

#### Select the model
fit <- fit4

#### Useful quantities
npar1  <- NROW(fit$coef)                            ## Number of parameters
lag1   <- npar1 + c(1, 2, 5, 10, 15, 20)
res1   <- residuals(fit)                            ## Residuals
resst1 <- ( res1 - mean(res1) ) / sqrt(fit$sigma2)  ## Standardized residuals

#### Ts plot, acf, pacf, Ljung-Box of residuals
par(mfrow = c(3,1))
main <- "residuals"
x1 <- res1
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
Acf(x = x1, type = "partial",     lag.max = 60, na.action = na.pass, main = main)

#lb <- Box.test(x = res1, lag = 15, type = "Ljung-Box", fitdf = NROW(fit$coef))
cat("\n", paste("Ljung-Box of", main, "at different lags\n") )
lb <- mapply(FUN = Box.test, lag = lag1, 
             MoreArgs = list(x = x1, type = "Ljung-Box", fitdf = npar1))[1:3, , drop = FALSE]
print(rbind(lag = lag1, lb))

#### Ts plot, acf of residuals^2
par(mfrow = c(3,1))
main <- "residuals^2"
x1 <- res1^2
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
Acf(x = x1, type = "partial",     lag.max = 60, na.action = na.pass, main = main)

#### Ts plot, acf of |residuals|
par(mfrow = c(3,1))
main <- "|residuals|"
x1 <- abs(res1)
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
Acf(x = x1, type = "partial",     lag.max = 60, na.action = na.pass, main = main)

#### Another diagnostic: the ARCH test
cat("\n-----------------------------------------------------------------
  ARCH test on demeaned log-returns\n")
lag <- c(1, 2, 3, 6, 12, 24)
at <- mapply(FUN = ArchTest, lags = lag, 
             MoreArgs = list(x = x1, demean = TRUE))
print(rbind(lag = lag, at[1:3,]))
#### Comment: mild heterosk. detected only by ACF(|res|); transforming data by 
##   sqrt() or log() produces even worse results.

.trsf.test(fit=fit4)

#### Unconditional distribution of residuals
## Plot
par(mfrow = c(1,2))
hist(x = resst1, breaks = 25, freq = FALSE, main = "residuals", xlab = "")
x1 <- seq(from = min(resst1), to = max(resst1)+1, length.out = 100) 
lines(x = x1, y = dnorm(x = x1, mean = 0, sd = 1), col = "red")
qqnorm(y = resst1, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
abline(a = 0, b = 1, col = "red")
## Test of normality 
cat("\nTest of normality\n")
print( shapiro.test(x = res1 ) )

################################################################################
## Forecasts
################################################################################



################################
## Ex-post forecasts: all made 1-step ahead
################################

#### Settings
J  <- 12                                              ## How many ex-post forecast to compute
H  <- 1                                               ## Forecasting horizon
t1 <- .predict.t1(nobs = NROW(y), J = J, n.ahead = H) ## 1st obs in the ex-post period (needed below)

#### No external regressors
pred1.1 <- .predict(object = fit1, n.ahead = H, t = t1, y = y,
                    fixed.n.ahead = TRUE)

#### If we have external regressors
newxreg <- cal
pred2.1 <- .predict(object = fit2, n.ahead = H, t = t1, y = y, xreg = newxreg,
                    fixed.n.ahead = TRUE)

#### If we have outliers
newxreg <- .oeff.4.predict(object = fit1.o, n.ahead = 0)
pred3.1 <- .predict(object = fit3, n.ahead = H, t = t1, y = y, xreg = newxreg,
                    fixed.n.ahead = TRUE)

#### If we have external regressors and outliers
x2 <- .oeff.4.predict(object = fit2.o, n.ahead = 0)
newxreg <- as.matrix(cbind(cal, x2))
pred4.1 <- .predict(object = fit4, n.ahead = H, t = t1, y = y, xreg = newxreg,
                    fixed.n.ahead = TRUE)

#### Naive
# predn.1 <- .predict.naive(fit = fit4, J = J, n.ahead = H, g = "log10") ## Only in case of transformed variable
predn.1 <- .predict.naive(fit = fit4, J = J, n.ahead = H)

#### Conversion of predictions to the original scale (if needed) and computation of bands
# x1 <- .pred.bands(pred = pred1.1, alpha = 0.05, g = "log10")   ## Only in case of transformed variable; repeat for all
x1 <- .pred.bands(pred = pred1.1, alpha = 0.05)
x2 <- .pred.bands(pred = pred2.1, alpha = 0.05)
x3 <- .pred.bands(pred = pred3.1, alpha = 0.05)
x4 <- .pred.bands(pred = pred4.1, alpha = 0.05)

#### Error Measures
em1.1  <- .ErrorMeasures(y = y, fit = x1$mean, naive = predn.1)
em2.1  <- .ErrorMeasures(y = y, fit = x2$mean, naive = predn.1)
em3.1  <- .ErrorMeasures(y = y, fit = x3$mean, naive = predn.1)
em4.1  <- .ErrorMeasures(y = y, fit = x4$mean, naive = predn.1)
emn.1  <- .ErrorMeasures(y = y, fit = predn.1, naive = predn.1)
## Print
ErrorMeas <- data.frame(
  model = c("Arima", "Arima + Calendar", "Arima + Outliers", "Arima + Calendar + Outliers", "Naive"),
  h = H,
  rbind( em1.1, em2.1,em3.1, em4.1, emn.1, deparse.level = 0 ) )
print( ErrorMeas )

#### Plot
ind  <- (NROW(y) - J + 1) : NROW(y) ## To extract last data from y
par(mfrow = c(1,1))
ylim <- range(
  x1$lower, x1$upper, x2$lower, x2$upper,
  x3$lower, x3$upper, x4$lower, x4$upper, predn.1 )
time <- date[ind]
plot(x = time, y = y[ind], ylim = ylim,
     main = "(Ex-post) Forecasts of the past 12 months", xlab = "time", ylab = unit)
#lines(x = time, y = x1$mean,  col = "red")
#lines(x = time, y = x2$mean,  col = "blue")
#lines(x = time, y = x3$mean,  col = "violet")
lines(x = time, y = x4$mean,  col = "red")
lines(x = time, y = predn.1,  col = "green", lty = "solid")
lines(x = time, y = x4$lower, col = "cyan", lty = "dotted")
lines(x = time, y = x4$upper, col = "cyan", lty = "dotted")
legend("topleft", legend = c("pred","bands","naive"), fill=c("red","cyan","green"), bty = "n", cex = 0.9)


################################
## Ex-ante (genuine) forecasts: from 1 to H steps ahead
################################

#### Settings
H  <- 12      ## Forecasting horizon
t1 <- NROW(y) ## Last obs in the info set

#### No external regressors
newxreg <- NULL
pred1 <- .predict(object = fit1, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  fixed.n.ahead = FALSE)

#### If we have external regressors
## Generate time
time1 <- .extend.time(x = date, n.ahead = H, by = "month")
## Generate calendar effects
x1 <- .calendarEffects(time = time1)[, colnames(cal), drop = FALSE]
newxreg <- as.matrix(rbind(cal, x1))
pred2 <- .predict(object = fit2, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  fixed.n.ahead = FALSE)

#### If we have outliers
x2 <- .oeff.4.predict(object = fit1.o, n.ahead = H)
newxreg <- x2
pred3 <- .predict(object = fit3, n.ahead = H, t = t1, y = y, xreg = newxreg,
                 fixed.n.ahead = FALSE)
#newxreg <- NULL
#pred3 <- .predict(object = fit1, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  #fixed.n.ahead = FALSE)

#### If we have external regressors and outliers
x2 <- .oeff.4.predict(object = fit2.o, n.ahead = H)
newxreg <- as.matrix( cbind( rbind(cal, x1), x2) )
pred4 <- .predict(object = fit4, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  fixed.n.ahead = FALSE)

#### Naive
# predn <- .predict.naive(fit = fit4, J = H, n.ahead = H, g = "log10") ## Only in case of transformed variable
predn <- .predict.naive(fit = fit4, J = H, n.ahead = H) 

#### Bands
# x1 <- .pred.bands(pred = pred1, alpha = 0.05, g = "log10")   ## Only in case of transformed variable
x1 <- .pred.bands(pred = pred1, alpha = 0.05)
x2 <- .pred.bands(pred = pred2, alpha = 0.05)
x3 <- .pred.bands(pred = pred3, alpha = 0.05)
x4 <- .pred.bands(pred = pred4, alpha = 0.05)

#### Print
print( cbind(t = x1$t, pred1 = x1$mean, pred2 = x2$mean, pred3 = x3$mean, pred4 = x4$mean, 
             naive = predn) )

#### Plot
par(mfrow = c(1,1))
ylim <- range(
  x1$lower, x1$upper, x2$lower, x2$upper,
  x3$lower, x3$upper, x4$lower, x4$upper, predn )
time <- time1
plot(x = time, y = x4$mean, type = "l",
     main = "(Ex-ante) Forecasts of the next 12 months", xlab = "time", ylab = unit,
     ylim = ylim)
#lines(x = time, y = x2$mean,  col = "blue")
#lines(x = time, y = x3$mean,  col = "violet")
lines(x = time, y = x4$mean,  col = "red")
lines(x = time, y = x4$lower, col = "cyan", lty = "dotted")
lines(x = time, y = x4$upper, col = "cyan", lty = "dotted")
lines(x = time, y = predn, col = "green")
legend("topleft", legend = c("pred","bands","naive"), fill=c("red","cyan","green"), bty = "n", cex = 0.9)

