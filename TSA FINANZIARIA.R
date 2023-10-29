################################################################################
##
## File:    TSA-MCD-20221110.R
## 
## Purpose: G.MI (Generali S.P.A.) daily data analysis.
##
## Created: 2016.10.31
##
## Version: 2022.11.11
## 
################################################################################

################################################################################
## Clean
################################################################################

####
rm(list = ls())


################################################################################
## Loading
################################################################################

#### Libraries
library(zoo)
library(tseries)  
library(sandwich)
library(lmtest)
library(urca)     ## For unit root
library(rugarch)  ## For GARCH models
library(FinTS)    ## For ArchTest (download from RForge)
library(car)
library(forecast) 
library(xts)      ## For time stamps
library(quantmod) ## For downloading data

#### Functions
source("/Users/leonardomichi/Desktop/statistica economica/script/TSA-Predict-Student-Functions.R")
source("/Users/leonardomichi/Desktop/statistica economica/script/TSA-Finance-Functions.R")


################################################################################
## Download data
################################################################################

#### Symbol
# symbol <- "G.MI" ## "WMT", "KO"
# ### Get data
#data <- getSymbols(Symbols = symbol, src = "yahoo", auto.assign = FALSE,
#   from = "1962-01-01")
# #### Adjust to a data.frame
# colnames(data) <- gsub(x = colnames(data), pattern = paste0(symbol, "."),
#   replacement = "")
# data <- data.frame(Date = index(data), data, check.names = FALSE)
# ### Write
# file <- file.path("")
# write.table(x = data, file = file, quote = FALSE, sep = "\t", na = ".",
#   row.names = FALSE, col.names = TRUE)


################################################################################
## Input
################################################################################

#### Data
file.data <- "/Users/leonardomichi/Desktop/statistica economica/data/G.MI-2.csv"


################################################################################
## Read data
################################################################################

#### Read
data <- read.table(file = file.data, header = TRUE, sep = ",", dec=".",
                   check.names = FALSE, comment.char = "", na.strings = ".")

#### Extract period
ind   <- as.Date(x = "2012-12-30") <= as.Date(x = data$Date)
data  <- data[ind, , drop = FALSE]

#### Add variables
data <- data.frame(data, 
                   cc.ret = c(NA, diff(log(data$`Adj Close`))), 
                   gkVol = .garmanklass(data = data, sd = TRUE),
                   check.names = TRUE)
data <- data[-1, , drop = FALSE]

#### Extract variables
time  <- as.Date(x = data$Date)
yc    <- data$Close
yclog <- log(yc)
y     <- data$Adj.Close
ylog  <- log(y)

################################################################################
## Analysis of prices
################################################################################

#### Auxiliary quantities
nobs <- NROW(y)

#### Plots
par(mfrow = c(2,2))
plot(x = time, y = yc,    main = "Close",        xlab = "", ylab = "", type = "l")
plot(x = time, y = yclog, main = "Ln(close)",    xlab = "", ylab = "", type = "l")
plot(x = time, y = y,     main = "AdjClose",     xlab = "", ylab = "", type = "l")
plot(x = time, y = ylog,  main = "Ln(AdjClose)", xlab = "", ylab = "", type = "l")

#### Serial correlation
par(mfrow = c(2,1))
Acf(x = ylog, lag.max = 100, type = "correlation", main = "Price")
Acf(x = ylog, lag.max = 100, type = "partial", main = "Price")
## Apparent non-stationarity

#### Comments: 
##      Since a.y. 2017-2018: Ignore points 1 and 2 below, because new 
##      Yahoo-Finance data are already adjusted for splits.
##   1. Unadjusted prices (open, high, low, close) may be affected by stock 
##      splits: the jump of Close in 1992 is due to a 3:1 (new:old) split. 
##      An additional split happened in 1999 (2:1).
##   2. Being adjusted for splits, adjusted prices do not show abrupt changes. 
##      Note that such prices are adjusted not only for splits but also 
##      for the payment of dividends at specific dates. Explain why. 
##   3. The log scales allows to appreciate price changes better than levels.
##   4. From this point on we work on log(Adjusted).
##   5. Apparent non-stationarity.

######### ADF test procedure
cat("\n-----------------------------------------------------------------
  Unit root analysis\n")
#### (DGP:   RW + drift (+ other possible stationary terms); 
##    Model: AR(1) + Trend (+ other possible stationary terms))
adf.1 <- ur.df(y = ylog, type = "trend", lags = 20, selectlags = "AIC")
cat("\n-----\nTest1: ADF with trend\n")
print( summary(adf.1) )
#### Comment: Accept for tau3, Accept for Phi3 -> look at Phi2.
##   Accept for Phi2. According to the procedure, we have now to assume
##   (DGP:   RW; 
##    Model: AR(1) + constant (+ other possible stationary terms))

#### (DGP:   RW; 
##    Model: AR(1) + constant (+ other possible stationary terms))
adf.2 <- ur.df(y = ylog, type = "drift", lags = 20, selectlags = "AIC")
cat("\n-----\nTest1: ADF with drift\n")
print( summary(adf.2) )
#### Comment: Accept for tau2, Accept for Phi1 -> Unit root.
#### IMPORTANT: This conclusion is typical in time series of daily prices of 
##   financial assets. 

################################################################################
## Preliminary analyses of log-returns
################################################################################

#### Percentage log-returns
yret <- xts(x = 100 * data$cc.ret, order.by = time)

######## Preliminary analysis
cat("\n-----------------------------------------------------------------
  Preliminary analysis of log-returns\n")
#### Time series
par(mfrow = c(1,1))
plot(x = time, y = yret, main = "Returns", 
     xlab = "", ylab = "", type = "l")
####  Comments: 
##   1) Daily returns move around a mean close to zero similarly to a WN; 
##   2) There are periods with different variability around the mean (sometimes 
##      high, sometimes low) -> volatility clustering

#### Serial correlation
par(mfrow = c(2,1))
Acf(x = yret, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = yret, lag.max = 100, type = "partial", main = "Returns")
cat("\nLjung-Box statistics on log-returns\n")
npar <- 0
lag <- c(2, 5, 10, 15, 20, 30, 50) + npar
lb <- mapply(FUN = Box.test, lag = lag, 
             MoreArgs = list(x = yret, type = "Ljung-Box", fitdf = npar))[1:3,]
print(rbind(lag = lag, lb))
#### Comment: All significant but it is quite uncommon. 

#### A further check should be "do an ADF test on returns to check whether they 
##   have additional UR's". 
##   I leave this to students, but the result is easily predictable.

#### Independence test on returns
#x1 <- log(abs(yret))
# x1 <- x1[is.finite(x1)]
# bds <- bds.test(x = x1, m = 5, 
#   eps = seq(from = 0.5 * sd(x1), to = 2 * sd(x1), length = 4),
#   trace = FALSE)
# cat("BDS test on returns\n")
# print(bds)
#### Comment: returns are not i.i.d.

#### ACF of returns, abs returns and squared returns
par(mfrow = c(3,1))
Acf(x = yret, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = abs(yret), lag.max = 100, type = "correlation", main = "|Returns|")
Acf(x = yret^2, lag.max = 100, type = "correlation", main = expression(Returns^2))

#### Another diagnostic: the ARCH test
cat("\n-----------------------------------------------------------------
  ARCH based preliminary analyses\n")
cat("ARCH test on demeaned log-returns\n")
lag <- c(4, 8, 12, 16)
at <- mapply(FUN = ArchTest, lags = lag, 
             MoreArgs = list(x = yret, demean = TRUE))
print(rbind(lag = lag, at[1:3,]))
#### Comment: largely significant at all lags

#### Unconditional distribution
par(mfrow = c(1,2))
.hist(x = yret, xlim = c(-10, 10), n = 200, breaks = 200, main = "Returns")
qqnorm(y = scale(yret))
abline(a = 0, b = 1, col = "red")
cat("\nShapiro-Wilk statistics on log-returns")
print( shapiro.test(x = as.numeric(yret)) )
#### Comment: Normality is highly rejected; the distribution is leptokurtic.

################################################################################
## ARMA modeling
################################################################################

####
cat("\n-----------------------------------------------------------------
  ARMA on log-returns\n")
#### Now we use package rugarch
##   Try an ARMA and look at the results; try to change distribution
## ARMA(1,0)-norm Akaike 3.758410  Bayes 3.765281
## ARMA(1,0)-std  Akaike 3.539311  Bayes 3.548472
spec0 <- arfimaspec(
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE, 
                    external.regressors = NULL), 
  distribution.model = "std") 
fit0 <- arfimafit(spec = spec0, data = yret, 
                  solver = "solnp")
## Store the number of parameters
np0 <- NROW(fit0@fit$coef)
## Some statistics
cat( "\nInformation Criteria" )
print( infocriteria(fit0) )
cat("\nMatcoef\n")
print( fit0@fit$matcoef )
cat("\nRobust matcoef\n")
print( fit0@fit$robust.matcoef )
#### Result: yret looks like a WN, implying that y is a RW. Really?

#### ACF of residuals, abs residuals and squared residuals
res <- as.numeric( residuals(fit0) )
par(mfrow = c(3,1))
Acf(x = res, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = abs(res), lag.max = 100, type = "correlation", main = "|res|")
Acf(x = res^2, lag.max = 100, type = "correlation", main = expression(res^2))

#### Distribution of residuals
par(mfrow = c(1,2))
xlim <- c(-5, 5)
.hist.fit(fit = fit0, xlim = xlim, ylim = c(0,0.75), n = 200, breaks = 100, 
          plot.norm = TRUE, main = "")
.qqplot.fit(fit = fit0)

#### Comments: 
##   1) Large serial correlation of absolute and squared residuals; 
##   2) Absolute values are more correlated than squares.
##   3) Nice fit of "std"
##   Conclusion: residuals are not WN. Since residuals are similar to the 
##   original time series, the original too is not a WN.

################################################################################
## ARCH/GARCH modeling
################################################################################

####
cat("\n-----------------------------------------------------------------
  GARCH on log-returns\n")

#### Simple GARCH
spec1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,  
                    external.regressors = NULL), 
  distribution.model = "std")
fit1 <- ugarchfit(spec = spec1, data = yret, solver = "solnp")
## Store the number of parameters
np1 <- NROW(fit1@fit$coef)
## Some statistics
cat( "\nInformation Criteria" )
print( infocriteria(fit1) )
cat("\nMatcoef\n")
print( fit1@fit$matcoef )
cat("\nRobust matcoef\n")
print( fit1@fit$robust.matcoef )

#### Diagnostics: Use standardized residuals!
fit <- fit1
par(mfrow = c(3,1))
Acf(x = fit@fit$z,      lag.max = 100, type = "correlation", main = "z")
Acf(x = abs(fit@fit$z), lag.max = 100, type = "correlation", main = "|z|")
Acf(x = fit@fit$z^2,    lag.max = 100, type = "correlation", main = expression(z^2))
lag1 <- np1 + c(1, 2, 5, 10, 15, 20)
cat("\nLjung-Box on standardized residuals:\n")
lb1 <- mapply(FUN = Box.test, lag = lag1, 
              MoreArgs = list(x = fit@fit$z, type = "Ljung-Box", fitdf = np1) )
print(rbind(lag = lag1, lb1[1:3,]))
cat("\nLjung-Box on |standardized residuals|\n")
lb1 <- mapply(FUN = Box.test, lag = lag1, 
              MoreArgs = list(x = abs(fit@fit$z), type = "Ljung-Box", fitdf = np1) )
print(rbind(lag = lag1, lb1[1:3,]))
cat("\nLjung-Box on standardized residuals^2:\n")
lb1 <- mapply(FUN = Box.test, lag = lag1, 
              MoreArgs = list(x = fit@fit$z^2, type = "Ljung-Box", fitdf = np1) )
print(rbind(lag = lag1, lb1[1:3,]))

#### ARCH test
cat("\nARCH test on standardized residuals\n")
lag <- c(4, 8, 12, 16)
at <- mapply(FUN = ArchTest, lags = lag, 
             MoreArgs = list(x = fit1@fit$z, demean = TRUE))
print(at[1:3,])
#### Comment: 

#### Unconditional distribution of residuals
par(mfrow = c(1,2))
xlim <- c(-5, 5)
.hist.fit(fit = fit1, xlim = xlim, ylim = c(0,0.55), n = 200, breaks = 100, 
          plot.norm = TRUE, main = "")
.qqplot.fit(fit = fit1)

#### Leverage check
cat("\nSign bias test\n")
print( signbias(fit1) )
#### Comment:

#### This can be verified in an explicit modeling -> gjrGARCH in place of sGARCH
spec2 <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
                        submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, 
                    external.regressors = NULL), 
  distribution.model = "std")
fit2 <- ugarchfit(spec = spec2, data = yret, solver = "solnp")
## Store the number of parameters
np2 <- NROW(fit2@fit$coef)
## Some statistics
cat( "\nInformation Criteria" )
print( infocriteria(fit2) )
cat("\nMatcoef\n")
print( fit2@fit$matcoef )
cat("\nRobust matcoef\n")
print( fit2@fit$robust.matcoef )

#### Compare the News Impact Curves (sGARCH vs gjrGARCH) 
ni1 <- newsimpact(z = NULL, fit1)
ni2 <- newsimpact(z = NULL, fit2)
legend <- c("Simple-GARCH", "GJR-GARCH")
col  <- c("black", "red")
ylim <- range( ni1$zy, ni2$zy )
par(mfrow = c(1,1), mar = c(4, 4.5, 3, 1) + 0.1, lwd = 2)
plot(x = ni1$zx, y = ni1$zy, ylab = ni1$yexpr, xlab = ni1$xexpr, type = "l", 
     ylim = ylim, main = "News Impact Curve", col = col[1])
lines(x = ni2$zx, y = ni2$zy, col = col[2], lwd = 2)
legend(x = "topright", y = NULL, legend = legend, border = FALSE, col = col, 
       lty = 1, text.col = col)

#### Stability check
cat("\nStability check (Nyblom test)\n")
print( nyblom(fit2) )
## Comment: stability is strongly rejected -> reduce the length of the series 





