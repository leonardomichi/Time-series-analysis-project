################################################################################
##
## File:    CalendarEffects-Functions.R
## 
## Purpose: Functions to manage calendar effects.
##
## Created: 2018.03.03
##
## Version: 2022.07.14
## 
################################################################################

.calendar <- 
function(time)
{
  #### Settings
  ## nobs
  nobs <- NROW(time)
  if (nobs < 2)
  {
    stop("Argument 'time' must have at least two elements.")
  }
  nobs1 <- nobs + 1
  ## by
  x1 <- time[2] - time[1] 
  by <- if ( round(x1) == 1 ) { "day" }
    else if ( round(x1 / 7) == 1 ) { "week" }
    else if ( round(x1 / 30.4375) == 1 ) { "month" }
    else if ( round(x1 / 91.3125) == 1 ) { "quarter" }
    else { stop("By must be 'day', 'week', 'month' or 'quarter'.") }

  #### Days
  x1 <- seq(from = time[1], by = by, length.out = nobs1)

  #### Answer
  data.frame(time = time, from = x1[-nobs1], to = x1[-1] - 1)
}
# ------------------------------------------------------------------------------


.singleHolidays <- 
function(year, country = "it")
{
  ## FUNCTION:
  
  #### Italy
  if (country == "it")
  {
    x1 <- c("01-01", "01-06", "04-25", "05-01", "06-02", "08-15", "11-01", 
      "12-08", "12-25", "12-26" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x1 <- c( x1, as.character(EasterMonday(year)) )
  }
  #### US
  else if (country == "us")
  {
    x1 <- c("01-01", "02-14", "03-17", "07-04", "10-31", "12-24", "12-25", "12-31" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- USThanksgivingDay(year)
    x3 <- as.timeDate(as.Date(x2) + 1)
    x1 <- c( x1,  
      as.character( c( USMemorialDay(year), x2, x3, USLaborDay(year) ) ) )
  }
  #### Germany
  else if (country == "de")
  {
    x1 <- c("01-01", "05-01", "10-03", "12-25", "12-26" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- c( GoodFriday(year), EasterMonday(year), PentecostMonday(year) )
    x1 <- c( x1, as.character( x2 ) )
  }
  #### Spain
  else if (country == "es")
  {
    x1 <- c("01-01", "01-06", "05-01", "08-15", "10-12", "11-01", "12-06", "12-08", 
      "12-25" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x1 <- c( x1, as.character(GoodFriday(year)) )
  }
  #### Belgium
  else if (country == "be")
  {
    x1 <- c("01-01", "05-01", "07-21", "08-15", "11-01", "11-11", "12-25" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- .AscensionDay(year)
    x3 <- .PentecostMonday(year)
    x1 <- c( x1, as.character(EasterMonday(year)), as.character( c(x2, x3) ) )
  }
  #### Netherlands
  else if (country == "nl")
  {
    x1 <- c("01-01", "05-05", "12-25", "12-26" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- .NLKingsBirthday(year)
    x3 <- .AscensionDay(year)
    x4 <- .PentecostMonday(year)
    x1 <- c( x1, as.character( c(GoodFriday(year), EasterMonday(year)) ), 
      as.character( c(x2, x3, x4) ) )
  }
  else
  {
    stop("Country ", country, " not yet implemented")
  }
  
  #### Answer
  as.Date( x1 )
}
# ------------------------------------------------------------------------------


.AscensionDay <- function(year)
{
  as.Date(EasterSunday(year)) + 39
}
# ------------------------------------------------------------------------------

.PentecostMonday <- function(year)
{
  as.Date(EasterSunday(year)) + 50
}
# ------------------------------------------------------------------------------

.NLKingsBirthday <- function(year)
{
  x1 <- as.Date( paste0(year, "-04-27") )
  wd <- weekdays(x = x1, abbreviate = TRUE)
  ind <- wd %in% c("Sun",'dom','Dom')
  x1[ind] <- x1[ind] - 1
  x1
}
# ------------------------------------------------------------------------------


.easterHolidays <- 
function(year, len = 3)
{
  ## FUNCTION:
  
  #### Easter dates
  easter <- as.Date( EasterSunday( year ) )
  #### Answer
  fun <- function(easter, len)
  {
    as.Date( seq(from = easter - 1, length.out = len, by = -1) )
  }
  unlist( mapply( FUN = fun, easter = easter, MoreArgs = list(len = len), 
    SIMPLIFY = FALSE ) )
}
# ------------------------------------------------------------------------------


.calendarEffects.2 <- 
function(from, to, 
  easter.len = 3, country = "it")
{
  #### Library
  require(timeDate)
  require(car)
  
  #### Year (used below)
  x1 <- as.numeric( format(x = c(from, to), format = "%Y") )
  year <- unique( x1[1] : x1[2] )

  #### Weekdays
  ## Start from from[1] - 1 and ends at to[1] + 1 to manage long weekend 
  ## holidays
  xx  <- seq(from = from[1] - 1, to = to[1] + 1, by = "day")
  wdx <- weekdays(x = xx, abbreviate = TRUE)
  #### Recode wd expressed in Italian to US
  x1 <- 
   "c('lun','Lun')='Mon';c('mar','Mar')='Tue';c('mer','Mer') ='Wed';
    c('gio','Gio')='Thu';c('ven','Ven')='Fri';c('sab','Sab')='Sat';
    c('dom','Dom')='Sun'" 
  wdx <- car::recode(var = wdx, recodes = x1)

  #### Easter
  eh <- .easterHolidays(year = year, len = easter.len[1])
  ind <- xx %in% eh
  wdx[ind] <- "eh"
  
  #### Single holidays
  single <- .singleHolidays(year = year, country = country[1])
  ind.single <- xx %in% single  

  #### Improve workdays: single holidays happening in weekdays different from Sun 
  ##   are isolated
  ind <- wdx != "Sun" & ind.single
  wdx[ind] <- "sh"

  #### Long weekend
  ind <- wdx %in% c("Sun", "sh")
  n1 <- NROW(ind)
  ind <- c(FALSE, ind[ 3 : n1 ] & !ind[2 : (n1 - 1)] & ind[ 1 : (n1 - 2) ], FALSE )
  wdx[ind] <- "lh"
  
  #### Cut the extra added days
  wdx[ -c(1, NROW(wdx)) ]
}
# ------------------------------------------------------------------------------


.calendarEffects.1 <- 
function(from, to, 
  easter.len = 3, country = "it")
{  
  #### Workdays 
  wd <- .calendarEffects.2(from = from, to = to, easter.len = easter.len, 
    country = country)
  
  #### Table
  weekd  <- table(wd, dnn = NULL)

  #### Append single, long weekend and Easter holidays if not included
  if ( !any( names(weekd) == "sh" ) )
  {
    weekd <- c(weekd, sh = 0)
  } 
  if ( !any( names(weekd) == "lh" ) )
  {
    weekd <- c(weekd, lh = 0)
  } 
  if ( !any( names(weekd) == "eh" ) )
  {
    weekd <- c(weekd, eh = 0)
  } 
  
  #### Workdays imbalance variable
  ind <- names(weekd) %in% c("Mon", "Tue", "Wed", "Thu", "Fri")
  workd  <- sum(weekd[ind]) - 2.5 * sum(weekd[!ind]) 
  
  #### Answer
  ## To sort
  ind <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh") 
  c(weekd[ind], wd = workd, nd = sum(weekd))
}
# ------------------------------------------------------------------------------


.calendarEffects <- 
function(time, country = "it")
{
  ##############################################################################
  ## Arguments
  ##  time:    (Date[n]) vector of dates.
  ##  country: (character[1]) abbreviated (two letter) country.
  ## Value
  ##  A matrix reporting, in each row (corresponding to a week, a month or a 
  ##   quarter), the number of days corresponding to the following types of days: 
  ##   "Mon": number of Mondays which are not sh, lh, or eh (see below).
  ##   "Tue": number of Tuesdays which are not sh, lh, or eh (see below).
  ##   ...
  ##   "Sat": number of Saturdays which are not sh, lh, or eh (see below).
  ##   "Sun": number of Sundays.
  ##   "sh":  number of single holidays (holidays not falling in Sun).
  ##   "lh":  number of long weekends (giorni di ponte) excluding Sun and 
  ##     sh.
  ##   "eh":  number of Easter holidays (different from Easter, included in 
  ##     Sun, and Easter Monday included in sh). By default, each year, 3 days 
  ##     are considered as eh.
  ##   "wd":  workdays imbalance. It is computed as 
  ##     wd = (Mon + Tue + ... + Fri) - 2.5 * (Sat + Sun).
  ##   "nd":  number of days. It is equal to (Mon + ... + Sun + sh + lh + eh)  
  ##############################################################################

  ## FUNCTION:
  
  #### Calendar
  time <- as.Date( paste0(format(time, "%Y-%m-"), "01") ) 
  x1 <- .calendar(time = time)
  #### Calendar effects
  x1 <- do.call(what = rbind, args = mapply(FUN = .calendarEffects.1, 
    from = x1$from, to = x1$to, country = country, SIMPLIFY = FALSE) )
  #### Answer
  data.frame(time = time, x1)
}
# ------------------------------------------------------------------------------


.extend.time <- 
function(x, n.ahead, by = "month")
{
  ##############################################################################
  ## Arguments
  ##  x:       (Date[n]) vector of dates.
  ##  n.ahead: (numeric[1]) number of values after the last date in x.
  ##  by:      (character[1]) on among "day", "week", "month", "quarter", 
  ##           "year".
  ## Value
  ##  The argument x extended some steps ahead.
  ##############################################################################

  #### Answer
  seq(from = x[NROW(x)], by = by, length.out = n.ahead + 1)[-1]
}
# ------------------------------------------------------------------------------
