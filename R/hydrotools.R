## HydroTools - R functions for hydrology
require(ggplot2)
require(plyr)
require(scales)


## "Constants"
###################

#' @title hydrologic constants
#' @description used for converting between flows in (k)cfs and volumes in (K)AF
#' @note an exact conversion as cubic feet per acre-foot is 43560, seconds in a day is 86400, simplifing the conversion gets 24/12.1
#' @export
AF_PER_CFS_DAY = 24/12.1  ## This is an exact conversion.  == (24hr/day * 60min/hr * 60s/min)/(43560 ft^2/acre * 1 ft)

## Water specific date functions
#################################

#' @name wateryear 
#' @description Gets water year (Oct to Sept) from POSIXt object.
#' @title 'lubridate' type functions for getting numeric values from `Date` and `POSIXt` objects.
#' @param t timestamp object (e.g. Date, POSIXt)
#' @return integer of water year
#' Works similar to 'year' function on POSIXt classes
#' @export
#' @aliases wymonth, wyday, wymonth.abb
wateryear <-  function(t) year(t) + ifelse(month(t) >= 10, 1, 0)

#' @name wymonth 
#' @description Get's the month in the water year (Oct=1, Sept=12)
#' @title Month of water year
#' @param t timestamp object (e.g. Date, POSIXt)
#' @return integer of month in water year (OCT = 1, SEP = 12)
#' Works similar to 'year' function on POSIXt classes
#' @export
#' @fai
wymonth = function(t) (month(t) + 2) %% 12 + 1

#' @name wyday 
#' @description Get the day of water year (01Oct=1, 30Sep=365+leapyear(y))
#' @title Day of water year
#' @param t timestamp object (e.g. Date, POSIXt)
#' @return integer of day in water year (01Oct=1, 30Sep=365+leapyear(y))
#' @export
wyday <- function(t) as.integer(as.POSIXct(t) - as.POSIXct(paste0(wateryear(t)-1, "-10-01")))
                           
#' @name wymonth.abb 
#' @description month.abb reordered for Oct (1) to Sep (12)
#' @title Water year month
#' @export
wymonth.abb = month.abb[c(10:12,1:9)]




## Error Functions
###################

## For comparisons with data from Excel - because people are comfortable with this.

#' Model error measuring functions
#' @name model_error_measurement
#' @aliases rmse, nash.sutcliffe, excelR2
#' @title Model error measurement functions
#' @param x.obs - observed values
#' @param x.model - modeled values
#' @param x.alt - alternate model for comparison (Nash Sutcliffe only, defaults to mean of observed)
#' @return score - see details.
#' @family model_error_functions
#' @details
#' `excelR2` returns the R^2 as reported by Excel's curve fits.
#' `rmse` returns the root mean square error.
#' `nash.sutcliffe` returns the Nash-Sutcliffe model coefficent, greater than 0 if `x.model` is a better fit than x.alt, 1 if a perfect fit, and between 0 and -\Inf if a worse fit than x.alt.

#' @export
excelR2 = function(x.obs, x.model) cor(x.obs, x.model, method="pearson")**2

#' RMSE function, checked
#' one parameter assumes x1 is residuals,
#' two parameters assumes x1 and x2 are modeled versus fitted
#' @name rmse
#' @export
rmse = function(x.obs, x.model=NULL){
  if(!is.null(x.obs)){
    x.obs = x.obs - x.model
  }
  return(sqrt(mean((x.obs)**2)))
}

#' Nash-Sutcliffe model coefficient
#' checks for better performance than assuming climatology or against another model.
#' @name nash.sutcliffe
#' @export
nash.sutcliffe = function(x.obs, x.model, x.alt=mean(x.obs)){
  return(1 - sum((x.obs - x.model)**2) / sum((x.obs - x.alt)**2))
}


## Frequency Curve For ggplot2
###############################
#' @name flowBreaks
#' @description
#' Generates a list of breaks for log10 axes that includes multiples of each magnitude, with names from 'labels'.
#' @export
#' @param Q flows, used to determine range
#' @param labels which breaks should be labeled, using seq(1,9) would be crowded, default is usually acceptable.
#' @return list of breaks with names to use as labels.
flowBreaks <- function(Q, labels=c(1,2,3,5,7)){
  logRange = log10(range(Q))
  lower = 10^floor(logRange[1])
  upper = 10^ceiling(logRange[2])
  cap = lower
  ybreaks = NULL
  ynames = NULL
  while(cap < upper){
    ybreaks = c(ybreaks, seq(cap, cap*9, by=cap))
    ynames = c(ynames, ifelse(seq(1,9) %in% labels, 
                              as.character(seq(cap, cap*9, by=cap)), ""))
    cap = cap*10
  }
  names(ybreaks) = ynames
  return(ybreaks)
}

#' @name wiebullProbs
#' @description
#' generates weibull plotting positions for given vector.  Often used in hydrologic 'flow frequency curves' 
#' @param Qs list of points for which weibull plotting positions are needed
#' @param exceedance boolean that determines if plotting positions should be exceedance probabilities or non-exceedance probabilities.
#' @param as.points boolean if ties should get independent points or not.
#' @return probabilities in range of (0,1) corresponding to each point in input vector.
#' @export
weibullProbs <- function(Qs, exceedance=FALSE, as.points=FALSE){
  ## Ties.method as 'min' if diplsaying points, "first" if displaying as a line.  Series of points with same value having different probabilities doesn't make sense.
  return(abs(exceedance - rank(Qs, ties.method=ifelse(as.points, "min", "first"), na.last=FALSE) / (length(Qs)+1)))
}

probBreaks <- function(maxLevel=3, lines=c(1,2,5), labels=c(1,2,5), invert=TRUE, as.percent=TRUE, byPeriod=FALSE, periodSuffix=" yr"){
  probBreaks = NULL
  probLabels = NULL
  level = -1
  
  # TODO - assign maximum level from max(log10(weibullProbs(Q)))
  while(level >= -maxLevel){
    p = 10^level*lines
    p = c(p, 1-p)
    if(as.percent){
      labs = ifelse(c(lines, lines) %in% labels, paste0(as.character(100*p), "%"), "")
      labs = ifelse(p==0.5, "50%", labs) 
    } else {
      labs = ifelse(c(lines, lines) %in% labels, as.character(p), "")
    }
    if(byPeriod){
      warning("Generating breaks with reoccurance periods shown - You shouldn't be using '-year' events!")
      period = 1 / p
      period = paste0(period, periodSuffix)
      labs = ifelse(p <= 0.5 & labs != "", paste(labs, period, sep="\n"), labs)
    }
    probBreaks = c(probBreaks, p)
    probLabels = c(probLabels, labs)
    level = level - 1
  }
  if(invert){
    probBreaks = 1-probBreaks
  }
  names(probBreaks) = probLabels
  return(probBreaks)
}

## Transformations for ggplot / scales package
## with inspiration from probability_trans and log_trans in the scales package.
##
## TODO:  reimplement probBreaks and flowBreaks into proper _breaks functions.
## TODO:  implement nice formatters that behave similarly.
##
## such that 
## ggplot(data) + geom_line(aes(x=flow, y=probs)) + 
##    scale_x_continuous(trans=hydro_flow_trans()) + 
##    scale_y_continuous(trans=hydro_probs_trans())
## produces a decent flow frequency plot by default
##
## TODO: implement ways to pass arguments to flowBreaks and probBreaks through these.
##
## these are the probability_trans and log10_trans, but with special break and
## format functions.
##
## Example that works:
## ggplot(peaks, aes(y=PEAK_DAILY_FLOW, x=PROB)) + geom_point() + 
##   theme_bw(base_size=11) + theme(legend.position = "bottom", panel.grid.minor=element_blank()) +
##   scale_y_continuous(trans=hydro_flow_trans()) + 
##   scale_x_continuous(trans=hydro_prob_trans(lines=c(1,2,5), labels=c(1,2,5), byPeriod=TRUE)) + 
##   stat_smooth(method="glm", family=gaussian(link="log"))

#' @name hydrologic transforms for ggplot2
#' @description 
#' Axis transforms for hydrologic plots in ggplot2.
#' @details
#' hydro_prob_trans for probability axes on frequency plots
#' hydro_flow_trans for log axes with additional breaks
#' @export
#' @aliases hydro_flow_trans, hydro_prob_breaks, hydro_flow_breaks
hydro_prob_trans <- function(distribution="norm", distArgs=list(), ...){
  qfun <- match.fun(str_c("q", distribution))
  pfun <- match.fun(str_c("p", distribution))
  
  return(trans_new(
    name=str_c("hydro_probs_", distribution),
    transform=function(x) { qDistArgs = distArgs; qDistArgs$p = x; do.call(qfun, qDistArgs)},
    inverse=function(x) { pDistArgs = distArgs; pDistArgs$q = x; do.call(pfun, pDistArgs)},
    breaks=hydro_prob_breaks(...),
    format=format_format(),
    domain=c(1e-9, 1-1e-9)))
}

#' @name hydro_flow_trans
#' @export
hydro_flow_trans <- function(...){
  return(trans_new(
    name="hydro_flow",
    transform=log10_trans()$transform, #function(x) log(x, base=10),
    inverse=log10_trans()$inverse, #function(x) x^10,
    breaks=hydro_flow_breaks(...),
    format=format_format(),
    domain=c(1e-100,Inf)))
}

#' @name hydro_prob_breaks
#' @export
hydro_prob_breaks <- function(...){
  return(function(x){
    magnitude = ceiling(abs(log10(min(x))))
    return(probBreaks(maxLevel=magnitude, ...))
  })
}

#' @name hydro_flow_breaks
#' @export
hydro_flow_breaks <- function(){
  return(function(x){
    magnitudes = diff(log10(range(x)))
    #if(magnitudes < 1){
    #  return(flowBreaks(x, labels=seq(1,9)))
    #} else if(magnitudes > 4){
    #  return(flowBreaks(x, labels=c(1,5)))
    #} else {
      return(flowBreaks(x))
    #}
  })
}