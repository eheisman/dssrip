## HydroTools - R functions for hydrology
require(ggplot2)
require(plyr)
require(scales)


## "Constants"
###################

AF_PER_CFS_DAY = 1.9835

## Water specific date functions
#################################

#' @name wy Gets water year (Oct to Sept) from POSIXt object.
#' @aliases wateryear
#' @title Index by water year.
#' @param t
#' @return integer of water year
#' @note aliased to 'wateryear'
#' Works similar to 'year' function on POSIXt classes
#' @rdname wy
#' @export
wy <-  function(t) year(t) + ifelse(month(t) >= 10, 1, 0)
#' @rdname wy
#' @export
wateryear = wy

#' @name wymonth Get's the month in the water year (Oct=1, Sept=12)
#' @title Month of water year
#' @param t
#' @return integer of month in water year (OCT = 1, SEP = 12)
#' @note aliased to 'wateryear'
#' Works similar to 'year' function on POSIXt classes
#' @export
wymonth = function(t) (month(t) + 2) %% 12 + 1

#' @export
wymonth.abb = month.abb[c(10:12,1:9)]




## Error Functions
###################

## For comparisons with data from Excel
excelR2 = function(x.obs, x.model) cor(x.obs, x.model, method="pearson")**2

## RMSE function, checked
# one parameter assumes x1 is residuals,
# two parameters assumes x1 and x2 are modeled versus fitted
rmse = function(x.obs, x.model=NULL){
  if(!is.null(x.obs)){
    x.obs = x.obs - x.model
  }
  return(sqrt(mean((x.obs)**2)))
}

## Nash-Sutcliffe measure
# checks for better performance than assuming climatology or against another model.
nash.sutcliffe = function(x.obs, x.model, x.alt=NULL){
  if(is.null(x.alt)){
    x.alt = mean(x.obs)
  }
  return(1 - sum((x.obs - x.model)**2) / sum((x.obs - x.alt)**2))
}


## Frequency Curve For ggplot2
###############################

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

weibullProbs <- function(Qs, exceedance=FALSE, as.points=FALSE){
  ## Ties.method as 'min' if diplsaying points, "first" if displaying as a line.  Series of points with same value having different probabilities doesn't make sense.
  return(abs(exceedance - rank(Qs, ties.method=ifelse(as.points, "min", "first"), na.last=FALSE) / (length(Qs)+1)))
}

probBreaks <- function(maxLevel=3, lines=c(1,2,5), labels=c(1), invert=TRUE, as.percent=TRUE, byPeriod=FALSE, periodSuffix=" yr"){
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