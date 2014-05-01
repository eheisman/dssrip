## HydroTools - R functions for hydrology

## Constants
#############

#' @title Hydrologic constants
#' @description used for converting between flows in (k)cfs and volumes in (K)AF
#' @author Evan Heisman
#' @note an exact conversion as cubic feet per acre-foot is 43560, seconds in a day is 86400, simplifing the conversion gets 24/12.1
#' @export
AF_PER_CFS_DAY = 24/12.1  ## This is an exact conversion.  == (24hr/day * 60min/hr * 60s/min)/(43560 ft^2/acre * 1 ft)





## Water specific date functions
#################################

#' @name hydrologic_date_functions
#' @title Hydrologic date helper functions 
#' @author Evan Heisman
#' @description Provides 'lubridate' type functions for getting numeric values from `Date` and `POSIXt` objects based on the water year.
#' @note \code{wy} is no longer provided due to conflicts with the common use as a variable name in loops
#' @param t timestamp object (e.g. \code{Date}, \code{POSIXt} classes)
#' @return
#' 
#' \code{wateryear} water year for a given timestamp (October of previous calendar year through September)
#' 
#' \code{wymonth} month of water year (Oct = 1, Sept = 12)
#' 
#' \code{wyday} day of water year (01Oct=1, 30Sep=365+leapyear(y))
#' 
#' @aliases wateryear wymonth wyday wymonth.abb

#' @export
wateryear <-  function(t) {
  require(lubridate)
  year(t) + ifelse(month(t) >= 10, 1, 0)
}

#' @export
wymonth = function(t){
  require(lubridate)
  (month(t) + 2) %% 12 + 1
}

#' @export
wyday <- function(t) {
  require(lubridate)
  as.integer(as.POSIXct(t) - as.POSIXct(paste0(wateryear(t)-1, "-10-01")))
}
                           
#' @export
wymonth.abb = month.abb[c(10:12,1:9)]




## Model Performance Functions
################################
#' @name model_error_measurement
#' @aliases rmse nash.sutcliffe excelR2
#' @title Model error measurement metrics
#' @author Evan Heisman
#' @description Common interface for metrics commonly for assessing quality of hydrologic models, statistical or otherwise.
#' @usage
#' \code{excelR2(x.obs, x.model)}
#' \code{rmse(x.obs, x.model)}
#' \code{rmse(residuals)}
#' \code{nash.sutcliffe(x.obs, x.model)}
#' \code{nash.sutcliffe(x.obs, x.model, x.alt)}
#' 
#' @param x.obs - observed values 
#' @param x.model - modeled values
#' @param x.alt - alternate model for comparison (Nash Sutcliffe only, defaults to mean of observed)
#' @details
#' 
#' \code{excelR2} returns the R^2 as reported by Excel's curve fits.  It is provided as people are comfortable with it, but is a terrible measure of model accuracy as it only provides correlation, not fit.
#'
#' \code{rmse} returns the root mean square error.  With one parameter assumes x.obs is residuals, with two, computes residuals between x.obs and x.model
#'
#' \code{nash.sutcliffe} returns the Nash-Sutcliffe model coefficent, greater than 0 if `x.model` is a better fit than x.alt, 1 if a perfect fit, and between 0 and -infinity if a worse fit than x.alt.
#' 
#' @references Nash, J. E. and J. V. Sutcliffe (1970), River flow forecasting through conceptual models part I - A discussion of principles, Journal of Hydrology, 10 (3), 282–290.
#' @export

#' @title Excel-like R^2 function
#' @name excelR2
excelR2 = function(x.obs, x.model) cor(x.obs, x.model, method="pearson")**2

#' @title RMSE function
#' @name rmse
#' @export
rmse = function(x.obs, x.model=NULL, residuals=x.obs){
  if(!is.null(x.model)){
    residuals = x.obs - x.model
  }
  return(sqrt(mean((residuals)**2)))
}

#' @title Nash-Sutcliffe model coefficient function
#' @name nash.sutcliffe
#' @export
nash.sutcliffe = function(x.obs, x.model, x.alt=mean(x.obs)){
  return(1 - sum((x.obs - x.model)**2) / sum((x.obs - x.alt)**2))
}



#' @name weibullProbs
#' @title Compute Weibull plotting positions for flow frequency curves
#' @author Evan Heisman
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