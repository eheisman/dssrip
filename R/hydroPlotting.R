## Frequency Curve For ggplot2
###############################

#' @export
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

#' @export
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

#' @name hydro_axis_trans
#' @title Hydrologic axis transforms for ggplot2
#' @author Evan Heisman
#' @description 
#' Axis transforms for hydrologic plots in ggplot2.
#' @details
#' 
#' \code{hydro_prob_trans} and \code{hydro_prob_breaks} for probability axes on frequency plots
#'
#' \code{hydro_flow_trans} and \code{hydro_flow_breaks} for log axes with additional breaks
#'
#' @aliases hydro_flow_trans hydro_prob_trans hydro_prob_breaks hydro_flow_breaks flowBreaks probBreaks
#' @examples
#' ggplot(peaks, aes(y=PEAK_DAILY_FLOW, x=weibullProbs(PEAK_DAILY_FLOW))) + geom_point() + 
#'   theme_bw(base_size=11) + theme(legend.position = "bottom", panel.grid.minor=element_blank()) +
#'   scale_y_continuous(trans=hydro_flow_trans()) + 
#'   scale_x_continuous(trans=hydro_prob_trans(lines=c(1,2,5), labels=c(1,2,5), byPeriod=TRUE)) + 
#'   stat_smooth(method="glm", family=gaussian(link="log"))

#' @export
hydro_prob_trans <- function(distribution="norm", distArgs=list(), ...){
  require(scales)
  
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

#' @export
hydro_flow_trans <- function(...){
  require(scales)
  return(trans_new(
    name="hydro_flow",
    transform=log10_trans()$transform, #function(x) log(x, base=10),
    inverse=log10_trans()$inverse, #function(x) x^10,
    breaks=hydro_flow_breaks(...),
    format=format_format(),
    domain=c(1e-100,Inf)))
}

#' @export
hydro_prob_breaks <- function(...){
  return(function(x){
    magnitude = ceiling(abs(log10(min(min(x),1-max(x)))))
    return(probBreaks(maxLevel=magnitude, ...))
  })
}

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
