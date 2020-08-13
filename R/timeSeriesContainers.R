#########
# timeSeriesContainers.R
# Functions to parse timeseries containers into objects that R recognizes
#########

#' tsc.to.xts Converts Java TimeSeriesContainer objects into XTS time series objects.
#' 
#' convert time series container to XTS
#' 
#' Long Description
#' 
#' @param colnamesSource which TimeSeriesContainer field should be used for column names, typically `parameter`
#' @param offsetForType defaults to `FALSE` for backwards compatibility, offsets data if period average
#' 
#' @return xts object from times and values in TSC.
#' @note NOTE
#' @author Evan Heisman
#' @export 
tsc.to.xts <- function(tsc, colnamesSource="parameter", offsetForType=FALSE){
  
  metadata = getMetadata(tsc, colnamesSource=colnamesSource)
  print(metadata)
  # compute offset to account for data type and maybe timezone, etc.
  offset = 0
  # data type - inst vals = 0; period vals += interval
  if(offsetForType){
    if(str_starts(metadata[["type"]], fixed("PER-", ignore_case=TRUE))){
      # period average or period cumulative data, offset by interval
      offset = -as.numeric(metdata[["interval"]])
    }
  }
  granularity = as.numeric(metadata[["timeGranularitySeconds"]])
  if(is.na(granularity)){
    # minutes by default
    granularity = 60
  }
  
  ## TODO: fix tz="UTC" to use local if not specified in tsc's timezone parameter
  out = xts(tsc$values, order.by=as.POSIXct(tsc$times*granularity + offset, origin="1899-12-31 00:00")) #, dssMetadata= as.data.frame(metadata))
  colnames(out) = metadata[[colnamesSource]]
  
  attr(out, "tsc.type") = metadata[["type"]]
  attr(out, "tsc.units") = metadata[["units"]]
  
  return(out)
}

#' getTSC Get a TSC from a file and pathname as a XTS.
#' 
#' Skips intermediate step of getting TimeSeriesContainer object.
#' 
#' Long Description
#' 
#' @return xts from time series located at path in file.
#' @note NOTE
#' @author Evan Heisman
#' @export 
#' @family getTSC
getTSC <- function(file, path, fullTSC=FALSE, ...){
  return(tsc.to.xts(file$get(path, fullTSC), ...))
}

#' getFullTSC Get a full TSC, ignoring date parameters.
#' 
#' Gets paths, converts to XTS, and merges to one time series.
#' 
#' Warning - does not check that all paths are the same except for D part
#' 
#' @return merged xts of all times and values in time series matching paths.
#' @note NOTE
#' @author Evan Heisman
#' @export 
#' @family getTSC
getFullTSC <- function(file, paths, ...){
  ## Accepts sets of paths or like getFullTSC, or single path. 
  if(length(paths) > 0){
    ## Check if all paths are identical, sans D part.
    pathdf = separatePathParts(paths)
    identicalPaths = apply(pathdf, 2, function(col) all(unique(col)==col))
    if(!all(identicalPaths[c("A", "B", "C", "E", "F")])){
      stop("Cannot create condensed pathname to pull!")
    }
  }
  return(getTSC(file, paths[1], fullTSC=TRUE, ...))  
}

#' getTimeseriesAsDataFrame Get a TSC, ignoring date parameters.
#' 
#' Gets paths, converts to `data.frame`, er a `tibble` into a`base::data.frame`
#' 
#' @return data.frame of all times and values in time series matching paths.
#' @author Evan Heisman
#' @export 
#' @family getTSC
getTimeSeriesAsDataFrame <- function(file, path, leaveAsTibble=FALSE, getFullTSC=TRUE, ...){
  require(broom)
  tscTibble = tidy(getTSC(file, path, getFullTSC, ...))
  if(isFALSE(leaveAsTibble)){
    return(data.frame(tscTibble))
  } else {
    return(tscTibble)
  }
}

# Adjusts "24:00" timestamps back one day so indexing appears appropriately.
#' Adjusts timestamps back by one day.  
#' 
#' Used to convert daily data's 24:00 timestamps back to proper date.
#' 
#' This should be fixed by passing "offsetByInterval" to the `tsc.to.xts` function as that is more robust to timesteps other than daily.
#' 
#' @note - this should be automated in future version by checking if the TSC's type allows 24:00 timestamps
#' 
#' @param n seconds to adjust timestamp by, defaults to -24*60*60, e.g. 1 day.
#' @return xts with timestamps adjusted back by n
#' @export
fixTimestamps <- function(ts, n=-24*60*60){
  index(ts) = as.POSIXct(index(ts), origin="1970-01-01") + n
  return(ts)
}
