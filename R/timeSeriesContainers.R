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
  
  # compute offset to account for data type and maybe timezone, etc.
  offset = 0
  # data type - inst vals = 0; period vals += interval
  if(offsetForType){
    offset = int(metdata[["interval"]])
  }

  
  ## TODO: fix tz="UTC" to use local if not specified in tsc's timezone parameter
  out = xts(tsc$values, order.by=as.POSIXct(tsc$times*60+offset, origin="1899-12-31 00:00", tz="UTC")) #, dssMetadata= as.data.frame(metadata))
  colnames(out) = metadata[[colnamesSource]]
  
  return(out)
}

#' tsc.to.dt Converts Java TimeSeriesContainer objects to data.table objects.
#' 
#' convert time series container to data.table
#' 
#' Long Description
#' 
#' @return data.table object from times and values in TSC.
#' @note NOTE
#' @author Cameron Bracken
#' @export 
tsc.to.dt <- function(tsc, ...){
  
  require(data.table)
  
  times = as.POSIXct(tsc$times*60, origin="1899-12-31 00:00", tz="UTC")
  values = tsc$values
  units = tsc$units
  if(length(values)==0)units = character(0)
  
  out = data.table(datetime=times,value=values,units=units)
  setkey(out, "datetime")
  
  attr(out,'dssMetadata') = as.data.frame(getMetadata(tsc))
  
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

#' getDT Get a TSC from a file and pathname as a data.table
#' 
#' short desc
#' 
#' Long Description
#' 
#' @return stuff
#' @note NOTE
#' @author Cameron Bracken
#' @export 
getDT <- function(file, path){
  return(tsc.to.dt(file$get(path)))
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
getLooseTSC <- function(file, paths, ...){
  tscList = list()
  for(p in paths){
    tscList[[p]] = getTSC(file, p, ...)
  }
  xtsOut = do.call(rbind.xts, tscList)
  xtsAttributes(xtsOut) = list(dssMetadata=do.call(rbind, lapply(tscList, function(x) attr(x, "dssMetadata"))))
  return(xtsOut)
}

#' getFullDT Get a full TSC as data.table, ignoring date parameters.
#' 
#' Gets paths, converts to data.table, and merges to one time series.
#' 
#' Long Description
#' 
#' @return merged data.tame of all times and values in the time series matching paths.
#' @note NOTE
#' @author Cameron Bracken
#' @export 
getFullDT <- function(file, paths, discard_empty = TRUE){
  require(data.table)
  
  dtList = list()
  for(p in paths){
    dt = getDT(file, p)
    if(nrow(dt) == 0 & isTRUE(discard_empty)) next else dtList[[p]] = dt
  }
  dtOut = do.call(rbind, dtList)
  attr(dtOut,'dssMetadata') = do.call(rbind, lapply(dtList, function(x) attr(x, "dssMetadata")))
  
  return(dtOut)
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
