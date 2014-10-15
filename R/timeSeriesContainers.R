#' tsc.to.xts Converts Java TimeSeriesContainer objects into XTS time series objects.
#' 
#' convert time series container to XTS
#' 
#' Long Description
#' 
#' @return xts object from times and values in TSC.
#' @note NOTE
#' @author Evan Heisman
#' @export 
tsc.to.xts <- function(tsc, colnamesSource="parameter"){
  
  metadata = getMetadata(tsc, colnamesSource=colnamesSource)
  
  ## TODO: fix tz="UTC" to use local if not specified in tsc's timezone parameter
  out = xts(tsc$values, order.by=as.POSIXct(tsc$times*60, origin="1899-12-31 00:00", tz="UTC"), dssMetadata= as.data.frame(metadata))
  colnames(out) = metadata[[colnamesSource]]
  
  return(out)
}

#' tsc.to.dt Converts Java TimeSeriesContainer objects to data.table objects.
#' 
#' convert time series container to data.table
#' 
#' Long Description
#' 
#' @return data.table object from times andvalues in TSC.
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

#' xts.to.tsc Converts xts objects to Java TimeSeriesContainer objects.
#' 
#' Converts xts objects to TimeSeriesContainers for writing to DSS files.
#' 
#' Long Description
#' 
#' @return java TimeSeriesContainer.
#' @note NOTE
#' @author Evan Heisman
#' @export 
xts.to.tsc <- function(tsObject, ..., ePart=NULL, interval=NULL, fillData=FALSE, protoTSC=NULL){
  ## Fill empty time slots in tsObject
  times = as.POSIXct(index(tsObject))
  deltas =  diff(as.integer(times)/60)
  if(!is.null(ePart)){
    interval = TSC_INTERVALS[ePart]
  } else {
    ePart = names(TSC_INTERVALS[TSC_INTERVALS == interval])[1] # TSC_INTERVALS[[as.character(metadata$interval)]] 
  }
  if(is.null(interval)){
    if(max(deltas) <= 25*3600){ ## Less than one day (25 for DST/StdT change), we can count on this to be pretty accurate
      interval = deltat(tsObject)/60
    } else {
      if(any(TSC_INTERVALS %in% deltas)){
        interval = TSC_INTERVALS[TSC_INTERVALS %in% deltas][1]
      } else {
        interval = 0
      }  
    }
  }
  if(fillData & interval > 0 & interval < 25*3600){
    fullTimes = seq(min(times), max(times), by=deltat(tsObject))
      blankTimes = fullTimes[!(fullTimes %in% times)]
      empties = xts(rep(J("hec/script/Constants")$UNDEFINED, length(blankTimes)), order.by=blankTimes)
      colnames(empties) = colnames(tsObject)
      tsObject = rbind(tsObject, empties)
  }
  ## Configure slots for TimeSeriesContainer object
  times = as.integer(index(tsObject))/60 + 2209075200/60
  values = as.numeric(tsObject)
  metadata = list(
    times = .jarray(as.integer(times), contents.class="java/lang/Integer"), #, as.integer(times)), new.class="java/lang/Integer")
    values = .jarray(values, contents.class="java/lang/Double"),
    endTime = max(times),
    startTime = min(times),
    numberValues = length(values),
    storedAsdoubles = TRUE,
    modified=FALSE,
    fileName="",
    ...
  )
  metadata$interval = interval
  
  dssMetadata = attr(tsObject, "dssMetadata")
  for(mdName in colnames(dssMetadata)){
    if(mdName %in% names(metadata)){
      next
    }
    #if(any(dssMetadata[[mdName]] != first(dssMetadata[[mdName]]))){
    #  warning(sprintf("Not all metadata matches for %s", mdName))
    #}
    metadata[[mdName]] = first(dssMetadata[[mdName]])
  }
  ## TODO: pull from protoTSC if required
  
  dPart = paste0("01JAN", year(first(index(tsObject))))
  metadata$fullName = paste("", metadata$watershed, metadata$location, metadata$parameter, dPart, ePart, metadata$version, "", sep="/")
  tsc = .jnew("hec/io/TimeSeriesContainer")
  tscFieldsDF = get("tscFieldsDF", envir=hecJavaObjectsDB)
  for(n in names(metadata)){
    #print(sprintf("%s:", n))
    #print(metadata[[n]])
    writeVal = metadata[[n]]
    if(is.na(writeVal) | writeVal == ""){
      #print("Value is NA, not writing.")
      next
    }
    if(is.factor(writeVal)){
      writeVal = as.character(writeVal)
    }
    if(tscFieldsDF$CLASS[tscFieldsDF$SHORTNAME == n] %in% c("int")){
      #print("Converting to integer.")
      writeVal = as.integer(writeVal)
    }
    .jfield(tsc, n) = writeVal
  }
  return(tsc)
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
  return(getTSC(file, paths[1], fullTSC=TRUE))  
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
#' Used to convert 24:00 timestamps back to proper date.
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
