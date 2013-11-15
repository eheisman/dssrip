## DSS - R interface project
## Evan Heisman

#' initialize.dssrip Starts JVM with configuration for DSS-Vue's jar and dll files.
#'
#'Starts JVM with parameters required to use HEC's jar files.
#' 
#' as.package is an experimental parameter for calling this as part of the onLoad function as part
#'   of the DSS-Rip package.  This is the prefered method for an R package, but not yet functional
#'   for this.  The best practice is to load the DSS-Rip package, and call initialize.dssrip with
#'   as.package=FALSE, the default value.  Either the 'nativeLibrary' parameter for .jpackage, or
#'   as a dyn.load, would be the place to load javaHeclib.dll, but rather than distribute it with 
#'   this R package, the user should obtain it from an install of HEC-DSSVue.  Further reasons to 
#'   use .jinit include being able to initialize the JVM in the same manner as HEC-DSSVue would, 
#'   adding the appropriate jars and DLLs as start up options.
#' 
#' @param as.package If true, uses .jpackage instead of .jinit for better encapsulation of module. (Buggy!)
#' @param dss_location Specify location of DSSVue libraries if not in default location.
#' @param platform Specify platform, used in determining default DSS location.
#' @param quietDSS - don't show 'Z' messages during opening, reading, and writing to a file.  Experimental.
#' @param parameters Options string to pass to JVM.
#' @return JVM initialization status - 0 if successful, positive for partial initialization, negative for failure.  See ?.jinit
#' @note NOTE
#' @author Evan Heisman
#' @export 
initialize.dssrip = function(pkgname=NULL, lib.loc,
                             dss_location=getOption("dss_location"), platform=NULL, quietDSS=F, verboseLib=F, parameters=NULL, ...){
  ## parameters examples: '-Xmx2g -Xms1g' to set up memory requirements for JVM to 2g heap and 1g stack.
  
  ## TODO:  Add check if DSSRip is already initialized, exit function and return nothing 
  ##        if not "force.reinit=T" with warning message
  
  if(is.null(platform)){
    platform = tolower(Sys.info()[["sysname"]])
  }
  path.sep = "/"
  library.ext = ".so"
  if(platform == "windows"){
    path.sep = "\\"
    library.ext = ".dll"
  }
  if(is.null(dss_location)){
    if(platform == "windows"){
      dss_location = paste0(Sys.getenv("ProgramFiles(x86)"), path.sep, "HEC", path.sep, "HEC-DSSVue", path.sep)
    } else {
      dss_location = Sys.getenv("DSS_HOME")
    }
  }
  
  jars = paste0(dss_location, "jar", path.sep, c("hec", "heclib", "rma", "hecData"), ".jar")
  
  if(is.null(pkgname)){ ## Loading outside of onLoad function
    if(version$arch=="x86_64" & Sys.getenv("JAVA_HOME")!="")
      Sys.setenv(JAVA_HOME="") ## rJava on 64-bit R has problems when JAVA_HOME is set and it can't find the JRE.
    require(rJava)
    require(stringr)
    require(xts)
    libs = paste0("-Djava.library.path=", dss_location, "lib", path.sep)
    if(verboseLib) cat(str_trim(paste(libs,parameters))); cat("\n")
    return(.jinit(classpath=jars, parameters=str_trim(paste(libs,parameters)), ...))
  } else {
    libdir = paste0(dss_location, "lib", path.sep)
    #dyn.load(lib)
    .jpackage(pkgname, lib.loc, morePaths=jars)
    ## Add javaHeclib.dll to loaded libraries.
    #.jcall("java/lang/System", returnSig='V', method="load", lib)
    Sys.setenv(PATH=paste0(Sys.getenv("PATH"), ";", dss_location, ";", libdir))
    lib = paste0(libdir, "javaHeclib.dll")
    .jcall("java/lang/System", returnSig='V', method="load", lib)
    .jcall("java/lang/System", returnSig='V', method="loadLibrary", "javaHeclib")
  }
  if(quietDSS){
    ## Neither works
    ## TODO:  Try this with a temporary file instead of NULL
    #.jcall("java/lang/System", returnSig='V', method="setOut", .jnull())
    ## See heclib programmers manual for this trick.
    #.jcall("hec/heclib/util/Heclib", returnSig='V', method="zset", 'MLVL', ' ', 0)
  }
}

test.quiet <- function(){
  initialize.dssrip(quietDSS=T, verboseLib=T, force.init=T)
  foobar = opendss("./extdata/test.dss")
}


## useful function for indexing by water year

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


#' opendss Opens a DSS file.
#' 
#' Returns a DSS file object.
#' 
#' Returns an object from the java class 'hec.heclib.dss.HecDss' used for reading and writing to
#' the file located at filename.  Don't forget to call myFile$close() or myFile$done() when 
#' finished.
#' 
#' @param filename Location of DSS file to open.
#' @return 'hec.heclib.dss.HecDss' object of DSS file at filename
#' @note NOTE
#' @author Evan Heisman
#' @export 
opendss <- function(filename){
	dssFile = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;", method="open", filename)
  return(dssFile)
}

## Deprecated function - DO NOT USE.
OLDgetPaths <- function(file, ...){
  warning("This function calls the getCatalogedPathnames function and can take some time.")
  warning("OLDgetPaths is deprecated.  Please replace.")

	paths = file$getCatalogedPathnames(...)
	n = paths$size()
  if(n==0){
    return(list())
  }
	myList = character()
	for(i in 1:n){
		myList[[i]] = paths$get(as.integer(i-1))
	}
	return(myList)
}

#' getAllPaths Returns all paths in DSS file.
#' 
#' Returns a list of all DSS paths in a file, useful for searching for data.
#' 
#' Long Description
#' 
#' @param file a DSS file reference from opendss
#' @param rebuild Set to true to force rebuilding the DSS catalog file (.dsc).
#' @return a character vector of DSS paths in the file.
#' @note NOTE
#' @author Evan Heisman
#' @export 
getAllPaths <- function(file, rebuild=FALSE){
  require(stringr)
  dss_fn = file$getFilename()
  dsc_fn = sprintf('%s.dsc',tools:::file_path_sans_ext(dss_fn))
  dsc_exists = file.exists(dsc_fn)

  dsc_mtime = file.info(dsc_fn)$mtime
  dss_mtime = file.info(dss_fn)$mtime
    # this will force the recreation of the catalog file if:
    # 1. it does not exist
    # 2. it is older than the dss file
    # 3. a rebuild is forced with rebuild=TRUE
  if(!isTRUE(dsc_exists) | isTRUE(dss_mtime > dsc_mtime) | isTRUE(rebuild))
    file$getCatalogedPathnames(TRUE)
  
  #meta = read.table(dsc_fn,skip=10,stringsAsFactors=FALSE)
  #paths = meta[,ncol(meta)]
  dsc = readLines(dsc_fn)
  paths = dsc[11:length(dsc)]
  paths = str_sub(paths,19,str_length(paths))  
  return(paths)
}

#' getPaths Search DSS paths by filter.
#' 
#' Allows searching DSS paths similar to getCatalogedPathnames(searchPattern) in the Jython API.
#' 
#' Uses the pattern parameter to return a filtered list of paths.  The filter method is defined by 
#' the searchfunction parameter.
#' 
#' @param file DSS file reference
#' @param pattern Search string
#' @param searchfunction Filter function to use with search string
#' @return character vector of paths matching filter criteria.
#' @note NOTE
#' @author Evan Heisman
#' @export 
getPaths <- function(file, pattern=NULL, searchfunction=fullPathByWildcard){
  #TODO - detect pattern type and select search function appropriately.
  paths = getAllPaths(file)
  if(!is.null(searchfunction)){
    paths = searchfunction(paths, pattern)
  }
  return(paths)
}

splitPattern <- function(pattern, to.regex=F){
  ## For use in the pathByParts searches
  if(!grepl(fixed("="), pattern)){
    warning(paste0("Bad pattern: ", pattern))
  }
  pattern.raw = str_split(pattern, "=")[[1]]
  keys = pattern.raw[1:(length(pattern.raw)-1)]
  keys = str_trim(substr(keys, str_length(keys)-1, str_length(keys)))
  values = pattern.raw[2:(length(pattern.raw))]
  values = str_trim(substr(values, 1, str_length(values)-c(rep(1,length(values)-1),0)))
  if(to.regex) values = glob2rx(values)
  values = as.list(values)
  names(values) = keys
  return(values)
}

## A template / placeholder filter function.
nofilter <- function(paths, pattern){
  return(paths)
}

#' fullPathByWildcard Search paths by wildcard.
#' 
#' Searches full paths by wildcard, e.g. "/A/B/C/*/*/F/"
#' 
#' Long Description
#'  
#' @return stuff
#' @note NOTE
#' @author Evan Heisman
#' @export 
fullPathByWildcard <- function(paths, pattern){
  return(fullPathByRegex(paths, glob2rx(pattern)))
}

#' pathByPartsWildcard Search paths by parts, using wildcards.
#' 
#' Searches path by individual parts, e.g. "A=*CREEK* C=FLOW"
#' 
#' Long Description
#' 
#' @return stuff
#' @note NOTE
#' @author Evan Heisman
#' @export 
pathByPartsWildcard <- function(paths, pattern){
  ## TODO:  Replace "@" in pattern with "*", to match HEC wildcard set
  return(pathByPartsRegex(paths, pattern.parts=splitPattern(pattern, to.regex=T)))
}

#' fullPathByRegex Search full paths with regex.
#' 
#' Searches full paths using regular expressions, e.g. "/A/B/C/.*/.*/F/"
#' 
#' Long Description
#' 
#' @return stuff
#' @note NOTE
#' @author Evan Heisman
#' @export 
fullPathByRegex <- function(paths, pattern){
  return(paths[grepl(pattern, paths)])
}

#' separatePathParts Separates path parts into dataframe.
#' 
#' useful function for writing filters
#' 
#' Long Description
#' 
#' @return data frame consisting of split path parts and full paths.
#' @note NOTE
#' @author Evan Heisman
#' @export 
separatePathParts <- function(paths){
  parts.df = data.frame(do.call(rbind, str_split(paths, fixed("/")))[,2:7])
  colnames(parts.df) = toupper(letters[1:6])
  parts.df$PATH = paths
  return(parts.df)
}

#' pathByPartsRegex Search path by parts using regex.
#' 
#' Searches path by parts using regular expressions, e.g. "A=.*CREEK.* C=FLOW"
#' 
#' Long Description
#' 
#' @return stuff
#' @note NOTE
#' @author Evan Heisman
#' @export 
pathByPartsRegex <- function(paths, pattern, pattern.parts=NULL){
  parts.df = separatePathParts(paths)
  if(is.null(pattern.parts)){
    pattern.parts = splitPattern(pattern, to.regex=T)
  }
  parts.df$MATCH = T
  for(n in names(pattern.parts)){
    parts.df$MATCH = parts.df$MATCH & grepl(pattern.parts[[n]], parts.df[,n])
  }
  return(subset(parts.df, MATCH)$PATH)
}

treesearch <- function(paths, pattern){
  warning("treesearch not yet implemented")
  return(paths)
}


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
tsc.to.xts <- function(tsc){
  times = as.POSIXct(tsc$times*60, origin="1899-12-31 00:00")
  values = tsc$values
  out = xts(values, times)
  colnames(out) = tsc$parameter
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
tsc.to.dt <- function(tsc){
  require(data.table)
  times = as.POSIXct(tsc$times*60, origin="1899-12-31 00:00")
  values = tsc$values
  units = tsc$units
  if(length(values)==0)units = character(0)
  out = data.table(datetime=times,value=values,units=units)
  setkey(out, "datetime")
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
getTSC <- function(file, path){
  return(tsc.to.xts(file$get(path)))
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
getFullTSC <- function(file, paths){
  tscList = list()
  for(p in paths){
    tscList[[p]] = getTSC(file, p)
  }
  return(do.call(rbind.xts, tscList))
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
getFullDT <- function(file, paths){
  require(data.table)
  dtList = list()
  for(p in paths){
    dtList[[p]] = getDT(file, p)
  }
  return(do.call(rbind, dtList))
}

## PairedDataContainer functions

#' getColumnsByName Get a column from a PairedDataContainer object.
#' 
#' Gets a column from a paired data container by name.
#' 
#' Name of column must be exact or NA is returned.
#' 
#' @return vector of values from column.
#' @note NOTE
#' @author Evan Heisman
#' @export 
getColumnsByName <- function(file, pdc, column){
  if(class(file)=="character"){
    file = opendss(file)
  }
  if(class(pdc)=="character"){
    pdc = file$get(pdc)
  }
  if(class(column) != "character"){
    return(pdc$yOrdinates[column,])
  } else {
    if(!(column %in% pdc$labels)){
      warning(sprintf("No column named [%s] found in paired data container.", column))
      return(NA)
    }
    return(pdc$yOrdinates[which(pdc$labels == column),])
  }
}
