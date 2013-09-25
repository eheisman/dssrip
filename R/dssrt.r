## DSS - R interface project
## Evan Heisman

#' initialize.dssrip
#' 
#' Starts JVM with parameters required to use HEC's jar files.
#' 
#' Long Description
#' 
#' @param as.package If true, uses .jpackage instead of .jinit for better encapsulation of module. (Buggy!)
#' @param dss_location Specify location of DSSVue libraries if not in default location.
#' @param platform Specify platform, used in determining default DSS location.
#' @param joptions Options string to pass to JVM.
#' @return Nothing useful returned.
#' @note NOTE
#' @author Evan Heisman
#' @export 
initialize.dssrip = function(as.package=F, dss_location=NULL, platform=NULL, joptions=NULL){
  ## joptions example: '-Xmx2g -Xms1g' to set up memory requirements for JVM to 2g heap and 1g stack.
  
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
  if(!as.package){
    require(rJava)
    require(stringr)
    libs = paste0("-Djava.library.path=", dss_location, path.sep, "lib", path.sep)
    .jinit(classpath=jars, parameters=str_trim(paste(libs,joptions)))
    require(xts)
  } else {
    lib = paste0(dss_location, path.sep, "lib", path.sep, "javaHeclib.dll")
    dyn.load(lib)
    .jpackage(pkgname, morePaths=jars)
  }
}

#' opendss
#' 
#' Returns a DSS file object.
#' 
#' Returns an object from the java class 'hec.heclib.dss.HecDss' used for reading and writing to
#' the file located at filename.  Don't forget to call myFile$close() or myFile$done() to when 
#' finished.
#' 
#' @param filename Location of DSS file to open.
#' @return stuff
#' @note NOTE
#' @author Evan Heisman
#' @export 
opendss <- function(filename){
	dssFile = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;", method="open", filename)
  return(dssFile)
}

## Legacy function
OLDgetPaths <- function(file, ...){
  warning("This function calls the getCatalogedPathnames function and can take some time.")
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

#' getAllPaths
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

#' getPaths
#' 
#' Allows searching DSS paths similar to getCatalogedPathnames(string searchPattern) in the Jython API.
#' 
#' Uses the pattern parameter to return a filtered list of paths.  The filter method is defined by the searchfunction parameter.
#' 
#' @param file DSS file reference
#' @param pattern Search string
#' @param searchfunction Filter function to use with search string
#' @return stuff
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

#' fullPathByWildcard
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

#' pathByPartsWildcard
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

#' fullPathByRegex
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

#' separatePathParts
#' 
#' useful function for writing filters
#' 
#' Long Description
#' 
#' @return stuff
#' @note NOTE
#' @author Evan Heisman
#' @export 
separatePathParts <- function(paths){
  parts.df = data.frame(do.call(rbind, str_split(paths, fixed("/")))[,2:7])
  colnames(parts.df) = toupper(letters[1:6])
  parts.df$PATH = paths
  return(parts.df)
}

#' pathByPartsRegex
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


#' tsc.to.xts
#' 
#' convert time series container to XTS
#' 
#' Long Description
#' 
#' @return stuff
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

#' tsc.to.dt
#' 
#' convert time series container to data.table
#' 
#' Long Description
#' 
#' @return stuff
#' @note NOTE
#' @author Cameron Bracken
#' @export 
tsc.to.dt <- function(tsc){
  require(data.table)
  times = as.POSIXct(tsc$times*60, origin="1899-12-31 00:00")
  values = tsc$values
  out = data.table(datetime=times,value=values)
  return(out)
}

#' getTSC
#' 
#' Short Desc
#' 
#' Long Description
#' 
#' @return stuff
#' @note NOTE
#' @author Evan Heisman
#' @export 
getTSC <- function(file, path){
  return(tsc.to.xts(file$get(path)))
}

#' detDT
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

#' getFullTSC
#' 
#' Gets paths, converts to XTS, and merges to one time series.
#' 
#' Warning - does not check that all paths are the same except for D part
#' 
#' @return stuff
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

#' getFullDT
#' 
#' Gets paths, converts to data.table, and merges to one time series.
#' 
#' Long Description
#' 
#' @return stuff
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

#' getColumnsByName
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
