## dssrt.r  "dessert"
## DSS - R interface
##
## Evan Heisman
## Provided as is.

if("package:rJava" %in% search()){
    warning("package rJava already loaded")
    ## Until this becomes a formal package and I can use the package version of rJava
}

library(xts)
library(rJava)
#source("dssrt.r") ## When ready
## initialize DSSVue Link
## sets to the default location - change if installed elsewhere
## assumes a 64-bit Windows system
## should work with Solaris / Linux version if correct path is set, and path separators are changed below
if(!exists("dss_location")){
        warning("variable 'dss_location' was undefined.  Trying default in 'C:\\Program Files (x86)'.")
	dss_location = "C:\\Program Files (x86)\\HEC/HEC-DSSVue\\" ## set this to the path to your DSSVue library
}
jars = c("hec", "heclib", "rma", "hecData")
jars = paste0(dss_location, "jar\\", jars, ".jar")
libs = paste0("-Djava.library.path=", dss_location, "\\lib\\")
.jinit(classpath=jars, parameters=libs)


opendss <- function(filename){
	dssFile = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;", method="open", filename)
}

OLDgetPaths = function(file, ...){
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


## get catalog to usable function
getAllPaths = function(file){
	paths = file$getCatalogedPathnames()
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

getPaths = function(dssfile, pattern=NULL, searchfunction=fullPathByWildcard){
  paths = getAllPaths(dssfile)
  if(!is.null(searchfunction)){
    paths = searchfunction(paths, pattern)
  }
  return(paths)
}

nofilter = function(paths, pattern){
  return(paths)
}

fullPathByWildcard = function(paths, pattern){
  return(fullPathByRegex(paths, glob2rx(pattern)))
}

pathByPartsWildcard= function(paths, pattern){
  ## TODO:  Replace "@" in pattern with "*", to match HEC wildcard set
  return(pathByPartsRegEx(paths, pattern.parts=splitPattern(pattern, to.regex=T)))
}

fullPathByRegex = function(paths, pattern){
  return(paths[grepl(pattern, paths)])
}

splitPattern = function(pattern, to.regex=F){
  ## For use in the pathByParts searches
  if(!grepl(fixed("="), pattern)){
    warning(paste0("Bad pattern: ", pattern))
  }
  pattern.raw = str_split(pattern, "=")[[1]]
  keys = pattern.raw[1:(length(pattern.raw)-1)]
  keys = str_trim(substr(keys, str_length(firsts)-1, str_length(firsts)))
  values = pattern.raw[2:(length(pattern.raw))]
  values = str_trim(substr(values, 1, str_length(values)-c(rep(1,length(values)-1),0)))
  if(to.regex) values = glob2rx(values)
  values = as.list(values)
  names(values) = keys
  return(values)
}

pathByPartsRegEx = function(paths, pattern, pattern.parts=NULL){
  parts.df = data.frame(do.call(rbind, str_split(paths, fixed("/")))[,2:7])
  colnames(parts.df) = toupper(letters[1:6])
  parts.df$PATH = paths
  if(is.null(pattern.parts)){
    pattern.parts = splitPattern(pattern, to.regex=T)
  }
  parts.df$MATCH = T
  for(n in names(pattern.parts)){
    parts.df$MATCH = parts.df$MATCH & grepl(pattern.parts[[n]], parts.df[,n])
  }
  return(subset(parts.df, MATCH)$PATH)
}

treesearch = function(paths, pattern){
  warning("treesearch not yet implemented")
  return(paths)
}


system.time(getPaths(dssfile, pattern="A=FCST B=TDA C=VOLUME*APR-AUG*", searchfunction=pathByPartsWildcard))



## convert time series container to TSC
tsc.to.xts <- function(tsc){
	times = as.POSIXct(tsc$times*60, origin="1899-12-31 00:00")
      #times = hecTime.to.timestamp(hecTime(tsc$times))
	values = tsc$values
      out = xts(values, times)
      colnames(out) = tsc$parameter
	return(out)
}

## HEC int time to timestamp
hecTime <- Vectorize(function(timeInt){
	t = new(J("hec/heclib/util/HecTime"))
	t$set(as.integer(timeInt))
	return(t)
})

hecTime.to.timestamp <- function(t, translator=as.POSIXct){
	datetimes = sapply(t, .jcall, method="dateAndTime", returnSig="S")
	return(translator(datetimes, format="%d %B %Y, %H:%M"))
}

## Compute HECTime directly
hecTimeDirect <- function(t){
  return(as.POSIXct(t$getTimeInMillis()/1000, "1970-01-01 00:00"))
}

getTSC <- function(file, path){
  return(tsc.to.xts(file$get(path)))
}

## Warning - does not check that all paths are the same except for D part
getFullTSC <- function(file, path){
	paths = getPaths(file, path)
	tscList = list()
	for(p in paths){
    tscList[[p]] = tsc.to.xts(file$get(p))
	}
	return(do.call(rbind.xts, tscList))
}

## PairedDataContainer functions

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
    return(pdc$yOrdinates[which(pdc$labels == column),])
  }
}
