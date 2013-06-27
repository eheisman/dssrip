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

## get catalog to usable function
getPaths = function(file, ...){
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
