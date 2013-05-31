## dssrt.r  "dessert"
## DSS - R interface
##
## Evan Heisman
## USACE NWW
##

library(xts)
library(rJava)
#source("dssrt.r") ## When ready
## initialize DSSVue Link
dss_location = "C:\\Users\\g4echeah\\AppData\\Roaming\\Microsoft\\Windows\\Start Menu\\Programs\\HEC-DSSVue2.1.42_03-30-2011-Jython2.5.2\\"
jars = c("hec", "heclib", "rma", "hecData") 
jars = paste0(dss_location, "jar\\", jars, ".jar")
libs = paste0("-Djava.library.path=", dss_location, "\\lib\\")
.jinit(classpath=jars, parameters=libs)


opendss <- function(filename){
	dssFile = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;", method="open", filename)
}

## get catalog to usable function
getPaths = function(file, ...){
	paths = file$getCatalogedPathnames(...)
	n = paths$size()
	myList = character()
	for(i in 1:n){
		myList[[i]] = paths$get(as.integer(i-1))
	}
	return(myList)
}

## convert time series container to TSC
tsc.to.xts <- function(tsc){
	times = hecTime.to.timestamp(hecTime(tsc$times))
	values = tsc$values
	return(xts(values, times))
}

## HEC int time to timestamp
hecTime <- Vectorize(function(timeInt){
	t = new(J("hec/heclib/util/HecTime"))
	t$set(as.integer(timeInt))
	return(t)
})

hecTime.to.timestamp <- function(t, translator=as.POSIXct){
	datetimes= sapply(t, .jcall, method="dateAndTime", returnSig="S")
	return(translator(datetimes, format="%d %B %Y, %H:%M"))
}

## Warning - does not check that all paths are the same except for D part
getFullTSC <- function(file, path){
	paths = getPaths(file, path)
	fullTSC = xts()
	for(p in paths){
		if(length(fullTSC)==0){
			fullTSC = tsc.to.xts(file$get(p))
		} else {
			fullTSC = rbind.xts(fullTSC, tsc.to.xts(file$get(p)))
		}
	}
	return(fullTSC)
}