## DSS - R interface project
## Evan Heisman

initialize.dssrip = function(as.package=F, dss_location=NULL, platform=NULL, jmemory=NULL){
  ## jmemory example: '-Xmx2g -Xms1g' to set up memory requirements for JVM to 2g heap and 1g stack.
  
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
    .jinit(classpath=jars, parameters=str_trim(paste(libs,jmemory)))
    require(xts)
  } else {
    lib = paste0(dss_location, path.sep, "lib", path.sep, "javaHeclib.dll")
    dyn.load(lib)
    .jpackage(pkgname, morePaths=jars)
  }
}

opendss <- function(filename){
	dssFile = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;", method="open", filename)
}

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

## get catalog to usable function
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

getPaths <- function(dssfile, pattern=NULL, searchfunction=fullPathByWildcard){
  paths = getAllPaths(dssfile)
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

nofilter <- function(paths, pattern){
  return(paths)
}

fullPathByWildcard <- function(paths, pattern){
  return(fullPathByRegex(paths, glob2rx(pattern)))
}

pathByPartsWildcard <- function(paths, pattern){
  ## TODO:  Replace "@" in pattern with "*", to match HEC wildcard set
  return(pathByPartsRegex(paths, pattern.parts=splitPattern(pattern, to.regex=T)))
}

fullPathByRegex <- function(paths, pattern){
  return(paths[grepl(pattern, paths)])
}

## useful function for writing filters
separatePathParts <- function(paths){
  parts.df = data.frame(do.call(rbind, str_split(paths, fixed("/")))[,2:7])
  colnames(parts.df) = toupper(letters[1:6])
  parts.df$PATH = paths
  return(parts.df)
}

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


## convert time series container to XTS
tsc.to.xts <- function(tsc){
	times = as.POSIXct(tsc$times*60, origin="1899-12-31 00:00")
	values = tsc$values
  out = xts(values, times)
  colnames(out) = tsc$parameter
	return(out)
}

## convert time series container to DT
tsc.to.dt <- function(tsc){
  require(data.table)
  times = as.POSIXct(tsc$times*60, origin="1899-12-31 00:00")
  values = tsc$values
  out = data.table(datetime=times,value=values)
  return(out)
}

getTSC <- function(file, path){
  return(tsc.to.xts(file$get(path)))
}

getDT <- function(file, path){
  return(tsc.to.dt(file$get(path)))
}

## Warning - does not check that all paths are the same except for D part
getFullTSC <- function(file, paths){
  tscList = list()
	for(p in paths){
    tscList[[p]] = getTSC(file, p)
	}
	return(do.call(rbind.xts, tscList))
}

getFullDT <- function(file, paths){
  require(data.table)
  dtList = list()
  for(p in paths){
    dtList[[p]] = getDT(file, p)
  }
  return(do.call(rbind, dtList))
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
