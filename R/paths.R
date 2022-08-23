####Heisman###
# paths.R
# Functions for finding records in a DSS file 
#######

#' @title Functions for searching pathnames in a DSS file
#' @name dss_path_functions
#' @author Evan 
#' @aliases getCatalogedPathnames getCondensedCatalog getAllPaths getPaths fullPathByWildcard fullPathByRegex pathByPartsWildcard pathByPartsRegex splitPattern separatePathParts pathsToDataFrame
#' @description Functions to search for path names in DSS files.
#' @usage
#' \code{getCatalogedPathnames(file)}
#' \code{getPaths(file, searchString)}
#' \code{getPaths(file, searchString, searchFunction, ...)}
#' \code{getAllPaths(file, rebuild=TRUE)}
#' \code{splitPattern(pattern)}
#' \code{separatePathParts(paths)}
#' @param file dss file from \code{opendss}
#' @param searchString string sent to filter for search, see detail section on filters. 
#' @param searchFunction function used to search, defaults to path by parts.
#' @param useRegex boolean to determine if regex or wildcards should be used.
#' @return list or data.frame of pathnames
#' @details
#' functions ending in \code{Wildcard} use the standard "*" to match wildcard characters, converting search strings with \code{\link{glob2rx}}.  Search functions ending in \code{Regex} use the \code{\link{grepl}} function to test parts.
#' 
#' \code{fullPath} functions expect a six part full path name (e.g. \code{/A/B/C/D/E/F/}), with asterisks for unknown parts, as blanks match blank path parts. 
#' 
#' \code{pathByParts} functions use the format "\code{A=PATTERN B=PATTERN}" for their search string.  Parts not specified will be searched for as a wildcard
#' 
#' \code{getCatalogedPathnames} and \code{getCondensedCatalog} calls `getCatalogedPathnames` and `getCondensedCatalog` functions in Java API and converts result to R character vector.
#' 
#' \code{getAllPaths} tries to read the DSS catalog file (does not work in R 4.0) instead of calling Java API and should be deprecated.
#' 
#' See Page 8-32 of the HEC-DSSVue manual for further examples on the wildcard methods.  The "at" character cannot be used as a wildcard.  
#' 
#' Custom search functions can be written to take the first two parameters as the full list of paths as a character vector, and a search pattern string.  
#' The search pattern string can be provided as an empty string with other parameters provided for the search. \code{splitPattern} will split a path by parts pattern into search terms, and \code{separatePathParts} will split a full path pattern into search terms.
#' 
#' @examples
#' 
#' # recommended search technique:
#' paths = pathsToDataFrame(getCatalogedPathnames(dssfile), simplify=T)
#' bwCreekStagePaths = subset(paths, LOCATION="BRANDYWINE CREEK", PARAMETER="STAGE")$PATH
#' 
#' # these can be a little flaky.
#' getPaths(dssfile, "A=BRANDYWINE CREEK B=WILMINGTON, DE C=STAGE F=USGS")
#' getPaths(dssfile, "A=BRANDYWINE CREEK B=WILMINGTON, DE C=STAGE F=USGS", pathByPartsWildcard)
#' getPaths(dssfile, "A=BRANDYWINE CREEK B=WILMINGTON, DE C=STAGE F=US.*", pathByPartsRegex)
#' getPaths(dssfile, "/BRANDYWINE CREEK/WILMINGTON, DE/STAGE/*/*/USGS/")
#' getPaths(dssfile, "/BRANDYWINE CREEK/WILMINGTON, DE/STAGE/*/*/USGS/", fullPathByWildcard)
#' getPaths(dssfile, "/BRANDYWINE CREEK/WILMINGTON, DE/STAGE/.*/.*/USGS/", fullPathByRegex)
#' getAllPaths(dssfile)
#' getAllPaths(dssfile, rebuild=TRUE)

#' @export 
getAllPaths <- function(file, rebuild=FALSE){
  # left this function for backwards compatibility purposes, still does check if rebuilding the catalog is needed.
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
    rebuild = TRUE
  
  return(getCatalogedPathnames(file, forceRebuild=rebuild))
}

#' @export
getCatalogedPathnames <- function(file, forceRebuild=FALSE){
  # assume rebuild is false, let DSS api decide to update
  .javaVectorToStrings(file$getCatalogedPathnames(forceRebuild))
}

#' @export
getCondensedCatalog <- function(file){
  .javaVectorToStrings(file$getCondensedCatalog())
}

#' @export 
getPaths <- function(file, searchString="/*/*/*/*/*/*/", 
                     searchfunction=NULL, pattern=searchString, searchFunction=searchfunction, 
                     useRegex=FALSE, ...){
  searchString = str_trim(searchString)
  if(is.null(searchFunction)){
    if(grepl(pattern=fixed("="), x=searchString)){
      searchFunction = pathByPartsWildcard
      if(useRegex){
        searchFunction = pathByPartsRegex
      }
    } else if(grepl(pattern="^/.*/.*/.*/.*/.*/.*/$", x=searchString)) {
      searchFunction = fullPathByWildcard
      if(useRegex){
        searchFunction = fullPathByRegex
      }
    } else{
      warning("No search function specified and could not be automatically selected.")
      searchFunction = nofilter
    }
  }
  paths = getAllPaths(file)
  if(!is.null(searchFunction)){
    paths = searchFunction(paths, searchString, ...)
  }
  return(paths)
}
#' @export
splitPattern <- function(pattern, to.regex=FALSE){
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

#' @export 
fullPathByWildcard <- function(paths, pattern){
  return(fullPathByRegex(paths, glob2rx(pattern)))
}

#' @export 
pathByPartsWildcard <- function(paths, pattern){
  ## TODO:  Replace "\@" in pattern with "*", to match HEC wildcard set
  return(pathByPartsRegex(paths, pattern.parts=splitPattern(pattern, to.regex=T)))
}

#' @export 
fullPathByRegex <- function(paths, pattern){
  return(paths[grepl(pattern, paths)])
}

#' @export 
separatePathParts <- function(paths){
  parts.df = data.frame(rbind(do.call(rbind, str_split(paths, fixed("/")))[,2:7]), stringsAsFactors=FALSE)
  colnames(parts.df) = toupper(letters[1:6])
  parts.df$PATH = paths
  return(parts.df)
}

# This should replace/be merged with separatePathParts
# Adds ability to convert paths to have blank d-parts for reading 'condensed' records.
#' @export
pathsToDataFrame = function(paths, simplify=TRUE, letterLabels=FALSE){
    labels = c("WATERSHED", "LOCATION", "PARAMETER", "DATERANGE", "INTERVAL", "VERSION")
    if(letterLabels & !simplify){
        labels = paste0(LETTERS[1:6], ".PART")
    }
    pathsDF = ldply(paths, function(path){
        splitPath = str_split(path, fixed("/"))[[1]]
        splitPath = splitPath[2:7]
        names(splitPath) = labels
        splitPath[["PATH"]] = path
        return(splitPath)
    })
    if(simplify){
        ## merge paths with same parts, except for D. good for getTSC for the whole range
        pathsDF = ddply(pathsDF, labels[c(1:3,5:6)], summarize, 
                        FIRST.PATH = first(DATERANGE),
                        LAST.PATH = last(DATERANGE),
                        # TODO: fix the next line so it works with both full labels and letter labels
                        PATH=first(sprintf("/%s/%s/%s//%s/%s/", WATERSHED, LOCATION, PARAMETER, INTERVAL, VERSION)))
    }
    return(pathsDF)
}


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
