#######
# files.R
# Functions for opening / working with DSS files
#######
# TODO: add easy-squeeze method

#' @title Open a DSS file.
#' 
#' @description
#' Returns an object from the java class 'hec.heclib.dss.HecDss' used for reading and writing to
#' the file located at filename.  Don't forget to call myFile$close() or myFile$done() when 
#' finished.
#' 
#' file stored in variable \code{dssfile} can be closed with \code{dssfile$close()} or \code{dssfile$done()}
#' 
#' @param filename Location of DSS file to open.
#' @return 'hec.heclib.dss.HecDss' object of DSS file at filename
#' @note This call failing saying that javaHeclib.dll cannot be found usually indicates that the \code{dss_location} configuration variable is not set correctly.
#' @author Evan Heisman
#' @export 
opendss <- function(filename, warnIfNew=TRUE, stopIfNew=FALSE){
  if(!file.exists(filename) & (warnIfNew | stopIfNew)){
    message = sprintf("DSS: %s does not exist.  Creating file.", filename)
    errFunc = warning
    if(stopIfNew){
      errFunc = stop
    }
    errFunc(message)
  }
  dssFile = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;", method="open", filename)
  return(dssFile)
}

#' @title Squeeze a DSS file
#' 
#' @description 
#' Calls squeeze method on file to remove deleted or overwritten data
#' 
#' @param file dss file object from `opendss`
#' 
#' @export
squeeze<-function(file){
  file$getDataManager()$squeeze()
<<<<<<< HEAD
}
=======
}
>>>>>>> 3ff4510602a389dd18b4de45cd63d3a601ff16c2
