#' @title DSS-Rip Initalization Options
#' @author Evan Heisman
#' @description
#' Starts a rJava JVM with configuration for DSS-Vue's jar and dll files.
#' 
#' @details
#' as.package is an experimental parameter for calling this as part of the onLoad function as part
#'   of the DSS-Rip package.  This is the prefered method for an R package, but not yet functional
#'   for this.  The best practice is to load the DSS-Rip package, and call initialize.dssrip with
#'   as.package=FALSE, the default value.  Either the 'nativeLibrary' parameter for .jpackage, or
#'   as a dyn.load, would be the place to load javaHeclib.dll, but rather than distribute it with 
#'   this R package, the user should obtain it from an install of HEC-DSSVue.  Further reasons to 
#'   use .jinit include being able to initialize the JVM in the same manner as HEC-DSSVue would, 
#'   adding the appropriate jars and DLLs as start up options.
#' TODO Implement so that dssrip can be loaded after other rJava based packages
#' TODO Quiet DSS status messages
#' @param as.package If true, uses .jpackage instead of .jinit for better encapsulation of module. (Buggy!)
#' @param dss_location Specify location of DSSVue libraries if not in default location.
#' @param platform Specify platform, used in determining default DSS location.
#' @param quietDSS - don't show 'Z' messages during opening, reading, and writing to a file.  Experimental.
#' @param parameters Options string to pass to JVM.
#' @return JVM initialization status - 0 if successful, positive for partial initialization, negative for failure.  See ?.jinit 
initialize.dssrip = function(pkgname=NULL, lib.loc,
                             dss_location=getOption("dss_location"), 
                             dss_jre=getOption("dss_jre_location"),
                             platform=NULL, quietDSS=T, verboseLib=F, parameters=NULL, ...){
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
  
  ## Set JRE location
  if(!is.null(dss_jre)){
    Sys.setenv(JAVA_HOME=dss_jre)
  } #else {
  #   if(version$arch=="x86_64"){
  #     Sys.setenv(JAVA_HOME="")
  #   }
  # }
  if(verboseLib) packageStartupMessage(sprintf("JRE location is %s\n", Sys.getenv("JAVA_HOME")))
  
  ## Set DSS location
  if(is.null(dss_location)){
    if(platform == "windows"){
      dss_location = paste0(Sys.getenv("ProgramFiles(x86)"), path.sep, "HEC", path.sep, "HEC-DSSVue")
    } else {
      dss_location = Sys.getenv("DSS_HOME")
    }
  }
  if(verboseLib) packageStartupMessage(sprintf("DSS Location is %s\n", dss_location))
  
  jars = paste0(dss_location, path.sep, "jar", path.sep, c("hec", "heclib", "rma", "hecData"), ".jar")
  require(rJava)
  
  if(is.null(pkgname)){ ## Loading outside of onLoad function
    require(rJava)
    require(stringr)
    require(xts)
    libs = paste0("-Djava.library.path=", dss_location, path.sep, "lib", path.sep)
    if(verboseLib) packageStartupMessage(str_trim(paste(libs,parameters)))
    
    #LOGS='-Dlogfile.directory="%APPDATA%/HEC/HEC-DSSVue/logs" -DLOGFILE="%APPDATA%/HEC/HEC-DSSVUE/logs/HEC-DSSVue.log" -DCACHE_DIR="%APPDATA%/HEC/HEC-DSSVue/pythonCache"'
    #MEMPARAMS="-ms256M -mx2000M"
    .jinit(classpath=jars, parameters=str_trim(paste(libs,parameters)), ...)
    #.jaddClassPath(jars)
    if(verboseLib){
      for(jpath in .jclassPath()){
        packageStartupMessage(jpath)
      }
    }
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
    ## None of the below work
    ## TODO:  Try this with a temporary file instead of NULL
    #opt 1
    #.jcall("java/lang/System", returnSig='V', method="setOut", .jnull())
    #opt 2
    #nullPrintStream = .jnew("java/lang/System/PrintStream", paste0(dss_location, path.sep, "dssrip_temp.txt"))
    #.jcall("java/lang/System", returnSig='V', method="setOut", nullPrintStream)
    #opt 3: See heclib programmers manual for this trick.
    messageLevel = 2 # only print errors
    .jcall("hec/heclib/util/Heclib", returnSig='V', method="zset", 'MLEVEL', ' ', as.integer(messageLevel))
  }
}

TSC_TYPES = c("INST-VAL", "INST-CUM", "PER-AVER", "PER-CUM")
minutes = c(1,2,3,4,5,6,10,12,15,20,30)
hours = c(1,2,3,4,6,8,12)
TSC_INTERVALS = c(minutes, 60*hours, 60*24*c(1,7,10,15,30,365), rep(0,5))
## Irregular appears to have interval of 0, not -1
names(TSC_INTERVALS) = c(paste0(minutes, "MIN"),
                         paste0(hours, "HOUR"),
                         "1DAY","1WEEK","TRI-MONTH","SEMI-MONTH", "1MON","1YEAR",
                         paste0("IR-",c("DAY","MON","YEAR","DECADE","CENTURY")))


## Only useful when working in package directory!
openTestFile <- function(){
  opendss("./extdata/test.dss")
}

## Convenience function for viewing a DSS file.  DOES NOT WORK
#' @export
newDSSVueWindow <- function(file=NULL){
  mw = .jcall("hec/dssgui/ListSelection",
              returnSig="Lhec/dssgui/ListSelection;",
              method="createMainWindow")
  mw = .jnew("hec/dssgui/ListSelection")
  mw.show()
  if(!is.null(file)){
    mw.openDSSFile(file)
  }
  return(mw)
}

## used to help with introspection on Java Objects
sigConversions = list(boolean="Z", byte="B", char="C", 
                      short="T", void="V", int="I", 
                      long="J", float="F", double="D")
fieldsDF <- function(jObject){
  require(plyr)
  fields = ldply(.jfields(jObject), function(x) data.frame(FULLNAME=x, 
                                                           SHORTNAME=last(str_split(x, fixed("."))[[1]]), 
                                                           CLASS=str_split(x, fixed(" "))[[1]][2], 
                                                           stringsAsFactors=FALSE))
  fields$SIGNATURE = llply(fields$CLASS, function(x){
    out = str_replace_all(x, "\\[\\]", "")
    if(out %in% names(sigConversions)){
      out = sigConversions[[out]]
    } else {
      out = paste0("L", str_replace_all(out, fixed("."), "/"), ";")
    }
    ## If vector, add [
    if(grepl(fixed("\\[\\]"), x)){
      out = paste0("[", out)
    }
    return(out)
  })
  return(fields)
}




#' getMetadata get metadata from a tsc java object 
#' 
#' get metadata from a tsc java object 
#' 
#' Long Description
#' 
#' @return data.frame containing metadata 
#' @note NOTE
#' @author Evan Heisman
#' @export 
getMetadata <- function(tsc, colnamesSource="parameter"){
  require(stringr)
  require(plyr)
  tscFieldsDF = get("tscFieldsDF", envir=hecJavaObjectsDB)
  metadata = dlply(tscFieldsDF, "SHORTNAME", function(df){
    #cat(sprintf("%s\t%s\t%s\n", df$FULLNAME, df$SHORTNAME, df$SIGNATURE))
    if(df$SHORTNAME %in% c("values", "times", "modified", "quality")) {
      return()
    }
    val = .jfield(tsc, name=df$SHORTNAME, sig=as.character(df$SIGNATURE))
    if(.jnull() == val){
      return(NA)
    }
    return(val)
  })
  metadata = metadata[!(names(metadata) %in% c("values", "times", "modified", "quality"))]

  return(metadata)
}
