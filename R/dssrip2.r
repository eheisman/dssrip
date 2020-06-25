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
#' @param quietDSS - if true, don't show 'Z' messages during opening, reading, and writing to a file.
#' @param parameters list of options string to pass to JVM.
#' @param setJavaLoc - override Java location with one in config
#' @param verbose - set to true for debuggering
#' @return JVM initialization status - 0 if successful, positive for partial initialization, negative for failure.  See ?.jinit 
initialize.dssrip2 = function(pkgname=NULL, quietDSS=T, parameters=options()[["dss_jvm_parameters"]], setJavaLoc=FALSE, verbose=FALSE, ...){
  ## parameters examples: '-Xmx2g -Xms1g' to set up memory requirements for JVM to 2g heap and 1g stack.

  require(rJava)
  require(stringr)
  require(xts)
    
  config = dssConfig()
  dss_location = config$dss_location # avoid using this if dssConfig returns value needed
  if(verbose) packageStartupMessage(sprintf("DSS Location is %s\n", dss_location))
  jars = config$jars
  libs = config$libs
  java = config$java
  config = config$config
  path.sep = config$path.sep
  lib.ext = config$lib.ext
  #library.ext = config$library.ext
  
  ## Set JRE location
  if(setJavaLoc){
    Sys.setenv(JAVA_HOME=java)
  }
  if(verbose) packageStartupMessage(sprintf("JRE location is %s\n", Sys.getenv("JAVA_HOME")))

  
  # is this necessary?
  #Sys.setenv(PATH=paste0(Sys.getenv("PATH"), ";", dss_location, ";", libdir))
  
  # initialize JVM/rJava
  .jpackage(pkgname, lib.loc) #, jars=jars) #, java.parameters=libpath)
  #.jcall("java/lang/System", returnSig='V', method="load", lib)
  # is this necessary?
  javaImport(packages = "java.lang")
  propertyString = .jnew("java/lang/String","java.library.path")
  libString = .jnew("java/lang/String", libs[1])
  .jcall("java/lang/System", returnSig='S', method="setProperty", propertyString, libString);
  .jaddLibrary("javaHeclib", paste0(libs[0], path.sep, "javaHeclib.", "libExt"))
  .jaddClassPath(jars)
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
    #try(, silent=TRUE)
    .jcall("hec/heclib/util/Heclib", returnSig='V', method="zset", 'MLEVEL', ' ', as.integer(messageLevel))
  }
}


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
#' @param configFileName filename of configurations to load, defaults to the one in this package.
#' @param defaultConfig name of default configuration to use; "none" will allow config file to specify a prefered config.
#' @param allowedStates only use configurations matching this state; defaults to "tested"
#' @param override_dss_location if set, forces using a particular location for dss jar files.
dssConfig = function(configFileName="./config/jar_config.json", 
                     defaultConfig="none",
                     allowedStates=c("tested"), 
                     override_dss_location=options()[["override_dss_location"]]){
  # libraries needed
  require(rjson)
  require(stringr)
  
  platform=R.Version()$platform
  # read jar config file and match first platform and allowed states with files that exist
  configfile = rjson::fromJSON(file=configFileName, simplify=TRUE)
  if(defaultConfig == "none" & ("default_config" %in% names(configfile))){
    defaultConfig = configfile$default_config
  }
  for(config in configfile$configs){
    # don't know this yet
    foundJars = FALSE
    foundConfig = FALSE
    
    # check files exist in dss_location
    if(is.null(override_dss_location)){
      dss_location = config$dss_location
    } else {
      dss_location = override_dss_location
    }
    # add path.sep if needed
    if(str_sub(dss_location, -1) != config$path.sep){
      dss_location = paste0(dss_location, config$path.sep)
    }
    jarList = paste0(dss_location, config$jars)
    libList = paste0(dss_location, config$libs)
    javaLocation = paste0(dss_location, config$JAVA_HOME)

    matchPlatform = config$platform == platform # use only if this is true
    foundJars = (all(file.exists(jarList)) & all(file.exists(libList))) # check if these exists
    isDefaultConfig = config$name == defaultConfig # use this if true
    isAllowedState = config$state %in% allowedStates # only use if allowed state
    if(matchPlatform & (foundJars | isDefaultConfig) & isAllowedState){
      foundConfig = TRUE
      break
    }
    # else go to the next one
    
  }
  # check that the found config will work?
  if(!foundConfig | length(jarList) == 0 | length(libList) == 0){
    stop("Could not find any config with matching jars and libraries.")
  }
  return(list(dss_location=dss_location, jars=jarList, libs=libList, java=javaLocation, config=config))
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
  EXCLUDE_FROM_METADATA = c("values", "times", "modified", "quality", "inotes")
  require(stringr)
  require(plyr)
  tscFieldsDF = get("tscFieldsDF", envir=hecJavaObjectsDB)
  metadata = dlply(tscFieldsDF, "SHORTNAME", function(df){
    #cat(sprintf("%s\t%s\t%s\n", df$FULLNAME, df$SHORTNAME, df$SIGNATURE))
    if(df$SHORTNAME %in% c("values", "times", "modified", "quality")) {
      return()
    }
    val = try(.jfield(tsc, name=df$SHORTNAME, sig=as.character(df$SIGNATURE)), silent=T)
    if(.jnull() == val){
      return(NA)
    }
    return(val)
  })
  metadata = metadata[!(names(metadata) %in% EXCLUDE_FROM_METADATA)]
  
  return(metadata)
}


