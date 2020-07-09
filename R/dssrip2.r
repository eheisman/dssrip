#######
# dssrip2.r
# functions for connecting to the DSS libraries through Java
#######

#' @title dssrip initialization
#' @author Evan Heisman
#' @description
#' Starts a rJava JVM with configuration for dss's jar and dll files.
#' 
#' @details
#' This method is typically called by the onLoad function when the package is imported.
#' List of `options` can be passed to this package via the R global `options()` function:
#' - `dss_jvm_parameters` string passed to JVM when initialized, allowing memory settings to be altered or other JVM start-up options set.  This is untested.
#' - `dss_override_location` file path to force dssrip to load a particular set of dss libraries.  This is untested. 
#' - `dss_config_filename` filename that points to a jar_config.json file external to the package's default configuration.
#' - `dss_allowed_states` list, defaults to `c('tested')`, filters config file to only allowed states.
#' - `dss_default_config`, string, forces to only use config going by this `name`.
#' @param quietDSS - if true, don't show 'Z' messages during opening, reading, and writing to a file.
#' @param parameters list of options string to pass to JVM. (defaults to the option `dss_jvm_parameters` or NULL)
#' @param setJavaLoc - override Java location with one in config file. (untested)
#' @param verbose - set to true for debuggering
#' @seealso loadConfig
#' @export
#' @return JVM initialization status - 0 if successful, positive for partial initialization, negative for failure.  See ?.jinit 
initialize.dssrip = function(pkgname=NULL, quietDSS=TRUE, parameters=options()[["dss_jvm_parameters"]], setJavaLoc=FALSE, verbose=TRUE, ...){
  ## parameters examples: '-Xmx2g -Xms1g' to set up memory requirements for JVM to 2g heap and 1g stack.

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
  # Still want to do this in .Rprofile... ugh.
  #if(setJavaLoc){
    #Sys.setenv(JAVA_HOME=java)
    #print(paste("JAVA_HOME is ", Sys.getenv("JAVA_HOME")))
  #}
  if(verbose) packageStartupMessage(sprintf("JRE location is %s\n", Sys.getenv("JAVA_HOME")))
  
  # don't do this until after setting JAVA_HOME
  require(rJava)

  # is this necessary?
  #Sys.setenv(PATH=paste0(Sys.getenv("PATH"), ";", dss_location, ";", libdir))
  
  # initialize JVM/rJava
  # Don't use the jars and nativeLibrary path as the DSS libraries are external to this package
  .jpackage(pkgname, lib.loC) #, jars=jars) #, java.parameters=libpath)

  # is this necessary? appears so
  javaImport(packages = "java.lang") 
  
  # is this necessary? appears so
  propertyString = .jnew("java/lang/String","java.library.path")
  libString = .jnew("java/lang/String", libs[1])
  .jcall("java/lang/System", returnSig='S', method="setProperty", propertyString, libString);
  
  # proper way to do these from rJava 0.9-12
  javaHeclibPath =  paste0(libs[1], path.sep, "javaHeclib.", lib.ext)
  .jaddLibrary("javaHeclib", javaHeclibPath)
  .jaddClassPath(jars)
  
  if(quietDSS){
    # See heclib programmers manual for this trick.
    messageLevel = 2 # only print errors
    #try(, silent=TRUE)
    .jcall("hec/heclib/util/Heclib", returnSig='V', method="zset", 'MLEVEL', ' ', as.integer(messageLevel))
  }
}


#' @title dssrip read dss configuration file
#' @author Evan Heisman
#' @description
#' Reads dss configuration file from .json internal to package or externally selected by user.  Useful for managing multiple dss versions for linking to dssrip.
#' 
#' @details
#' Reads the jar_config.json file to find DSS libraries for use by dssrip.
#' @param configFileName defaults to package jar_config.json, otherwise can point to set of libraries from `options()`'s `dss_config_filename`
#' @param defaultConfig name of configuration to use if you know _exactly_ which one you want, defaults to option `dss_default_config`
#' @param allowedStates only allow loading of configs in these states, defaults to `tested` or `dss_allowed_states` option
#' @param dssOverrideLocation only allow dss libraries from this directory, will try all the configs at this location. can be set by option `dss_override_location`
#' @export
dssConfig = function(configFileName=options()[["dss_config_filename"]], 
                     defaultConfig=options()[["dss_default_config"]],
                     allowedStates=options()[["dss_allowed_states"]], 
                     dssOverrideLocation=options()[["dss_override_location"]]){
  # libraries needed
  require(rjson)
  require(stringr)
  
  # populate default values
  # dss_override_location handled later
  if(is.null(configFileName)){
    configFileName = "./config/jar_config.json"
  }
  
  if(is.null(defaultConfig)){
    defaultConfig = "none"
  }
  
  if(is.null(allowedStates)){
    allowedStates =   c("tested")
  }
  
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
    if(is.null(dssOverrideLocation)){
      dss_location = config$dss_location
    } else {
      dss_location = dssOverrideLocation
    }
    # add path.sep if needed
    if(str_sub(dss_location, -1) != config$path.sep){
      dss_location = paste0(dss_location, config$path.sep)
    }
    jarList = paste0(dss_location, config$jars)
    libList = paste0(dss_location, config$libs)
    javaLocation = paste0(dss_location, config$JAVA_HOME)

    # check if this config is can be used
    matchPlatform = config$platform == platform # use only if this is true
    foundJars = (all(file.exists(jarList)) & all(file.exists(libList))) # check if these exists
    isDefaultConfig = config$name == defaultConfig # use this if true
    isAllowedState = config$state %in% allowedStates # only use if allowed state

    # debug block
    # checks = c(matchPlatform=matchPlatform, foundJars=foundJars, isDefaultConfig=isDefaultConfig, isAllowedState=isAllowedState)
    # system = c(matchPlatform=platform, foundJars="", isDefaultConfig=defaultConfig, isAllowedState="")
    # setting = c(matchPlatform=config$platform, foundJars="", isDefaultConfig=config$name, isAllowedState=config$state)
    # checksdf = data.frame(SYSTEM=system, SETTING=setting, CHECKS=checks)
    # print(checksdf)
    
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


