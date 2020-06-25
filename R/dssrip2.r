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
  
  jars = paste0(dss_location, path.sep, "jar", path.sep, c("hec", "heclib", "rma", "hecData", "hec-dssvue-v3.0", "lookup", "help\\dssvueHelp"), ".jar")
  #require(rJava)
  
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
    require(rJava)
    require(stringr)
    require(xts)
    libdir = paste0(dss_location, "lib", path.sep)
    #dyn.load(lib)z
    ## Add javaHeclib.dll to loaded libraries.
    #.jcall("java/lang/System", returnSig='V', method="load", lib)
    Sys.setenv(PATH=paste0(Sys.getenv("PATH"), ";", dss_location, ";", libdir))
    lib = paste0(libdir, "javaHeclib.dll")
    #libpath = paste0("-Djava.library.path=",libdir)
    .jpackage(pkgname, lib.loc) #, jars=jars) #, java.parameters=libpath)
    #.jcall("java/lang/System", returnSig='V', method="load", lib)
    javaImport(packages = "java.lang")
    propertyString = .jnew("java/lang/String","java.library.path")
    libString = .jnew("java/lang/String",libdir)
    .jcall("java/lang/System", returnSig='S', method="setProperty", propertyString, libString);
    #.jcall("java/lang/System", returnSig='V', method="loadLibrary", "javaHeclib")
    .jaddLibrary("javaHeclib", lib)
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
    try(.jcall("hec/heclib/util/Heclib", returnSig='V', method="zset", 'MLEVEL', ' ', as.integer(messageLevel)), silent=TRUE)
    }
  }


loadConfig = function(configFile, platform, allowedStates=c("tested"), dss_jar_location=NULL){
  require(rjson)
  # read jar config file and match first platform and allowed states with files that exist
  configfile = rjson::fromJSON(file="./config/jar_config.json", unexpected.escape="keep", simplify=TRUE)
  defaultConfig = "none"
  if(!("default_config" %in% names(configs))){
    defaultConfig = configs$default_config
  }
  for(config in configfile$configs){
    if(config$name == defaultConfig){
      # found the specified default
      break
    }
    # else check platform and files exist
    vers = R.Version()
    dss_location = config$dss_location
    # add path.sep if needed
    if(stringr::str_sub(dss_location, -1) != config$path.sep){
      dss_location = paste0(dss_location, config$path.sep)
    }
    
    jarList = paste0(dss_location, config$jars)
    libList = paste0(dss_location, config$libs)
    jre_location = paste0(dss_location, config$JAVA_HOME)
    if(all(file.exists(jarList)) & all(file.exists(libList))){
      # found all jars and files
      foundJars = TRUE
      break
    }
    
  }
  if(!foundJars | length(jarList) == 0 | length(libList) == 0){
    errorCondition("Could not find any config with matching jars and libraries.")
  }
  return(list(jars=jarList, libs=libList, JAVA_HOME=jre_location))
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


