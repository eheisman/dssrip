## DSS - R interface project
## Evan Heisman

.onLoad <- function(libname, pkgname){
  ## initialize DSSVue Link
  ## sets to the default location - change if installed elsewhere
  ## assumes a 64-bit Windows system
  ## should work with Solaris / Linux version if correct path is set, and path
  ## separators are changed below
  if("package:rJava" %in% search()){
    warning("package rJava already loaded")
  }
  if(!exists("dss_location")){
    dss_location = ""
    if(Sys.info()[["sysname"]] == "Windows"){
      dss_location = paste0(Sys.getenv("ProgramFiles(x86)"),"\\HEC\\HEC-DSSVue\\")
    }
    warning(paste0("variable 'dss_location' was undefined.  Trying default in '", dss_location, "'."))
  }
  
  ## Setup Java
  #Sys.setenv(JAVA_HOME=paste0(dss_location, "jre\\bin\\"))
  jars = paste0(dss_location, "jar\\", c("hec", "heclib", "rma", "hecData"), ".jar")
  lib = paste0(dss_location,"\\lib\\javaHeclib.dll")
  dyn.load(lib)
  .jpackage(pkgname, morePaths=jars)

  #jars = paste0(dss_location, "jar\\", c("hec", "heclib", "rma", "hecData"), ".jar")
  #libs = paste0("-Djava.library.path=", dss_location, "\\lib\\")
  #.jinit(classpath=jars, parameters=libs)  
}