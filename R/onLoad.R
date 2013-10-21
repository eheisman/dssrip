## onLoad.R
## Calls initialize.dssrip(as.package=T) to set up JVM, and add required DLLs.

.onLoad <- function(libname, pkgname){
  initialize.dssrip(pkgname=NULL, lib.loc=libname)
  #initialize.dssrip(pkgname=pkgname, lib.loc=libname) ## Option 2 for ignoring previous JVM. DOES NOT WORK
}