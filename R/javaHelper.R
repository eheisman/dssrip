# javaHelper.R
# functions to help with rJava

#' whichJavaVersion
#' 
#' @return java version string 
#' @author Evan Heisman
#' @export 
whichJavaVersion <- function(){
  return(.jcall("java/lang/System", "S", "getProperty", "java.runtime.version"))
}
