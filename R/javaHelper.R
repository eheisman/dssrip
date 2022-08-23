

whichJavaVersion <- function(){
  return(.jcall("java/lang/System", "S", "getProperty", "java.runtime.version"))
}


# internal function to convert java vector returned by catalog functions to strings
.javaVectorToStrings <- function(vect){
  vectList = .jevalArray(vect$toArray())
  sapply(vectList, .jcall, returnSig="S", "toString")
}