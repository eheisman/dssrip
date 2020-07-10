#########
# pairedDataContainers.R
# functions to turn PairedDataContainer objects into data.frames
#########


#' getColumnsByName Get a column from a PairedDataContainer object.
#' 
#' Gets a column from a paired data container by name.
#' 
#' Name of column must be exact or NA is returned.
#'
#' @return vector of values from column.
#' @note NOTE
#' @author Evan Heisman
#' @export 
getColumnsByName <- function(file, pdc, column){
  if(class(file)=="character"){
    file = opendss(file)
  }
  if(class(pdc)=="character"){
    pdc = file$get(pdc)
  }
  if(class(column) != "character"){
    return(pdc$yOrdinates[column,])
  } else {
    if(!(column %in% pdc$labels)){
      warning(sprintf("No column named [%s] found in paired data container.", column))
      return(NA)
    }
    return(pdc$yOrdinates[which(pdc$labels == column),])
  }
}

#' columnsToDataFrame
#' @description 
#' @param file string of filename or HecDss object from `opendss`
#' @param pdc string of pathname or PairedDataContainer object
#' @param colnameFunction function that takes pdc object and metadata list to generate column names for the 'yOrdinates' from the PDC, default takes format of 'yParameter.yLabel'.
#' @export
getPairedDataAsDataFrame <- function(file, pdc, colnameFunction=joinParameterToLabel){
  pdc = getPDC(file, pdc)
  md = getMetadata(pdc)
  out = list()
  out[[md$xparameter]] = pdc$xOrdinates
  nCurves = md$numberCurves
  # set y column names
  yColNames = colnameFunction(pdc, md)
  # pull data
  for(i in seq(nCurves)){
    out[[yColNames[i]]] = pdc$yOrdinates[i,]
  }
  # merge into dataframe
  out = do.call(cbind, out)
  attr(out, "metadata") = md
  return(out)
}

#' @export
joinParameterToLabel <- function(pdc, md){
  labels = as.character(seq(md$numberCurves))
  if(md$labelsUsed){
    labels = md$labels
  }
  return(paste(pdc$yparameter,labels,sep="."))
}

getPDC <- function(file, pdc){
  if(class(file)=="character"){
    file = opendss(file)
  }
  if(class(pdc)=="character"){
    pdc = file$get(pdc)
  }
  return(pdc)
}
