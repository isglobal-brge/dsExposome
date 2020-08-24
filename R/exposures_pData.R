#' @title Exposures and phenotyphes data
#' 
#' @description Extract a table with the exposures and phenotypes data for all the individuals of an
#' Exposome Set
#'
#' @param exposomeSet  \code{ExposomeSet} Exposome Set object
#'
#' @return \code{data.frame} With exposures and phenotypes
#' @import rexposome, Biobase
#'

exposures_pData <- function(exposomeSet) {

    # amb ds.cbind no funcione ¿????¿?¿
  data <- cbind(rexposome::expos(exposomeSet), Biobase::pData(exposomeSet))
  return(data)

}
