#' @title Exposures and phenotyphes data
#' 
#' @description Extract a table with the exposures and phenotypes data for all the individuals of an
#' Exposome Set
#'
#' @param exposomeSet  \code{ExposomeSet} Exposome Set object
#' @param target \code{character} To specify target of output table, \code{all} to include exposures and phenotypes,
#' \code{exposures} to include only the exposures and \code{phenotypes} to include only the phenotypes. Default \code{all}
#'
#' @return \code{data.frame} With exposures and phenotypes
#' @import rexposome, Biobase
#' @export

exposures_pData <- function(exposomeSet, target = "all") {

    # amb ds.cbind no funcione ¿????¿?¿
  if(target == "all"){
    data <- cbind(rexposome::expos(exposomeSet), Biobase::pData(exposomeSet))
  }
  else if(target == "exposures"){
    data <- rexposome::expos(exposomeSet)
  }
  else if(target == "phenotypes"){
    data <- Biobase::pData(exposomeSet)
  }
  
  else{stop("Incorrect 'target' argument. Options: 'all', 'exposures' and 'phenotypes'", call.=FALSE)}
  
  return(data)

}
