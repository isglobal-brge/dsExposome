#' @title Exposure Phenotypes
#' 
#' @description Extract the phenotypes names from a Expression Set object
#'
#' @param exposomeSet \code{ExposomeSet} Exposome Set object
#'
#' @return \code{character vector} of the phenotypes names
#' @import rexposome
#' @export

phenotypeNamesDS <- function(exposomeSet){
  data <- rexposome::phenotypeNames(exposomeSet)
  return(data)
}