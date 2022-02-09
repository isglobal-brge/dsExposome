#' @title Exposure Phenotype names
#' 
#' @description Extract the phenotypes names from a Expression Set object
#'
#' @param exposomeSet \code{ExposomeSet} ExposomeSet object
#'
#' @return \code{character vector} of the phenotypes names
#' @export

phenotypeNamesDS <- function(exposomeSet){
  data <- rexposome::phenotypeNames(exposomeSet)
  return(data)
}