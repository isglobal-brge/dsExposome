#' @title Get table of missings per variable
#' 
#' @description Obtain the number (or percentage) of missings of for the exposures or phenotypes of an ExposomeSet object
#'
#' @param exp \code{ExposomeSet} Exposome Set object
#' @param set \code{character} (default \code{"exposures"}) Set to get the missings: \code{"exposures"} or \code{"phenotypes"}
#' @param output \code{character} (default \code{"n"}) Get missing number (\code{"n"}) or percentage (\code{"p"})
#'
#' @return
#' Vector of named numerics, where the name corresponds to the variable and the associated numeric corresponds
#' to the missing (number or percentage, dependeing on the \code{output} parameter)

tableMissingsDS <- function(exp, set = "exposures", output = "n"){
  data <- rexposome::tableMissings(exp, set, output)
  return(data)
}