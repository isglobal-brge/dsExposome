#' @title Standardize ExposomeSet
#'
#' @param set \code{ExposomeSet} Exposome Set object
#' @param method \code{character} Method of standarization. 
#' Options are \code("normal") which scales the exposures using the mean as the center 
#' and the standard variation as dispersion, \code{"robust"} which uses the median and median absolute deviation respectively
#' and \code{"interquartile range"} which uses the median as the center and the coeficient between the 
#' interquartile range of the exposure and the normal range between the percentile 75 and 25 as variance.
#'
#' @return Returns a standardized ExposomeSet
#' @export

standardizeDS <- function(set, method = "normal"){
  std <- rexposome::standardize(set, method = method)
  return(std)
}