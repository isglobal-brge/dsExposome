#' @title Scale a data frame
#' 
#'
#' @param x \code{data frame} Table to be scaled
#' @param center Corresponds to the \code{center} parameter of the base function \code{scale}
#' @param scale Corresponds to the \code{scale} parameter of the base function \code{scale}
#'
#' @return \code{data frame} Scaled table
#' @export

scaleDS <- function(x, center = TRUE, scale = TRUE){
  data <- base::scale(x, center, scale)
  return(data)
}