#' @title Shapiro-Wilk Normality Test
#' 
#' @escription Perform the Shapiro-Wilk Normality Test on a numeric vector (or column of a data frame)
#'
#' @param x \code{numeric} Name of the numeric vector 
#'
#' @return Results of the Shapiro-Wilk Normality Test
#'
 
shapiro.testDS <- function(x){
  test <- stats::shapiro.test(x)
  return(test)
}