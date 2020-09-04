#' @title Normality test of exposures of ExposomeSet
#' 
#' @description Perform a normality test on all the exposures of an ExposomeSet object
#'
#' @param object \code{ExposomeSet} Exposome Set object
#' @param th \code{numeric} (default \code{0.05}) Threshold to considere an exposure to follow a normal distribution.
#' @param min.val \code{numeric} (default \code{5}) Minimum number of values not missings to test the exposures.
#' @param na.rm \code{bool} (default \code{TRUE}) Removes the NA values to test the normality on the exposure.
#' @param warnings \code{bool} (default \code{TRUE}) Show warnings if required.
#'
#' @return list of \code{data.frame} (one for each study server) with \cr
#' - exposure: \code{character} Name of the exposures \cr
#' - normality: \code{bool} If exposure is normal (\code{TRUE}) or not (\code{FALSE}) \cr
#' - p.value: \code{numeric} P-value of the Shapiro-Wilk Normality Test
#'

normalityTestDS <- function(object, th = 0.05, min.val = 5, na.rm = TRUE,
                            warnings = TRUE){
  norm_test <- rexposome::normalityTest(object, , th, min.val, na.rm, warnings)
  return(norm_test)
}