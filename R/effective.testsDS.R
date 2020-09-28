#' @title Obtain the threshold for effective tests of an ExposomeSet
#'
#' @param set \code{ExposomeSet} Exposome Set object
#'
#' @return \code{numeric} threshold for effective tests
#' @export
s
effective.testsDS <- function(set){
  cormat <- rexposome::extract(rexposome::correlation(set,
                                use="pairwise.complete.obs", method.cor = "pearson"))
  M <- ncol(cormat)
  lambdas <- base::eigen(cormat)$values
  Meff <- M - sum((lambdas>1)*(lambdas-1))
  alpha_corrected <- 1 - (1 - 0.05)^(1 / Meff)
  
  return(alpha_corrected)
}