#' Title
#'
#' @param object \code{ExposomeSet} Exposome object
#' @param phenotype \code{character} Phenotype objective
#' @param tef \code{bool} If \code{TRUE} computes the threshold for effective tests.
#' @param ... \code{character vector} Adjusting phenotype covariables
#'
#' @return \code{ExWAS} object. With tef, coefficients and pvalues of associations
#' @export

invExWASDS <- function(object, phenotype, tef, ...){
  covariables <- unlist(list(...))
  formula <- paste0("~ ", phenotype, if(is.null(covariables)){}else{paste(" +",paste0(covariables, collapse = " + "))})
  formula <- as.formula(formula)
  inv_exwas <- rexposome::invExWAS(object = object, formula = formula, tef = tef)
  return(inv_exwas)
}