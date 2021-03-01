#' Title
#'
#' @param object 
#' @param formula 
#' @param tef 
#'
#' @return
#' @export
#'
#' @examples
invExWASDS <- function(object, phenotype, tef, ...){
  covariables <- unlist(list(...))
  formula <- paste0("~ ", phenotype, if(missing(covariables)){}else{paste(" +",paste0(covariables, collapse = " + "))})
  formula <- as.formula(formula)
  inv_exwas <- rexposome::invExWAS(object = object, formula = formula, tef = tef)
  return(inv_exwas)
}