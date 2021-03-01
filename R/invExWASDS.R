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
invExWASDS <- function(object, formula, tef){
  inv_exwas <- rexposome::invExWAS(object = object, formula = formula, tef = tef)
  return(inv_exwas)
}