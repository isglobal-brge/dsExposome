#' Title
#'
#' @param x 
#' @param rid 
#'
#' @return
#' @export
#'
#' @examples
extractExposomeSetFromImputedSetDS <- function(x, rid){
  
  return(rexposome::toES(x, rid))
  
}