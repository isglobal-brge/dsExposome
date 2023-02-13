#' @title Apply a transformation to the exposure of an ExposomeSet
#'
#' @param Set `ExposomeSet` which exposures will be transformed
#' @param exposure `character` Name of the exposure to be transformed
#' @param method 	Function to be applied to the exposure
#'
#' @return `ExposomeSet` with a transformed exposure
#' @export
#'

transformDS <- function(Set, exposure, method){
  if(as.character(substitute(method)) %in% c("exp", "sqrt", "log")){
    Set <- rexposome::trans(
      object = Set, 
      fun = method,
      select = exposure
    )
  } else {
    stop(paste0("Selected transformation method [", 
                as.character(substitute(method)), 
                "] is unavailable. Use 'exp', 'sqrt' or 'log'."))
  }
  return(Set)
}