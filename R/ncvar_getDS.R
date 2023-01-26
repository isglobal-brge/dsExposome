#' Title
#'
#' @param nc 
#' @param varid 
#'
#' @return
#' @export
#'
#' @examples
ncvar_getDS <- function(nc, varid){
  var <- ncdf4::ncvar_get(
    nc = nc,
    varid = varid
  )
  return(var)
}