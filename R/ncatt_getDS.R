#' Title
#'
#' @param nc 
#' @param varid 
#'
#' @return
#' @export
#'
#' @examples
ncatt_getDS <- function(nc, varid, attname){
  att <- ncdf4::ncatt_get(
    nc = nc,
    varid = varid,
    attname = attname
  )
  return(att)
}