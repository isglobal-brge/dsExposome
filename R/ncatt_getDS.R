#' @title Get attribute from netCDF file
#' 
#' @description Reads an attribute from a netCDF file.
#'
#' @param nc An object of class ncdf4 (loaded from a NetCDF resource), 
#' indicating what file to read from.
#' @param varid The variable whose attribute is to be read.
#' @param attname Name of the attribute to read
#'
#' @return `list`
#' @export
#'
ncatt_getDS <- function(nc, varid, attname){
  att <- ncdf4::ncatt_get(
    nc = nc,
    varid = varid,
    attname = attname
  )
  return(att)
}