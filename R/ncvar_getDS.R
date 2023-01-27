#' @title Read data from netCDF file
#' 
#' @description Reads data from an existing netCDF file.
#'
#' @param nc An object of class ncdf4 (loaded from a NetCDF resource), 
#' indicating what file to read from.
#' @param varid What variable to read the data from.
#'
#' @return `vector`
#' @export
#'
ncvar_getDS <- function(nc, varid){
  var <- ncdf4::ncvar_get(
    nc = nc,
    varid = varid
  )
  return(var)
}