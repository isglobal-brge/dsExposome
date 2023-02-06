#' @title Get variable names of NetCDF object
#' 
#' @param nc An object of class ncdf4 (loaded from a NetCDF resource), 
#' indicating what file to get the variables from.
#'
#' @return `character vector` With the variable names
#' @export

netcdf_varsDS <- function(nc){
  
  vars <- names(nc$var)
  return(vars)
  
}