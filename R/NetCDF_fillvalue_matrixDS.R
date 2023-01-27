#' @title Set NAs to matrix fillvalues
#' 
#' @description Use a fillvalue extracted using `ncatt_get` and place NAs
#'
#' @param mat `matrix` Extracted from a `NetCDF` resource using `nc_dataDS`
#' @param fillvalue `list` Extracted from the `_FillValue` slot of a `NetCDF`
#' resource using `ncatt_getDS`
#'
#' @return `matrix`
#' @export
#'
NetCDF_fillvalue_matrixDS <- function(mat, fillvalue){
  mat[mat == fillvalue$value] <- NA
  return(mat)
}