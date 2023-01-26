#' Title
#'
#' @param nc 
#' @param varid 
#'
#' @return
#' @export
#'
#' @examples
NetCDF_fillvalue_matrixDS <- function(mat, fillvalue){
  mat[mat == fillvalue$value] <- NA
  return(mat)
}