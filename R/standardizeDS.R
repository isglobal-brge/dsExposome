#' Title
#'
#' @param set 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
standardizeDS <- function(set, method = "normal"){
  std <- rexposome::standardize(set, method = method)
  return(std)
}