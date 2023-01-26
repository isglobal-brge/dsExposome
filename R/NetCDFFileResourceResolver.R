#' NetCDF file resource resolver
#'
#' Build a NetCDF resource client from a resource object describing access to a 
#' NetCDF file to be studied.
#'
#' @docType class
#' @format A R6 object of class NetCDFFileResourceResolver
#' @import resourcer
#' @export
NetCDFFileResourceResolver <- R6::R6Class(
  "NetCDFFileResourceResolver",
  inherit = ResourceResolver,
  public = list(
    isFor = function(x) {
      if (super$isFor(x)) {
        !is.null(findFileResourceGetter(x)) && toupper(x$format) %in% "NETCDF"
      } else {
        FALSE
      }
    },
    newClient = function(x) {
      if (self$isFor(x)) {
        NetCDFFileResourceClient$new(x)
      } else {
        NULL
      }
    }
  )
)
