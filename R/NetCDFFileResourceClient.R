#' NetCDF file resource client
#'
#' Build a NetCDF handler from a resource object describing access to a 
#' NetCDF file to be studied.
#'
#' @docType class
#' @format A R6 object of class NetCDFFileResourceClient
#' @import resourcer
#' @export
NetCDFFileResourceClient <- R6::R6Class(
  "NetCDFFileResourceClient",
  inherit = FileResourceClient,
  public = list(
    initialize = function(resource) {
      super$initialize(resource)
    },
    getConnection = function(...) {
      conn <- super$getConnection()
      path <- super$downloadFile()
      resource <- super$getResource()
      private$loadncdf4()
      conn <- ncdf4::nc_open(normalizePath(path))
      super$setConnection(conn)
      conn
    },
    getValue = function(...) {
      self$getConnection(...)
    },
    close = function() {
      super$close()
      conn <- super$getConnection()
      if (!is.null(conn)) {
        ncdf4::nc_close(conn)
      }
    }
  ),
  private = list(
    loadncdf4 = function() {
      if (!require("ncdf4")) {
        install.packages("ncdf4", repos="http://R-Forge.R-project.org", dependencies = TRUE)
      }
    }
  )
)