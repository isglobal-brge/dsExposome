.onAttach <- function(libname, pkgname) {
  resourcer::registerResourceResolver(NetCDFFileResourceResolver$new())
}

.onDetach <- function(libpath) {
  resourcer::unregisterResourceResolver("NetCDFFileResourceResolver")
}