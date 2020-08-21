exposures_pData <- function(exposomeSet) {

    # amb ds.cbind no funcione ¿????¿?¿
  data <- cbind(rexposome::expos(exposomeSet), Biobase::pData(exposomeSet))
  return(data)

}
