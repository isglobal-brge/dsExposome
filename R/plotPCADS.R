#' Title
#'
#' @param pc1_vec 
#' @param pc2_vec 
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
plotPCADS <- function(..., dataset){
  
  cols <- 2
  vector_matrix <- matrix(c(...), ncol = cols)
  # NOT SURE BOUT COMPLETE CASES!
  dataset <- dataset[complete.cases(dataset), ]
  dataset_matrix <- data.matrix(dataset, rownames.force = FALSE)
  # return(t(dataset_matrix))
  transformed_data <- t(vector_matrix) %*% t(dataset_matrix) # obtain matrix. nrow = individuals, ncol = dim (2 PC's)
  
  transformed_data <- data.frame(t(transformed_data))
  colnames(transformed_data) <- c("pc1", "pc2")
  
  return(transformed_data)
  
  avere <- dsBase::scatterPlotDS(transformed_data[1, ], transformed_data[2, ], 1, 4, 20)
  
  return(avere)
  
  # AGAFAR AQUESTA TRANSFORMED DATA I FER UN SCATTER PLOT AMB LA FUNCIO DE DS!
  # INTENTAR DE AFEGIR AL TRANSFORMED DATA UN LINK AMB ELS FENOTIPS INDIVIDUALS PER AL FUTUR PODER FER
  # UN SCATTER PLOT AMB GROUPING
  # RETORNAR LA INFO DEL SCATTERPLOT I PRINTEJAR AL CLIENT EL PLOT EN SI
  # UN COP ACABAT LO DEL PLOT, MIRAR EL TEMA DEL INPUT datase, EL ds.exposome_pca HAURIE DE TREURE UNA LLISTA
  # AMB EL NOM DE LA TAULA SOBRE LA QUE HA FET EL PCA, DAQUESTA FORMA LO DEL PLOT ES FACILITE
  
  return(transformed_data)
  
}