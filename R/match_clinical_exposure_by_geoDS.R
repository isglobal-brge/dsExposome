#' Title
#'
#' @param nc 
#' @param varid 
#'
#' @return
#' @export
#'
#' @examples
get_exposure_from_geoDS <- function(lat, lon, exposure, exposure_name,
                                    clinical, clinical_lat_var,
                                    clinical_lon_var, clinical_id_var,
                                    exposures = NULL, exposures_id_var = NULL){
  lon_match <- MALDIquant::match.closest(
    clinical[[clinical_lon_var]],
    sort(lon, decreasing = F)
  )
  
  lat_match <- MALDIquant::match.closest(
    clinical[[clinical_lat_var]],
    sort(lat, decreasing = F)
  )
  
  lapply(1:length(lon_match), function(x){
    exposure[lon_match[x], lat_match[x]]
  }) %>% unlist() -> exposure_match
  
  new_exposure_matched <- cbind(
    clinical[,clinical_id_var,drop=F],
    exposure_match
  )
  colnames(new_exposure_matched)[2] <- exposure_name
  
  if(is.null(exposures)){
    exposures <- new_exposure_matched
  } else {
    exposures <- dplyr::left_join(
      x = exposures,
      y = new_exposure_matched,
      by.x = exposures_id_var,
      by.y = clinical_id_var
    )
  }
  return(exposures)
}