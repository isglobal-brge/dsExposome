#' @title Associate `NetCDF` data and individual location
#' 
#' @description Merge the exposure data contained in a `NetCDF` object
#' with the location data of the individuals present on a `clinical` `data.frame`
#'
#' @param lat `array` Latitude extracted from a `NetCDF` resource using `ncvar_getDS`
#' @param lon `array` Longitutde extracted from a `NetCDF` resource using `ncvar_getDS`
#' @param exposure `matrix` Exposure extracted from a `NetCDF` resource using `ncvar_getDS`
#' @param exposure_name `character` Name to assign to the `exposure`
#' @param clinical `data.frame` Clinical data data of the individuals, which contain among
#' other variables, the longitude and latitude of the individuals to be associated with the 
#' `NetCDF` data.
#' @param clinical_lat_var `character` Name of the latitude variable on the `clinical` data
#' @param clinical_lon_var `character` Name of the longitude variable on the `clinical` data
#' @param clinical_id_var `character` Name of the IDs variable on the `clinical` data
#' @param exposures (default `NULL`) If provided, existing `data.frame` with exposure data.
#' The new exposure calculated from the `NetCDF` data will be added to this table by ID
#' @param exposures_id_var (default `NULL`) Required if `exposures` is provided. Name of the
#' IDs variable on the `exposures` data.
#'
#' @return `data.frame`
#' @export
#'
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
  # TODO: Throw warning or error if there are 0 matches!
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