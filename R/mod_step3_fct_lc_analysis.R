
# Helper Functions ------------------------------------------------------------

# Creates a buffer around a point location
getCroppedArea <- function(place, dist=1000){
  place%>%
    terra::vect()%>%
    terra::buffer(width=dist)%>%
    sf::st_as_sf()%>%
    sf::st_set_crs(4326)
}

calculatePatchMetrics <- function(raster_crop, cell_areas) {
  # Convert raster to patches (connected areas of the same class)
  s = terra::segregate(raster_crop, keep=TRUE, other=NA)
  p = terra::patches(s, directions = 8)

  patch_areas = lapply(p, function(x){
    z = terra::zonal(cell_areas, x, sum)
    data.frame(layer = names(x),
               mean_patch_area = mean(z$area, na.rm=T)
    )
  })

  patch_areas = do.call(rbind, patch_areas)
  patch_areas = patch_areas%>%subset(layer != 'patches')
}


# Main Function: Create landcover dataframe ------------------------------------
#' Analyze categorical land cover surrounding a list of sites
#'
#' This function calculates land cover statistics for each site based on a given radius, using a categorical raster.
#'
#' @param in_df A data frame containing site information, including a `geometry` column with spatial point or polygon data.
#' @param r A categorical raster (`SpatRaster`) representing land cover classifications.
#' @param dist Numeric. The radius (in map units) around each site to extract land cover information.
#' @param progress Logical. If `TRUE`, updates a progress bar during processing.
#'
#' @return A data frame summarizing land cover metrics for each site, including total area, mean patch area, and proportion of land cover types.
#'
#' @details
#' The function processes each site by:
#' 1. Cropping the land cover raster within the specified radius.
#' 2. Calculating total cell area for each land cover type.
#' 3. Computing patch metrics such as mean patch area.
#' 4. Aggregating and restructuring the results into a long-format data frame.
#'
#' @export
createLCDataFrame <- function(in_df, r, dist, progress=F){

  ### Calculate landcover for each village based on radius ###
  df_list = lapply(seq(nrow(in_df)), function(i){

    # Crop land cover by dist radius from village
    area = getCroppedArea(in_df$geometry[i], dist)

    if(terra::relate(terra::ext(area), terra::ext(r), relation = 'within')){
      raster_crop = terra::crop(r, area)
      # Get cell areas
      cell_areas = terra::cellSize(raster_crop)
      # Get total by layer
      class_tot = terra::zonal(cell_areas, raster_crop, sum, na.rm=TRUE)

      # Get patch area by layer
      class_area = calculatePatchMetrics(raster_crop, cell_areas)

      # When only one landcover type
      if(nrow(class_area) == 0){
        class_area = data.frame(layer = NA, mean_patch_area = NA)
      }

      # Combine into one dataframe
      d_temp = cbind(class_tot, class_area)
      d_temp$site_id = in_df$site_id[i]
      d_temp$input_site = in_df$input_site[i]
      if(progress){incProgress(1/nrow(in_df), detail = paste("Site number:", i))}
      return(d_temp)
    }
  })
  df = do.call(rbind, df_list)

  if ("layer" %in% colnames(df)) {
    df = df %>%
      subset(!is.na(layer)) %>%
      dplyr::select(-c(layer)) %>%
      dplyr::group_by(site_id) %>%
      dplyr::mutate(total_area = sum(area)) %>%
      dplyr::group_by(site_id, cover) %>%
      dplyr::reframe(
        mean_patch_area = mean(mean_patch_area),
        cover_total_area = sum(area),
        proportion = sum(area) / total_area
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(c(cover_total_area, mean_patch_area, proportion), names_to = 'measure', values_to = 'value')
  } else {
    df = data.frame(site_id = integer(0), cover = character(0), measure = character(0), value = numeric(0))
  }

  return(df)
}


#' Analyze a continuous raster surrounding a list of sites
#'
#' This function returns statistics for an input raster for each site based on a given radius.
#'
#' @param in_df A data frame containing site information, including a `geometry` column with spatial point or polygon data.
#' @param raster A continuous raster (`SpatRaster`) representing NDVI or other continuous environmental data.
#' @param dist Numeric. The radius (in map units) around each site to extract raster statistics.
#' @param progress Logical. If `TRUE`, updates a progress bar during processing.
#'
#' @return A data frame summarizing raster-wide statistics (mean, standard deviation, min, max) for each site.
#'
#' @details
#' The function processes each site by:
#' 1. Cropping the raster within the specified radius.
#' 2. Calculating summary statistics: mean, standard deviation, range (min/max).
#' 3. Restructuring the results into a long-format data frame.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' # Example continuous raster (NDVI-like data)
#' r <- rast(ncol = 10, nrow = 10, vals = runif(100, -1, 1)) # Simulated NDVI values
#'
#' # Example site data with points
#' in_df <- data.frame(
#'   site = c("village1", "village2"),
#'   site_id = c(1, 2),
#'   input_site = c(TRUE, FALSE)
#' )
#' in_df$geometry <- st_sfc(st_point(c(1,1)), st_point(c(6,6)))
#' in_df <- st_as_sf(in_df)
#'
#' # Run function
#' createNDVIDataFrame(in_df, r, dist = 500, progress = TRUE)
#' }
#'
#' @export
createContDataFrame <- function(in_df, raster, dist, progress=F){

  df_list = lapply(seq(nrow(in_df)), function(i){

    area = getCroppedArea(in_df$geometry[i], dist)
    raster_crop = terra::crop(raster, area)

    d_temp = terra::global(raster_crop, c('mean', 'sd', 'range'), na.rm=T)
    d_temp$site = in_df$site[i]
    d_temp$site_id = in_df$site_id[i]
    d_temp$input_site = in_df$input_site[i]
    return(d_temp)
    if(progress){incProgress(1/nrow(in_df), detail = paste("Site number:", i))}
  })

  df = do.call(rbind, df_list)

  df$cover = 'Raster-wide'
  df = df%>%
    tidyr::pivot_longer(c(mean, sd, min, max), names_to = 'measure', values_to = 'value')

  return(df)
}
