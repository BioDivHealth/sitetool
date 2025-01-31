
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
createLCDataFrame <- function(in_df, r, dist, progress=F){

  ### Calculate landcover for each village based on radius ###
  df_list = lapply(seq(nrow(in_df)), function(i){

    # Crop land cover by dist radius from village
    area = getCroppedArea(in_df$geometry[[i]], dist)
    # tryCatch(!is.null(crop(r,extent(rect))), error=function(e) return(FALSE))

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

  df = df%>%
    subset(!is.na(layer))%>%
    dplyr::select(-c(layer))%>%
    dplyr::group_by(site_id)%>%
    dplyr::mutate(total_area = sum(area))%>%
    dplyr::group_by(site_id, cover)%>%
    dplyr::reframe(
      mean_patch_area =  mean(mean_patch_area),
      cover_total_area = sum(area),
      proportion = sum(area)/total_area)%>%
    dplyr::ungroup()%>%
    dplyr::distinct()%>%
    tidyr::pivot_longer(c(cover_total_area, mean_patch_area, proportion), names_to = 'measure', values_to = 'value')

  return(df)
}

createNDVIDataFrame <- function(in_df, raster, dist, progress=F){

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
