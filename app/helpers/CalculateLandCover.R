# Landcover Raster Calculations

### Required Packages ### 
library(terra)
library(tidyr)

# Helper Fucntions ------------------------------------------------------------

# Creates a buffer around a point location
getCroppedArea <- function(place, dist=1000){
  place%>%
    vect()%>%
    buffer(width=dist)%>% 
    st_as_sf()
}

# Finds the UTM zone based on longitude
long2utm <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

# Check if raster is in UTM
# To-do: error handling of no crs
isUTM <- function(raster) {
  crs_info <- crs(raster)
  if (grepl("+proj=utm", crs_info)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

calculatePatchMetrics <- function(raster_crop, cell_areas) {
  # Convert raster to patches (connected areas of the same class)
  s = segregate(raster_crop, keep=TRUE, other=NA)
  p = patches(s, directions = 8) 
  
  patch_areas = lapply(p, function(x){
    z = zonal(cell_areas, x, sum)
    data.frame(layer = names(x),
               mean_patch_area = mean(z$area, na.rm=T)
              # sd_patch_area = sd(z$area, na.rm=T)
              )
  })
    
  patch_areas = do.call(rbind, patch_areas)
  patch_areas = patch_areas%>%subset(layer != 'patches')
}


# Main Function: Create landcover dataframe ------------------------------------
createLCDataFrame <- function(in_df, raster, dist, progress=F){

  ### Calculate landcover for each village based on radius ###
  df_list = lapply(seq(nrow(in_df)), function(i){

    # Crop land cover by dist radius from village
    area = getCroppedArea(in_df$geometry[i], dist)
    raster_crop = crop(raster, area)
    
    # Get cell areas
    cell_areas = cellSize(raster_crop)
    
    # Get total by layer
    class_tot = zonal(cell_areas, raster_crop, sum, na.rm=TRUE)
    
    # Get patch area by layer
    class_area = calculatePatchMetrics(raster_crop, cell_areas)
    
    # When only one landcover type
    if(nrow(class_area) == 0){
      class_area = data.frame(layer = NA, mean_patch_area = NA)
    }
    
    # Combine into one dataframe
    d_temp = cbind(class_tot, class_area)
    d_temp$site = in_df$site[i]
    d_temp$site_id = in_df$site_id[i]
    d_temp$input_site = in_df$input_site[i]
    if(progress){incProgress(1/nrow(in_df), detail = paste("Site number:", i))}
    return(d_temp)
  })

  df = do.call(rbind, df_list)

  
  df = df%>%
    subset(!is.na(layer))%>%
    select(-c(layer))%>%
    group_by(site_id)%>%
    mutate(total_area = sum(area))%>%
    group_by(site_id, cover)%>%
    summarise(
           mean_patch_area =  mean(mean_patch_area),
           cover_total_area = sum(area),
           proportion = sum(area)/total_area)%>%
    ungroup()%>%
    unique()%>%
    pivot_longer(c(cover_total_area, mean_patch_area, proportion), names_to = 'measure', values_to = 'value')

  return(df)
}

createNDVIDataFrame <- function(in_df, raster, type, dist, progress=F){
  df_list = lapply(seq(nrow(in_df)), function(i){

    area = getCroppedArea(in_df$geometry[i], dist)
    raster_crop = crop(raster, area)
    
    d_temp = global(raster_crop, c('mean', 'sd', 'range'), na.rm=T)
    d_temp$site = in_df$site[i]
    d_temp$site_id = in_df$site_id[i]
    d_temp$input_site = in_df$input_site[i]
    return(d_temp)
    if(progress){incProgress(1/nrow(in_df), detail = paste("Site number:", i))}
  })

  df = do.call(rbind, df_list)
  df$measure = type
  df = df%>%
    pivot_longer(c(mean, sd, min, max), names_to = 'cover', values_to = 'value')
  
  return(df)
}


createBuildDF <- function(in_df, buildings, dist, progress=F){
  num_build = list(nrow(in_df))
  build_area = list(nrow(in_df))

  for (i in seq(nrow(in_df))){
    # crop land cover by dist radius from village
    area = getCroppedArea(in_df$geometry[i], dist)
    build_crop = st_crop(buildings, area)
    
    num_build[i] = nrow(build_crop)
    build_area[i] = sum(build_crop$area_in_meters)
    if(progress){progress(i,nrow(in_df))}
  }

  in_df$num_buildings = num_build
  in_df$build_area = build_area
  
  return(in_df)
  }


