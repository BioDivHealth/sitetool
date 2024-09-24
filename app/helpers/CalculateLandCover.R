
# Landcover Raster Area Calculation

# packages
library(terra)
library(landscapemetrics)
library(svMisc)

# functions
getCroppedArea <- function(place, dist=1000){
  #  Takes in a raster (landcover), a point location (place), and a distance in meters 
  # and returns a cropped raster. The distance functions as a radius from the center point. 
  area <- place%>%
    vect()%>%
    buffer(width=dist)%>% # dist in meters (doc says any w lat/long proj)
    st_as_sf()
  
  return(area)
}

# Used to find the UTM zone based on longitude
long2utm <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

# in_df = st_read('data/villages.osm', layer="points", quiet=T)
# in_df = in_df%>%
#   select(c(osm_id, name, geometry))%>%
#   rename(site_id = osm_id, site = name)%>%
#   subset(site != 'NA')
# raster = rast('data/ESA_WorldCover_10m_2021_NVCrop.tif')
# dist=2000

createLCDataFrame <- function(in_df, raster, dist, progress=F){
  ### Define base output df ###
  df = data.frame(site=character(),
                  site_id = character(),
                  class=numeric(),
                  metric=character(),
                  value=numeric())
  
  ### Project into UTM ### 
  # Find Zone:
  # Data may be in multiple zones, but this using zone from the mean point of list of points
  utm_zone = long2utm(mean(as.numeric(st_coordinates(in_df$geometry)[,1])))
  
  proj <- paste0("+proj=utm +zone=",utm_zone," +datum=WGS84 +units=m")
  #raster <- project(raster, proj, method='near')
  in_df <- st_transform(in_df, proj)
  str(raster)
  str(in_df)
  
  ### Calculate landcover for each village based on radius ###
  for (i in seq(nrow(in_df))){
    # crop land cover by dist radius from village

    area = getCroppedArea(in_df$geometry[i], dist)
    raster_crop = crop(raster, area)
    class_tot = lsm_c_ca(raster_crop)
    class_area = lsm_c_area_mn(raster_crop)
    mn_patch = lsm_l_area_mn(raster_crop)
    sd_patch = lsm_l_area_sd(raster_crop)
    tot = do.call(rbind, list(class_tot,class_area,mn_patch,sd_patch))
    
    tot$site = in_df$site[i]
    tot$site_id = in_df$site_id[i]
    
    df = dplyr::bind_rows(df,tot)
    if(progress){incProgress(1/n, detail = paste("Site number:", i))}
  }
  
  ###  Fix Dataframe for output ###
  cats = read.csv('helpers/cats.csv')
  match_indices = match(df$class, cats$val)
  df$class[!is.na(match_indices)] = cats$cover[match_indices[!is.na(match_indices)]]
  df$class <- ifelse(df$level == 'landscape', df$level, df$class)
  df = df%>%select(-c(id, layer))
  
  ###  Summary Stats ###
  # get percent area for each landcover type
  frq <- df%>%
    subset(metric == 'ca')%>%
    group_by(site_id)%>%
    mutate(value = value/sum(value))%>%
    ungroup()
  
  frq$metric <- 'percent'
  frq$measure = paste0(frq$class, '_', frq$metric)
  
  # get patch area for each landscape type and village

  pa <- df%>%
    subset(level == 'class')%>%
    subset(metric == 'area_mn')%>%
    mutate(measure = paste0(class, '_patch_area'))

  # get class total area
  pta = df%>%
    subset(metric == 'ca')%>%
    group_by(site, site_id, class, metric)%>%
    summarize(value = sum(value))%>%
    ungroup()
  
  pta$measure = paste0(pta$class, '_', 'tot_area')
  
  
  # get total area for each village 
  ta <- df%>%
    subset(metric == 'ca')%>%
    group_by(site_id)%>%
    summarise(tot_area = sum(value))%>%
    mutate(measure = 'tot_area')%>%
    ungroup()
    
  # # separate out other metric then combine
  la <- df%>%
    subset(level == 'landscape')%>%
    mutate(measure = metric)

  df <- dplyr::bind_rows(frq, pa, la, pta)
   
  # # clean up dataframe, convert to wide
  df_sum <- df%>%
    select(-c(metric, class, level))%>% # remove grouping factors
    mutate(value = round(value, digits=2))%>%
    group_by(site, site_id)%>%
    pivot_wider(
      names_from = measure,
      values_from = value)%>%
    ungroup()

  #df_sum <- full_join(df_sum, ta)%>%select(-c(measure))
  df_sum = full_join(df_sum, in_df%>%st_drop_geometry())
  
  str(df_sum)
  return(df_sum)
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

