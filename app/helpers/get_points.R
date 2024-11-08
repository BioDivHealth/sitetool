library(osmdata)
library(sf)

# Finds the UTM zone based on longitude
long2crs <- function(long, lat) {
  zone = (floor((long + 180)/6) %% 60) + 1
  
  if(lat > 0){
    paste0('EPSG:',32600 + zone)
  }
  else{
    paste0('EPSG:', 32700 + zone)
  }
}

get_land_area <- function(bbox){

  crop_land = st_crop(st_geometry(st_make_valid(land_boundaries)), bbox) 
  crop_lakes = st_crop(st_geometry(st_make_valid(lake_boundaries)), bbox) 
  
  if (length(crop_lakes) == 0 & length(crop_land) == 0) {

    return(bbox)  
  }
  else if (length(crop_lakes) == 0 ){

    return(crop_land)
  }
  else
  {
    return(st_make_valid(st_difference(crop_land, crop_lakes)))
  }
}

get_roads <- function(bbox, crs = 4326) {

  roads <- st_sf(geometry = st_sfc())
  tryCatch({
    roads_query <- opq(bbox = bbox,
                       timeout = 60) %>%
      add_osm_feature(key = "highway",
                      value = c("motorway", "primary", "secondary", "tertiary", "residential"))
    
    roads <- osmdata_sf(roads_query)$osm_lines %>% st_set_crs(crs)
    
    # Check if the result is NULL and return an empty spatial object if so
    if (is.null(roads)) {
      message("No roads data returned; returning empty spatial object.")
      roads <- st_sf(geometry = st_sfc())  # Create empty spatial object
    }
    
  }, error = function(e) {
    message("Overpass query timed out when retrieving road data.")
    
  })
  return(roads)
}

get_cities <- function(bbox, crs=4326){
  tryCatch({
      cities_query <- opq(bbox = bbox,
                          timeout = 1000) %>%
        add_osm_feature(key = "place", 
                        value = c("city", "borough", "suburb", "quarter", "village", "town", "hamlet"))
      
      cities <- osmdata_sf(cities_query)$osm_points%>%st_set_crs(crs)
      
      if (is.null(ciies)) {
        cities <- st_sf(geometry = st_sfc())  
      }
    }, error = function(e){
      message("Overpass query timed out when retrieving city data.")
    })

  return(cities)
}

check_distance <- function(points, sf, distance){
  exclude <- st_is_within_distance(points, sf, distance)
  points[!sapply(exclude, any), ]
}


get_random_points <- function(bbox, n_points, road_dist=0, city_dist=0, crs=4326){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Finding random points", value = 0)
  
  foundPoints = st_sfc(crs = st_crs(crs))
  
  if(road_dist > 0){
    progress$inc(detail = 'Getting road data', 1/5)
    roads = get_roads(bbox)
  }
  
  if(city_dist > 0){
    progress$inc(detail = 'Getting city data', 1/5)
    cities = get_cities(bbox)
  }
    
  progress$inc(detail= 'Getting land area', 1/5)
  bbox_sf = st_as_sfc(st_bbox(c(xmin=bbox[1], ymin=bbox[2], xmax=bbox[3], ymax=bbox[4]), crs=st_crs(crs)))
  land = get_land_area(bbox_sf)
  
  progress$inc(detail= 'Sampling area', 1/5)
  while(length(foundPoints) < n_points){
    # sample more than needed to speed up processing
    points = st_sample(land, n_points*5) 
    
    if(road_dist > 0){
        points = check_distance(points, roads, road_dist)
    }
    
    if(city_dist > 0){
      points = check_distance(points, cities, city_dist)
    }
    
    foundPoints = c(foundPoints, points)
  }
  progress$inc(1/4)
  # if more than needed
  if(length(foundPoints) > n_points){
    foundPoints = foundPoints[sample(1:length(foundPoints), n_points)]
  }
  foundPoints
}

get_village_points <- function(bbox, max_pop=1e8, crs=4326){
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Obtaining village data", value = 0)
  sites = opq(bbox = bbox,
              timeout = 200)%>%
    add_osm_feature(key = 'place', 
                    value = c("city", "suburb", "village", "town", "hamlet"))%>%
    osmdata_sf()
  
  progress$inc(1/2)
  bbox_sf = st_as_sfc(st_bbox(c(xmin=bbox[1], ymin=bbox[2], xmax=bbox[3], ymax=bbox[4]), crs=st_crs(crs)))
  
  if (!is.null(sites$osm_points)) {
    sites <- sites$osm_points %>%
      st_set_crs(crs)%>%
      mutate(population = as.numeric(population))%>%
      filter(is.na(population) | population < max_pop)%>%
      select(osm_id, name, geometry) %>%
      rename(site_id = osm_id, site = name) %>%
      mutate(input_site = FALSE)%>%
      st_filter(bbox_sf)%>%
      filter(!is.na(site))
      
    
  } else {
    showNotification('No sites found. Please select another location or site type.')
    sites = NULL
  }
  
}
