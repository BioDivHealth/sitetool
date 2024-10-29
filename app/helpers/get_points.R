library(osmdata)
library(sf)

get_random_points <- function(bbox, n_points, road_dist, city_dist){
  
  ### Get OSM Data ### 
  # water bodies
  water_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "water")
  print('here')
  water_data <- osmdata_sf(water_query)$osm_multipolygons
  if (is.null(water_data)) {
    water_data <- st_sf(geometry = st_sfc())  # Create empty spatial object
  } else {
    water_data <- st_make_valid(water_data)
  }
  print('here2')

  # roads
  roads_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway",
                    value = c("motorway", "primary", "secondary", "tertiary", "residential"))
  road_data <- osmdata_sf(roads_query)$osm_lines
  
  if (is.null(road_data)) {
    road_data <- st_sf(geometry = st_sfc())  # Create empty spatial object
  }
  
  # city locations
  cities_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "place", 
                    value = c("city", "borough", "suburb", "quarter", "village", "town", "hamlet"))
  city_data <- osmdata_sf(cities_query)$osm_points%>%st_set_crs(4326)
  if (is.null(city_data)) {
    city_data <- st_sf(geometry = st_sfc())  # Create empty spatial object
  }
  print('here3')
  
  
  valid_points = vector('list', length=n_points)
  point = 1
  while (point < n_points+1) {
    
    random_point <- st_sfc(st_point(c(runif(1, bbox[1], bbox[3]), runif(1, bbox[2], bbox[4]))), crs=4326)
    print('here4')
    # check if intersect with any restricted areas
    intersect_road <- any(st_is_within_distance(random_point, road_data%>%st_set_crs(4326), dist=road_dist, sparse=FALSE))
    intersect_city <- any(st_is_within_distance(random_point, city_data%>%st_set_crs(4326), dist=city_dist, sparse=FALSE))
    #intersect_city <- any(st_intersects(random_point, road_data, sparse = FALSE))
    intersect_water <- any(st_intersects(random_point, water_data%>%st_set_crs(4326), sparse = FALSE))
    print('here5')
    
    if(!(intersect_road | intersect_city | intersect_water)){
      print(point)
      valid_points[[point]] = random_point
      point = point+1
    }
    
  }
  

  valid_points = st_as_sf(data.frame(site = as.character(1:n_points),
                                    site_id = paste0('random_', 1:n_points),
                                    input_site = FALSE,
                                    geometry=do.call(rbind, valid_points)),
                          crs = 4362)
  
  str(valid_points)
    
  return(valid_points)
}




# # area around bay city
#bbox = c(-85.104078, 41.145817, -84.695181, 41.324443)
# out = get_random_points(bbox=bbox, 100, 100, 1)
# 
# s = data.frame(
#   site = as.character(1:100),
#   site_id = paste0('random_', 1:100),
#   input_site = FALSE,
#   lat = runif(100, min = bbox[2], max = bbox[4]),
#   lon = runif(100, min = bbox[1], max = bbox[3])
# )%>%
# st_as_sf(coords=c('lon', 'lat'), crs=4326)
# points = do.call(rbind, out)
# points = st_as_sf(data.frame(geometry = points))
# 
# plot(points, add=T)
# plot(water_data, col = "blue", add=T)
# plot(st_geometry(roads_data), col = "red", add = TRUE)
# plot(st_geometry(cities_data), col = "green", add=T)
# 
# 
# bbox=c(-10, -10, 10, 10)
