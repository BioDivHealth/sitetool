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
    print('also here')
    return(bbox)  
  }
  else if (length(crop_lakes) == 0 ){
    print('here')
    return(crop_land)
  }
  else
  {
    return(st_make_valid(st_difference(crop_land, crop_lakes)))
  }
}

get_roads <- function(bbox, crs=4326){
  print('back on the road again')
  roads_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway",
                    value = c("motorway", "primary", "secondary", "tertiary", "residential"))
  roads <- osmdata_sf(roads_query)$osm_lines%>%st_set_crs(crs)

  if (is.null(roads)) {
    roads<- st_sf(geometry = st_sfc())  # Create empty spatial object
  }

  roads
}

get_cities <- function(bbox, crs=4326){
  cities_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "place", 
                    value = c("city", "borough", "suburb", "quarter", "village", "town", "hamlet"))
  cities <- osmdata_sf(cities_query)$osm_points%>%st_set_crs(crs)
  if (is.null(ciies)) {
    cities <- st_sf(geometry = st_sfc())  # Create empty spatial object
  }
  cities
}

check_distance <- function(points, sf, distance){
  exclude <- st_is_within_distance(points, sf, distance)
  points[!sapply(exclude, any), ]
}


get_random_points <- function(bbox, n_points, distance, crs=4326){
  foundPoints = st_sfc(crs = st_crs(crs))
  
  roads = get_roads(bbox)
  
  bbox_sf = st_as_sfc(st_bbox(c(xmin=bbox[1], ymin=bbox[2], xmax=bbox[3], ymax=bbox[4]), crs=st_crs(crs)))
  print('getting land')
  land = get_land_area(bbox_sf)
  print('finding points')
  while(length(foundPoints) < n_points){
    # sample more than needed to speed up processing
    points = st_sample(land, n_points*5) 
    points = check_distance(points, roads, 50)
    foundPoints = c(foundPoints, points)
  }

  # if more than needed
  if(length(foundPoints) > n_points){
    print('here2')
    foundPoints = foundPoints[sample(1:length(foundPoints), n_points)]
  }
  foundPoints
}

 bbox = c(xmin=-86.595333, ymin=44.375409, xmax=-85.35171, ymax=44.84486)
# # # ontario
# # bbox = c(xmin=-87.564125, ymin=46.612344, xmax=-81.477613, ymax=49.644599)
# # bbox = c(xmin=-128.933916, ymin=38.862466, xmax=-120.113611, ymax=44.164863)
# # bbox = c(-85.104078, 41.145817, -84.695181, 41.324443)
# # 
# # # brazil
# # bbox = c(xmin=-53.109884, ymin=-1.978104, xmax=-46.63825, ymax=3.275872)
# # 
# # # # bbox = c(xmin = -85.104078, ymin = 41.145817, xmax = -84.695181, ymax = 41.324443)
# # 
# # 
# # # Shapefiles for cropping and getting points
# bbox = c(-7.411394, 37.852734, -6.04438999999999, 38.795762)
# land_boundaries = st_read('ss-analyzer/app/data/ne_50m_land/ne_50m_land.shp')
# lake_boundaries = st_read('ss-analyzer/app/data/ne_50m_lakes/ne_50m_lakes.shp')
# 
# # 
# points = get_random_points(bbox, 10, 10)
# p = data.frame(points)%>%
#   st_as_sf()%>%
#   dplyr::mutate(site = 1:length(points),
#          site_id = paste0('random_', 1:length(points)),
#          input_site = FALSE)
# 

#  # spain
# 
