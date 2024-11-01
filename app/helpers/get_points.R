library(osmdata)
library(sf)

bbox = c(xmin=-86.595333, ymin=44.375409, xmax=-85.35171, ymax=44.84486)
# ontario
bbox = c(xmin=-87.564125, ymin=46.612344, xmax=-81.477613, ymax=49.644599)
bbox = c(xmin=-128.933916, ymin=38.862466, xmax=-120.113611, ymax=44.164863)
bbox = c(-85.104078, 41.145817, -84.695181, 41.324443)

# brazil
bbox = c(xmin=-53.109884, ymin=-1.978104, xmax=-46.63825, ymax=3.275872)

# # bbox = c(xmin = -85.104078, ymin = 41.145817, xmax = -84.695181, ymax = 41.324443)


# Shapefiles for cropping and getting points
land_boundaries = st_read('ss-analyzer/app/data/ne_50m_land/ne_50m_land.shp')
lake_boundaries = st_read('ss-analyzer/app/data/ne_50m_lakes/ne_50m_lakes.shp')

get_land_area <- function(bbox){

  crop_land = st_crop(st_geometry(st_make_valid(land_boundaries)), bbox_sf) 
  crop_lakes = st_crop(st_geometry(st_make_valid(lake_boundaries)), bbox_sf) 
  
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

get_roads <- function(bbox){
  # roads
  roads_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway",
                    value = c("motorway", "primary", "secondary", "tertiary", "residential"))
  roads <- osmdata_sf(roads_query)$osm_lines%>%st_set_crs(4326)
  
  if (is.null(roads)) {
    road_data <- st_sf(geometry = st_sfc())  # Create empty spatial object
  }
  
  roads
}

get_cities <- function(bbox){
  # city locations
  cities_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "place", 
                    value = c("city", "borough", "suburb", "quarter", "village", "town", "hamlet"))
  cities <- osmdata_sf(cities_query)$osm_points%>%st_set_crs(4326)
  if (is.null(ciies)) {
    city_data <- st_sf(geometry = st_sfc())  # Create empty spatial object
  }
  cities
}

check_distance <- function(points, sf, distance){
  exclude <- st_is_within_distance(points, sf, distance)
  points[!sapply(exclude, any), ]
}


get_random_points <- function(bbox, n_points, roads, distance, crs=4326){
  foundPoints = st_sfc(crs = st_crs(crs))
  
  #roads = get_roads(bbox)
  
  bbox_sf = st_as_sfc(st_bbox(bbox, crs=st_crs(crs)))
  land = get_land_area(bbox_sf)
  
  while(length(foundPoints) < n_points){
    # sample more than needed to speed up processing
    points = st_sample(land, n_points*5) 
    points = check_distance(points, roads, 50)
    foundPoints = c(foundPoints, points)
  }
  
  # if more than needed
  if(length(foundPoints) > n_points){
    foundPoints = foundPoints[sample(1:length(foundPoints), n_points)]
  }
}

get_random_points2 <- function(bbox, n_points, roads, distance, crs=4326){
  foundPoints = st_sfc(crs = st_crs(crs))
  
  #roads = get_roads(bbox)
  
  bbox_sf = st_as_sfc(st_bbox(bbox, crs=st_crs(crs)))
  land = get_land_area(bbox_sf)
  
  while(length(foundPoints) < n_points){
    # sample more than needed to speed up processing
    points = st_sample(land, n_points) 
    points = check_distance(points, roads, 50)
    foundPoints = c(foundPoints, points)
  }
  
  # if more than needed
  if(length(foundPoints) > n_points){
    foundPoints = foundPoints[sample(1:length(foundPoints), n_points)]
  }
}


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
 

get_random_points3 <- function(bbox, n_points, road, road_dist){
  

  
  valid_points = vector('list', length=n_points)
  point = 1
  while (point < n_points+1) {
    
    random_point <- st_sfc(st_point(c(runif(1, bbox[1], bbox[3]), runif(1, bbox[2], bbox[4]))), crs=4326)

    # check if intersect with any restricted areas
    intersect_road <- any(st_is_within_distance(random_point, road%>%st_set_crs(4326), dist=road_dist, sparse=FALSE))
 #   intersect_city <- any(st_is_within_distance(random_point, city_data%>%st_set_crs(4326), dist=city_dist, sparse=FALSE))
    #intersect_city <- any(st_intersects(random_point, road_data, sparse = FALSE))
  #  intersect_water <- any(st_intersects(random_point, water_data%>%st_set_crs(4326), sparse = FALSE))

    
    if(!(intersect_road)){
      valid_points[[point]] = random_point
      point = point+1
    }
    
  }
  

  valid_points = st_as_sf(data.frame(site = as.character(1:n_points),
                                    site_id = paste0('random_', 1:n_points),
                                    input_site = FALSE,
                                    geometry=do.call(rbind, valid_points)),
                          crs = 4326)
  
    
  return(valid_points)
}

library(microbenchmark)

result <- microbenchmark(
  get_random_points(bbox, 10, roads, 50),
  get_random_points2(bbox, 10, roads, 50),
  get_random_points3(bbox, 10, roads, 50)
)




