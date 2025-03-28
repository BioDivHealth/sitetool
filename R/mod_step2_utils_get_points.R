#' Get land area within a shape
#'
#' @param shape a `sf` object representing the spatial extent within which to
#' calculate the land area.
#'
#' @return an `sf` object representing the valid land area within the shape.
#'
#' @examples
#'   shape <- sf::st_as_sfc(sf::st_bbox(c(xmin = -120, xmax = -119, ymin = 35, ymax = 36), crs = 4326))
#'   result <- get_land_area(shape)
#'
#' @importFrom sf st_crop st_geometry st_make_valid st_difference
#' @export get_land_area
#'
get_land_area <- function(shape){
  crop_land = sf::st_intersection(sf::st_geometry(sf::st_make_valid(land_boundaries)), shape)
  crop_lakes = sf::st_crop(sf::st_geometry(sf::st_make_valid(lakes_boundaries)), shape)

  if (length(crop_lakes) == 0 & length(crop_land) == 0) {

    return(shape)
  }
  else if (length(crop_lakes) == 0 ){

    return(crop_land)
  }
  else
  {
    return(sf::st_make_valid(sf::st_difference(crop_land, crop_lakes)))
  }
}

#' Get OpenStreetMap roads data within a bounding box
#'
#' A function returning a shapefile of the roads within a bounding box. Uses
#' OpenStreetMap API (requires internet to access). Roads returned as line objects.
#' Only roads tagged "highway" are retrieved.
#'
#' @param bbox  a vector containing coordinates of the search area as c(xmin, ymin, xmax, ymax)
#' @param crs the four-digit code for the coordinate reference system of the data. Default is EPSG:4326
#'
#' @returns a `sf` containing the highways within a bounding box
#' #' @export get_roads

get_roads <- function(bbox, crs = 4326) {
  tryCatch({
    roads_query <- osmdata::opq(bbox = bbox,
                       timeout = 60) %>%
      osmdata::add_osm_feature(key = "highway",
                      value = c("motorway", "primary", "secondary", "tertiary", "residential"))
    roads <- osmdata::osmdata_sf(roads_query)$osm_lines

    if (is.null(roads)) {
      roads <- sf::st_sf(geometry = sf::st_sfc())
    } else {
      roads <- sf::st_set_crs(roads, crs)
    }
    }, error = function(e) {
      roads <- sf::st_sf(geometry = sf::st_sfc())
      message(e$message)
    })
}

#' Get OpenStreetMap city data within a bounding box
#'
#' A function returning a shapefile of the cities within a bounding box. Uses
#' OpenStreetMap API (requires internet to access). Any location tagged
#' "city", "borough", "suburb", "quarter", "village", "town", "hamlet" is
#' retrieved. Cities are returned as point coordinate objects.
#'
#' @param bbox  a vector containing coordinates of the search area as c(xmin, ymin, xmax, ymax)
#' @param crs the four-digit code for the coordinate reference system of the data. Default is EPSG:4326
#'
#' @returns a `sf` containing cities within the bounding box
#' #' @export get_cities

get_cities <- function(bbox, crs=4326){
  tryCatch({
    cities_query <- osmdata::opq(bbox = bbox,
                        timeout = 60) %>%
      osmdata::add_osm_feature(key = "place",
                      value = c("city", "borough", "suburb", "quarter", "village", "town", "hamlet"))
    cities <- osmdata::osmdata_sf(cities_query)$osm_points%>%
      sf::st_set_crs(crs)
  }, error = function(e){
    message(e$message)
    cities <- sf::st_sf(geometry = sf::st_sfc())
  })
}

check_distance <- function(points, shape, distance){
  exclude <- sf::st_is_within_distance(points, shape, distance)
  points[!sapply(exclude, any)]
}

#' @title Get random points within a polygon or bounding box
#'
#' @description Returns a shapefile of points within a bounding box or polygon.
#' Only includes points located on land, and filters points located close to roads or
#' cities based on the distances provided.
#'
#' @param area either a vector containing coordinates of the search area as c(xmin, ymin, xmax, ymax)
#' or a sf object containing a polygon representing the search area.
#' @param n_points int. number of points to find
#' @param road_dist int. distance to the nearest road in meters.
#' @param city_dist int. points must be futher from cities than this distance.
#' @param crs the four-digit code for the coordinate reference system of the data. Default is EPSG:4326
#' @param in_app if in shiny version, adds notifications
#'
#' @return a `sf` object containing lat, long coordinates for random points located
#' within the provided polygon and bounding box.
#' @examples
#'   bbox <- c(-85, 29, -82, 31)
#'   result <- get_random_points(area=bbox, n_points=10)
#'
#' @export get_random_points

get_random_points <- function(area, n_points, road_dist=0, city_dist=0, crs=4326, in_app=FALSE){

  # Create a progress object if in shiny
  if(in_app){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Finding random points", value = 0)
  }

  foundPoints = sf::st_sfc(crs = sf::st_crs(crs))

  bbox_sf = check_validity(area, crs)
  if(is.null(bbox_sf)){return(NULL)}

  bbox = sf::st_bbox(bbox_sf)
  if(road_dist > 0){
    if(in_app){(progress$inc(detail = 'Getting road data', 1/5))}
    roads = get_roads(bbox)
    if(is.null(roads)){
      road_dist = 0
      if(in_app){
        showNotification('Unable to download OpenStreetMap data, setting road distance to 0.', type='warning')
      }
    }
  }

  if(city_dist > 0){
    if(in_app){progress$inc(detail = 'Getting city data', 1/5)}
    cities = get_cities(bbox)
    if(is.null(cities)){
      city_dist = 0
      if(in_app){
        showNotification('Unable to download OpenStreetMap data, setting city distance to 0.', type='warning')
      }
    }
  }

  if(in_app){progress$inc(detail= 'Getting land area', 1/5)}

  land = get_land_area(bbox_sf)

  # Check that search area is valid
  if(!sf::st_is_valid(bbox_sf)){
    if(in_app){
      showNotification("Geometry invalid, please select a different area", type="error")
    }
    return(foundPoints)
  }

  if(in_app){progress$inc(detail= 'Sampling area', 1/5)}

  while(length(foundPoints) < n_points){
    # sample more than needed to speed up processing
    points = sf::st_sample(land, n_points*5)

    if(road_dist > 0){
      points = check_distance(points, roads, road_dist)
    }

    if(city_dist > 0){
      points = check_distance(points, cities, city_dist)
    }

    foundPoints = c(foundPoints, points)
  }

  if(in_app){progress$inc(1/4)}
  # if more than needed
  if(length(foundPoints) > n_points){
    foundPoints = foundPoints[sample(1:length(foundPoints), n_points)]
  }
  # turn into dataframe
  sites = data.frame(foundPoints)%>%
    sf::st_as_sf()%>%
    dplyr::mutate(site = 1:length(foundPoints),
                  site_id = paste0('random_', 1:length(foundPoints)),
                  input_site = FALSE)
  return(sites)
}

#' @title Get village points within a polygon  or bounding box
#'
#' @description Returns a shapefile with the latitude and longitude of cities located
#' within a bounding box or polygon. Uses the OpenStreetMap API to find anywhere
#' tagged "city", "suburb", "village", "town", "hamlet". If 'max_pop' is provided, only
#' cities with a population smaller than the provided value are returned.
#'
#' @param area either a vector containing coordinates of the search area as c(xmin, ymin, xmax, ymax)
#' or a sf object containing a polygon representing the search area.
#' @param crs the four-digit code for the coordinate reference system of the data. Default is EPSG:4326.
#' @param in_app if in shiny version, adds notifications
#'
#' @return a `sf` containing points of the cities in the area
#'
#' @examples
#'   bbox <- sf::st_bbox(c(xmin = -120, xmax = -119, ymin = 35, ymax = 36), crs = 4326)
#'   result <- get_village_points(area=bbox)
#'
#' @export get_village_points

get_village_points <- function(area, crs=4326, in_app=FALSE){
  if(in_app){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Obtaining village data", value = 0)
  }

  bbox_sf = check_validity(area, crs)
  if(is.null(bbox_sf)){return(NULL)}

  bbox = sf::st_bbox(bbox_sf)
  sites = get_cities(bbox)

  if(in_app){progress$inc(1/2)}

  # only keep points inside object
  sites =  sites[sf::st_within(sites, bbox_sf, sparse=F), ]

  if (!is.null(sites$name)) {
    sites <- sites %>%
      dplyr::select(osm_id, name, geometry) %>%
      dplyr::rename(site_id = osm_id, site = name) %>%
      dplyr::mutate(input_site = FALSE)%>%
      dplyr::filter(!is.na(site))

    return(sites)
  }
  else {
    if(in_app){showNotification('No sites found. Please select another location, site type, or check your internet connection.', type='warning')}
    return(NULL)
  }
}
