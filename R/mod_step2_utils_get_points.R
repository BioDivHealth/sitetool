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
  shape_geom <- sf::st_union(sf::st_geometry(sf::st_make_valid(shape)))

  crop_land = sf::st_intersection(sf::st_geometry(sf::st_make_valid(land_boundaries)), shape_geom)
  crop_lakes = sf::st_crop(sf::st_geometry(sf::st_make_valid(lakes_boundaries)), shape_geom)

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

retry_with_backoff <- function(expr_fun, max_attempts = 3, base_delay = 0.5, max_delay = 4) {
  last_error <- NULL

  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(expr_fun(), error = function(e) e)
    if (!inherits(result, "error")) {
      return(result)
    }

    last_error <- result
    if (attempt < max_attempts) {
      delay <- min(max_delay, base_delay * (2^(attempt - 1)))
      Sys.sleep(delay)
    }
  }

  stop(last_error$message)
}

#' Get OpenStreetMap roads data within a bounding box
#'
#' A function returning a shapefile of the roads within a bounding box. Uses
#' OpenStreetMap API (requires internet to access). Roads returned as line objects.
#' Only roads tagged "highway" are retrieved.
#'
#' @param bbox  a vector containing coordinates of the search area as c(xmin, ymin, xmax, ymax)
#' @param crs the four-digit code for the coordinate reference system of the data. Default is EPSG:4326
#' @param in_app if in shiny version, adds notifications
#'
#' @returns a `sf` containing the highways within a bounding box
#' #' @export get_roads

get_roads <- function(bbox, crs = 4326, in_app = FALSE) {
  tryCatch({
    roads <- retry_with_backoff(function() {
      roads_query <- osmdata::opq(bbox = bbox, timeout = 60) %>%
        osmdata::add_osm_feature(
          key = "highway",
          value = c("motorway", "primary", "secondary", "tertiary", "residential")
        )

      osmdata::osmdata_sf(roads_query)$osm_lines
    })

    # If no roads returned, return empty sf
    if (is.null(roads) || nrow(roads) == 0) {
      roads <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(crs)))
      if (in_app) {
        shiny::showNotification("No roads found in this bounding box.", type = "warning")
      }
    } else {
      roads <- sf::st_set_crs(roads, crs)
    }

    return(roads)

  }, error = function(e) {
    if (in_app) {
      shiny::showNotification(paste("Error fetching roads. Please select a smaller area."), type = "error")
    } else {
      message("Error fetching roads: ", e$message)
    }
    # Return empty sf to prevent app crash
    return(sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(crs))))
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
#' @param in_app if in shiny version, adds notifications
#'
#' @returns a `sf` containing cities within the bounding box
#' #' @export get_cities

get_cities <- function(bbox, crs = 4326, in_app = FALSE) {
  tryCatch({
    cities <- retry_with_backoff(function() {
      cities_query <- osmdata::opq(bbox = bbox, timeout = 60) %>%
        osmdata::add_osm_feature(
          key = "place",
          value = c("city", "borough", "suburb", "quarter", "village", "town", "hamlet")
        )

      osmdata::osmdata_sf(cities_query)$osm_points
    })

    # If no points returned, return empty sf
    if (is.null(cities) || nrow(cities) == 0) {
      cities <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(crs)))
      if (in_app) {
        shiny::showNotification("No cities found in this bounding box.", type = "warning")
      }
    } else {
      cities <- sf::st_set_crs(cities, crs)
    }

    return(cities)

  }, error = function(e) {
    # Show notification in Shiny if desired
    if (in_app) {
      shiny::showNotification(paste("Error fetching cities. Please select a smaller area."), type = "error")
    } else {
      message("Error fetching cities: ", e$message)
    }
    # Return empty sf so app doesn't break
    return(sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(crs))))
  })
}

check_distance <- function(points, shape, distance){
  exclude <- sf::st_is_within_distance(points, shape, distance)
  points[!sapply(exclude, any)]
}


# In-memory cache for OSM queries (per R session)
.osm_query_cache <- new.env(parent = emptyenv())
OSM_QUERY_CACHE_TTL_SEC <- 15 * 60
OSM_QUERY_CACHE_MAX_ENTRIES <- 8

normalize_bbox_key <- function(bbox, digits = 4) {
  bbox_vals <- if (!is.null(names(bbox)) && all(c("xmin", "ymin", "xmax", "ymax") %in% names(bbox))) {
    bbox[c("xmin", "ymin", "xmax", "ymax")]
  } else {
    bbox[seq_len(4)]
  }

  bbox_vals <- as.numeric(bbox_vals)
  bbox_vals <- round(bbox_vals, digits = digits)

  paste(formatC(bbox_vals, format = "f", digits = digits), collapse = "|")
}

make_osm_cache_key <- function(type, bbox) {
  paste(type, normalize_bbox_key(bbox), sep = "::")
}

prune_osm_cache <- function(max_entries = OSM_QUERY_CACHE_MAX_ENTRIES) {
  keys <- ls(envir = .osm_query_cache, all.names = TRUE)
  if (length(keys) <= max_entries) {
    return(invisible(NULL))
  }

  access_time <- vapply(keys, function(k) {
    entry <- .osm_query_cache[[k]]
    if (!is.null(entry$last_access)) {
      as.numeric(entry$last_access)
    } else {
      0
    }
  }, numeric(1))

  n_drop <- length(keys) - max_entries
  keys_to_drop <- names(sort(access_time))[seq_len(n_drop)]
  rm(list = keys_to_drop, envir = .osm_query_cache)

  invisible(NULL)
}

get_osm_cache <- function(key, ttl_sec = OSM_QUERY_CACHE_TTL_SEC) {
  entry <- .osm_query_cache[[key]]
  if (is.null(entry)) {
    return(NULL)
  }

  if (is.null(entry$data) || is.null(entry$cached_at)) {
    .osm_query_cache[[key]] <- NULL
    return(NULL)
  }

  age_sec <- as.numeric(difftime(Sys.time(), entry$cached_at, units = "secs"))
  if (!is.finite(age_sec) || age_sec > ttl_sec) {
    .osm_query_cache[[key]] <- NULL
    return(NULL)
  }

  entry$last_access <- Sys.time()
  .osm_query_cache[[key]] <- entry

  entry$data
}

set_osm_cache <- function(key, data, max_entries = OSM_QUERY_CACHE_MAX_ENTRIES) {
  .osm_query_cache[[key]] <- list(
    data = data,
    cached_at = Sys.time(),
    last_access = Sys.time()
  )

  prune_osm_cache(max_entries = max_entries)
  invisible(NULL)
}

# Automatically choose a UTM CRS based on centroid
utm_crs <- function(sf_obj) {
  lon <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(sf_obj)))[, 1]
  zone <- floor((lon + 180) / 6) + 1
  epsg <- ifelse(sf::st_is_longlat(sf_obj),
                 32600 + zone,  # WGS 84 / UTM Northern Hemisphere
                 sf::st_crs(sf_obj)$epsg)
  sf::st_crs(epsg)
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
#' @param min_dist int. minimum distance between sampled points
#' @param road_dist int. distance to the nearest road in meters.
#' @param city_dist int. points must be further from cities than this distance, in meters.
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

get_random_points <- function(area, n_points, min_dist = 0, road_dist = 0, city_dist = 0, crs = 4326, in_app = FALSE) {
  if (n_points < 1) {
    return(sf::st_sf(
      data.frame(
        site = integer(),
        site_id = character(),
        input_site = logical(),
        geometry = sf::st_sfc(crs = crs)
      )
    ))
  }

  if (in_app) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Finding random points", value = 0)
  }

  # Validate area
  bbox_sf <- check_validity(area, crs)
  if (is.null(bbox_sf)) return(NULL)
  if (min_dist > 0) {
    # Project area to a metric CRS (if not already projected)
    area_m <- sf::st_transform(bbox_sf, 3857)
    total_area <- as.numeric(sf::st_area(area_m))

    point_area <- pi * (min_dist^2) / 0.9069
    max_points <- floor(total_area / point_area)

    if (n_points > max_points) {
      if(in_app){
        showNotification(paste0("Cannot fit ", n_points, " points with distance of ", min_dist,
                                " m between points in area. Max possible ~ ", max_points, "."), type='error')
        return(NULL)
      }
      else{
        stop(paste0("Cannot fit ", n_points, " points with min_dist = ", min_dist,
                    " m in area. Max possible ~ ", max_points, "."))
      }

    }
  }


  if (in_app) progress$inc(detail = "Preparing data", amount = 0.1)

  target_crs <- utm_crs(bbox_sf)
  bbox <- sf::st_bbox(bbox_sf)

  land <- get_land_area(bbox_sf)
  land_proj <- sf::st_transform(land, target_crs)

  if (road_dist > 0) {
    if (in_app) progress$inc(detail = 'Getting road data', 0.1)
    roads_key <- make_osm_cache_key("roads", bbox)
    roads <- get_osm_cache(roads_key)

    if (is.null(roads)) {
      roads <- get_roads(bbox, in_app = in_app)
      if (!is.null(roads) && nrow(roads) > 0) {
        set_osm_cache(roads_key, roads)
      }
    }

    if (is.null(roads) || nrow(roads) == 0) {
      road_dist <- 0
    } else {
      roads <- sf::st_transform(roads, target_crs)
    }
  }

  if (city_dist > 0) {
    if (in_app) progress$inc(detail = 'Getting city data', 0.1)
    cities_key <- make_osm_cache_key("cities", bbox)
    cities <- get_osm_cache(cities_key)

    if (is.null(cities)) {
      cities <- get_cities(bbox, in_app = in_app)
      if (!is.null(cities) && nrow(cities) > 0) {
        set_osm_cache(cities_key, cities)
      }
    }

    if (is.null(cities) || nrow(cities) == 0) {
      city_dist <- 0
    } else {
      cities <- sf::st_transform(cities, target_crs)
    }
  }

  foundPoints <- sf::st_sfc(crs = target_crs)
  loop_start <- Sys.time()
  loop_iteration <- 0
  stalled_iterations <- 0

  max_iterations <- min(5000, max(200, n_points * 30))
  max_seconds <- min(120, max(45, n_points * 0.6))
  max_stalled_iterations <- 30

  if (in_app) progress$inc(detail = "Sampling area", amount = 0.1)

  while (length(foundPoints) < n_points) {
    loop_iteration <- loop_iteration + 1
    elapsed_sec <- as.numeric(difftime(Sys.time(), loop_start, units = "secs"))

    if (loop_iteration > max_iterations || elapsed_sec > max_seconds || stalled_iterations >= max_stalled_iterations) {
      break
    }

    points_before <- length(foundPoints)
    sample_n <- max((n_points - points_before) * 5, 25)
    new_points <- sf::st_sample(land_proj, sample_n)

    if (road_dist > 0) {
      new_points <- check_distance(new_points, roads, road_dist)
    }

    if (city_dist > 0) {
      new_points <- check_distance(new_points, cities, city_dist)
    }

    # Filter too close to existing found points
    if (length(foundPoints) > 0 && min_dist > 0) {
      dists <- sf::st_distance(new_points, foundPoints)
      too_close <- apply(dists, 1, function(x) any(x < min_dist))
      new_points <- new_points[!too_close]
    }

    # Filter new points too close to each other
    if (length(new_points) > 1 && min_dist > 0) {
      keep <- rep(TRUE, length(new_points))
      min_dist_m <- units::set_units(min_dist, "m")
      zero_dist <- units::set_units(0, "m")

      for (i in seq_along(new_points)) {
        if (!keep[i]) next
        d <- sf::st_distance(new_points[i], new_points[keep])
        keep[which(keep)[d < min_dist_m & d != zero_dist]] <- FALSE
      }
      new_points <- new_points[keep]
    }

    foundPoints <- c(foundPoints, new_points)

    if (length(foundPoints) == points_before) {
      stalled_iterations <- stalled_iterations + 1
    } else {
      stalled_iterations <- 0
    }
  }

  if (length(foundPoints) < n_points) {
    elapsed <- round(as.numeric(difftime(Sys.time(), loop_start, units = "secs")), 1)
    msg <- paste0(
      "Could only generate ", length(foundPoints), " of ", n_points,
      " points after ", loop_iteration, " iterations (", elapsed, " seconds). ",
      "Try reducing minimum distance thresholds or requesting fewer points."
    )

    if (in_app) {
      shiny::showNotification(msg, type = "error")
      return(NULL)
    }

    stop(msg)
  }

  if (length(foundPoints) > n_points) {
    foundPoints <- foundPoints[sample(seq_along(foundPoints), n_points)]
  }

  foundPoints <- sf::st_transform(foundPoints, crs)  # Return to original CRS

  if (in_app) progress$inc(0.3)

  sites <- sf::st_sf(
    site = seq_along(foundPoints),
    site_id = paste0("random_", seq_along(foundPoints)),
    input_site = FALSE,
    geometry = foundPoints
  )

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

get_village_points <- function(area, crs = 4326, in_app = FALSE) {
  if (in_app) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Obtaining populated place data", value = 0)
  }

  bbox_sf <- check_validity(area, crs)
  if (is.null(bbox_sf)) return(NULL)

  bbox <- sf::st_bbox(bbox_sf)

  # Safe call to get_cities
  sites <- tryCatch({
    get_cities(bbox, crs = crs, in_app=in_app)
  }, error = function(e) {
    if (in_app) {
      shiny::showNotification(
        paste("Error retrieving populated place points:", e$message),
        type = "error"
      )
    } else {
      message("Error retrieving populated place points: ", e$message)
    }
    sf::st_sf(site_id = integer(0),
              site = character(0),
              input_site = logical(0),
              geometry = sf::st_sfc())  # empty sf with correct columns
  })

  if (in_app) progress$inc(0.5)

  # Only keep points inside area
  if (!is.null(sites) && nrow(sites) > 0) {
    intersect_mat <- sf::st_intersects(sites, bbox_sf, sparse = FALSE)
    intersect_any <- apply(intersect_mat, 1, any)
    sites <- sites[intersect_any, ]
  }

  # Ensure consistent columns even if empty
  if (nrow(sites) == 0 || !"name" %in% colnames(sites)) {
    if (in_app) {
      shiny::showNotification(
        "No sites found. Please select another location, site type, or check your internet connection.",
        type = "warning"
      )
    }
    sites <- sf::st_sf(
      site_id = integer(0),
      site = character(0),
      input_site = logical(0),
      geometry = sf::st_sfc()
    )
  } else {
    sites <- sites %>%
      dplyr::select(osm_id, name, geometry) %>%
      dplyr::rename(site_id = osm_id, site = name) %>%
      dplyr::mutate(input_site = FALSE) %>%
      dplyr::filter(!is.na(site))
  }

  return(sites)
}
