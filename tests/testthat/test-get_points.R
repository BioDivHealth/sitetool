
make_mock_cities <- function(lon, lat, name, crs = 4326) {
  sf::st_as_sf(
    data.frame(
      osm_id = seq_along(name),
      name = name,
      longitude = lon,
      latitude = lat,
      stringsAsFactors = FALSE
    ),
    coords = c("longitude", "latitude"),
    crs = crs
  )
}


make_mock_roads <- function(crs = 4326) {
  road_geom <- sf::st_sfc(
    sf::st_linestring(matrix(c(-90, 20, -89, 21), ncol = 2, byrow = TRUE)),
    crs = crs
  )

  sf::st_sf(highway = "primary", geometry = road_geom)
}


clear_osm_cache <- function() {
  cache_env <- get(".osm_query_cache", envir = asNamespace("sitetool"))
  keys <- ls(envir = cache_env, all.names = TRUE)
  if (length(keys) > 0) {
    rm(list = keys, envir = cache_env)
  }
  invisible(NULL)
}


test_that("get_land_area returns valid geometry", {
  bbox <- sf::st_bbox(c(xmin = -80, ymin = 25, xmax = -75, ymax = 30), crs = sf::st_crs(4326))
  result <- get_land_area(sf::st_as_sfc(bbox))

  expect_true(all(sf::st_is_valid(result)))
  expect_true(all(sf::st_geometry_type(result) %in% c("MULTIPOLYGON", "POLYGON")))
})


test_that("get_roads handles deterministic mocked responses", {
  local_mocked_bindings(
    retry_with_backoff = function(expr_fun, max_attempts = 3, base_delay = 0.5, max_delay = 4) {
      make_mock_roads()
    },
    .package = "sitetool"
  )

  roads <- get_roads(c(-85, 29, -82, 31))
  expect_s3_class(roads, "sf")
  expect_true(all(sf::st_is_valid(roads)))
  expect_true(all(sf::st_geometry_type(roads) %in% c("MULTILINESTRING", "LINESTRING")))

  local_mocked_bindings(
    retry_with_backoff = function(expr_fun, max_attempts = 3, base_delay = 0.5, max_delay = 4) {
      sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(4326)))
    },
    .package = "sitetool"
  )

  roads_empty <- get_roads(c(-85, 29, -82, 31))
  expect_equal(nrow(roads_empty), 0)

  local_mocked_bindings(
    retry_with_backoff = function(expr_fun, max_attempts = 3, base_delay = 0.5, max_delay = 4) {
      stop("mock roads error")
    },
    .package = "sitetool"
  )

  roads_error <- get_roads(c(-85, 29, -82, 31))
  expect_equal(nrow(roads_error), 0)
})


test_that("get_cities handles deterministic mocked responses", {
  local_mocked_bindings(
    retry_with_backoff = function(expr_fun, max_attempts = 3, base_delay = 0.5, max_delay = 4) {
      make_mock_cities(
        lon = c(-84.28, -84.25),
        lat = c(30.44, 30.46),
        name = c("Tallahassee", "Mock Town")
      )
    },
    .package = "sitetool"
  )

  cities <- get_cities(c(-85, 29, -82, 31))
  expect_s3_class(cities, "sf")
  expect_true(all(sf::st_geometry_type(cities) == "POINT"))
  expect_true("Tallahassee" %in% cities$name)

  local_mocked_bindings(
    retry_with_backoff = function(expr_fun, max_attempts = 3, base_delay = 0.5, max_delay = 4) {
      sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(4326)))
    },
    .package = "sitetool"
  )

  cities_empty <- get_cities(c(-85, 29, -82, 31))
  expect_equal(nrow(cities_empty), 0)

  local_mocked_bindings(
    retry_with_backoff = function(expr_fun, max_attempts = 3, base_delay = 0.5, max_delay = 4) {
      stop("mock cities error")
    },
    .package = "sitetool"
  )

  cities_error <- get_cities(c(-85, 29, -82, 31))
  expect_equal(nrow(cities_error), 0)
})


test_that("check_distance filters points correctly", {
  points <- sf::st_sfc(sf::st_point(c(-80, 25)), sf::st_point(c(-79, 26)), crs = 4326)
  shape <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(-79, 26)), crs = 4326))
  filtered <- check_distance(points, shape, distance = 1)

  expect_equal(length(filtered), 1)
})


test_that("get_random_points generates the correct number of points", {
  clear_osm_cache()
  bbox <- c(-85, 29, -82, 31)

  set.seed(123)
  points <- get_random_points(bbox, n_points = 10)
  expect_equal(nrow(points), 10)

  local_mocked_bindings(
    get_roads = function(bbox, crs = 4326, in_app = FALSE) {
      sf::st_sf(
        highway = "primary",
        geometry = sf::st_sfc(
          sf::st_linestring(matrix(c(-84.5, 40.0, -83.5, 40.5), ncol = 2, byrow = TRUE)),
          crs = crs
        )
      )
    },
    get_cities = function(bbox, crs = 4326, in_app = FALSE) {
      make_mock_cities(lon = -84.2, lat = 40.2, name = "Far Away City", crs = crs)
    },
    .package = "sitetool"
  )

  clear_osm_cache()
  set.seed(124)
  points_road <- get_random_points(bbox, road_dist = 10, n_points = 10)
  expect_equal(nrow(points_road), 10)

  clear_osm_cache()
  set.seed(125)
  points_both <- get_random_points(bbox, road_dist = 10, city_dist = 10, n_points = 10)
  expect_equal(nrow(points_both), 10)
})


test_that("get_random_points validates invalid area inputs", {
  bbox <- c(0, 0, 0, 0)
  expect_message(get_random_points(bbox, n_points = 10), "Area must be valid shape object.")

  invalid_area <- "aaaa"
  expect_message(get_random_points(invalid_area, n_points = 10), "Area must be shape object or bounding box.")

  points_zero <- get_random_points(invalid_area, n_points = 0)
  expect_equal(nrow(points_zero), 0)

  points_negative <- get_random_points(invalid_area, n_points = -5)
  expect_equal(nrow(points_negative), 0)
})


test_that("get_village_points works with shapefile using mocked city lookup", {
  coords <- matrix(c(
    -122.42, 37.74,
    -122.44, 37.77,
    -122.42, 37.79,
    -122.40, 37.77,
    -122.41, 37.74,
    -122.42, 37.74
  ), ncol = 2, byrow = TRUE)

  shape <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(coords))),
    crs = 4326
  )

  local_mocked_bindings(
    get_cities = function(bbox, crs = 4326, in_app = FALSE) {
      make_mock_cities(
        lon = c(-122.42, -73.99),
        lat = c(37.77, 40.75),
        name = c("San Francisco", "Outside City"),
        crs = crs
      )
    },
    .package = "sitetool"
  )

  points <- get_village_points(shape)
  expect_true("San Francisco" %in% points$site)
  expect_false("Outside City" %in% points$site)
})


test_that("get_points returns NULL for invalid geometry", {
  coords <- matrix(c(
    -122.42, 37.74,
    -122.44, 37.77,
    -122.40, 37.74,
    -122.42, 37.77,
    -122.42, 37.74
  ), ncol = 2, byrow = TRUE)

  invalid_shape <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(coords))),
    crs = 4326
  )

  set.seed(126)
  points <- get_random_points(invalid_shape, n_points = 10)
  expect_null(points)

  villages <- get_village_points(invalid_shape)
  expect_null(villages)
})


test_that("get_land_area works with multipolygon", {
  poly1 <- sf::st_polygon(list(rbind(
    c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
  )))
  poly2 <- sf::st_polygon(list(rbind(
    c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
  )))

  multipoly <- sf::st_multipolygon(list(poly1, poly2))
  shape <- sf::st_sfc(multipoly, crs = 4326)

  result <- get_land_area(shape)

  expect_s3_class(result, "sfc")
  expect_true(all(sf::st_is_valid(result)))
  expect_true(length(result) > 0)
})


test_that("get_random_points works with multipolygon", {
  poly1 <- sf::st_polygon(list(rbind(
    c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
  )))
  poly2 <- sf::st_polygon(list(rbind(
    c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
  )))

  multipoly <- sf::st_multipolygon(list(poly1, poly2))
  shape <- sf::st_sfc(multipoly, crs = 4326)

  set.seed(127)
  result <- get_random_points(shape, n_points = 10)
  expect_equal(nrow(result), 10)
})


test_that("get_village_points works with multipolygon", {
  poly1 <- sf::st_polygon(list(rbind(
    c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
  )))
  poly2 <- sf::st_polygon(list(rbind(
    c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
  )))

  multipoly <- sf::st_multipolygon(list(poly1, poly2))
  shape <- sf::st_sfc(multipoly, crs = 4326)

  local_mocked_bindings(
    get_cities = function(bbox, crs = 4326, in_app = FALSE) {
      make_mock_cities(
        lon = c(-119.5, -117.5, -80),
        lat = c(35.5, 35.5, 10),
        name = c("Reward", "Baker", "Outside"),
        crs = crs
      )
    },
    .package = "sitetool"
  )

  villages <- get_village_points(shape)
  expect_true("Reward" %in% villages$site)
  expect_true("Baker" %in% villages$site)
  expect_false("Outside" %in% villages$site)
})


test_that("get_points correctly spreads by distance", {
  bbox <- c(-85, 29, -82, 31)

  set.seed(128)
  result <- get_random_points(bbox, n_points = 40, min_dist = 2000)
  dists <- sf::st_distance(result)

  dist_matrix <- as.matrix(dists)
  dist_matrix[lower.tri(dist_matrix, diag = TRUE)] <- NA

  expect_true(all(dist_matrix[!is.na(dist_matrix)] >= units::set_units(2000, "m")))
})


test_that("get_points deals with too large min_dist", {
  bbox <- c(-85, 29, -82, 31)

  expect_error(
    get_random_points(bbox, n_points = 10, min_dist = 10000000),
    regexp = "Cannot fit.*points.*min_dist.*"
  )
})


test_that("get_village_points handles multirow multipolygons", {
  poly1a <- sf::st_polygon(list(rbind(
    c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
  )))
  poly1b <- sf::st_polygon(list(rbind(
    c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
  )))
  multipoly1 <- sf::st_multipolygon(list(poly1a, poly1b))

  poly2a <- sf::st_polygon(list(rbind(
    c(-116, 35), c(-116, 36), c(-115, 36), c(-115, 35), c(-116, 35)
  )))
  poly2b <- sf::st_polygon(list(rbind(
    c(-114, 35), c(-114, 36), c(-113, 36), c(-113, 35), c(-114, 35)
  )))
  multipoly2 <- sf::st_multipolygon(list(poly2a, poly2b))

  shape <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(multipoly1, multipoly2, crs = 4326)
  )

  local_mocked_bindings(
    get_cities = function(bbox, crs = 4326, in_app = FALSE) {
      make_mock_cities(
        lon = c(-119.5, -115.5, -90),
        lat = c(35.5, 35.5, 35.5),
        name = c("Baker", "Calico", "Outside"),
        crs = crs
      )
    },
    .package = "sitetool"
  )

  result <- get_village_points(shape)
  expect_true("Baker" %in% result$site)
  expect_true("Calico" %in% result$site)
  expect_false("Outside" %in% result$site)
})


test_that("get_village_points handles empty and invalid inputs", {
  bbox <- c(-85, 29, -82, 31)

  local_mocked_bindings(
    get_cities = function(bbox, crs = 4326, in_app = FALSE) {
      sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(crs)))
    },
    .package = "sitetool"
  )

  villages <- get_village_points(bbox)
  expect_equal(nrow(villages), 0)

  invalid <- get_village_points("hey")
  expect_null(invalid)
})
