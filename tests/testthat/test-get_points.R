
test_that("get_land_area returns valid geometry", {
  bbox <- sf::st_bbox(c(xmin = -80, ymin = 25, xmax = -75, ymax = 30), crs = sf::st_crs(4326))
  result <- get_land_area(sf::st_as_sfc(bbox))
  expect_true(all(sf::st_is_valid(result)))
  expect_true(all(sf::st_geometry_type(result) %in% c("MULTIPOLYGON", "POLYGON")))

})

test_that("get_roads returns valid spatial object", {
  testthat::skip_if_offline()
  bbox <- c(-85, 29, -82, 31) # xmin, ymin, xmax, ymax
  roads <- get_roads(bbox)
  expect_true(all(sf::st_is_valid(roads)))
  expect_true(all(sf::st_geometry_type(roads) %in% c("MULTILINESTRING", "LINESTRING")))

  bbox <- c(0, 0, 0, 0)
  roads <- get_roads(bbox)
  expect_true(nrow(roads) == 0 || all(sf::st_is_empty(roads)))
})

test_that("get_cities returns valid spatial object", {
  testthat::skip_if_offline()
  bbox <- c(-85, 29, -82, 31)
  cities <- get_cities(bbox)
  expect_true(all(sf::st_geometry_type(cities) == "POINT"))
  expect_true('Tallahassee' %in% cities$name)

  bbox <- c(0, 0, 0, 0)
  cities <- get_cities(bbox)
  expect_true(nrow(cities) == 0 || all(sf::st_is_empty(cities)))

})

test_that("check_distance filters points correctly", {
  points <- sf::st_sfc(sf::st_point(c(-80, 25)), sf::st_point(c(-79, 26)), crs = 4326)
  sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(-79, 26)), crs = 4326))
  filtered <- check_distance(points, sf, distance = 1)
  expect_equal(length(filtered), 1) # No points within 1 degree
})

test_that("get_random_points generates the correct number of points", {
  testthat::skip_if_offline()
  # Basic case: return right number of points
  bbox <- c(-85, 29, -82, 31)
  points <- get_random_points(bbox, n_points = 10)
  expect_true(length(points) == 10)

  bbox <- c(-80, 25, -75, 30)
  points <- get_random_points(bbox, road_dist=10, n_points = 10)
  expect_true(length(points) == 10)

  bbox <- c(-80, 25, -75, 30)
  points <- get_random_points(bbox, road_dist=10, city_dist=10, n_points = 10)
  expect_true(length(points) == 10)

  # Return no points
  bbox <- c(0, 0, 0, 0)
  expect_message(get_random_points(bbox, n_points = 10), "Area must be valid shape object.")

  bbox <- 'aaaa'
  expect_message(get_random_points(bbox, n_points = 10), "Area must be shape object or bounding box.")
})

test_that("get_points works with shapefile", {
  testthat::skip_if_offline()
  coords <- matrix(c(
    -122.42, 37.74,  # Bottom-left
    -122.44, 37.77,  # Top-left
    -122.42, 37.79,  # Top-center
    -122.40, 37.77,  # Top-right
    -122.41, 37.74,  # Bottom-right
    -122.42, 37.74
  ), ncol = 2, byrow = TRUE)

  # Create an sf polygon
  shape <- sf::st_polygon(list(coords))

  # Convert the polygon to an sf object with a CRS
  shape <- sf::st_sf(
    geometry = sf::st_sfc(shape),
    crs = 4326 # CRS: WGS84 (latitude/longitude)
  )
#
#   points <- get_random_points(shape, n_points = 10)
#   expect_true(length(points) == 10)

  points <- get_village_points(shape)
  expect_true("San Francisco" %in% points$site)

  # test with invalid geometry
  coords <- matrix(c(
    -122.42, 37.74,  # Point 1
    -122.44, 37.77,  # Point 2
    -122.40, 37.74,  # Point 3
    -122.42, 37.77,  # Point 4
    -122.42, 37.74
  ), ncol = 2, byrow = TRUE)

  # Create a polygon with invalid geometry
  invalid_polygon <- sf::st_polygon(list(coords))

  # Convert the polygon to an sf object with a CRS
  shape <- sf::st_sf(
    geometry = sf::st_sfc(invalid_polygon),
    crs = 4326 # CRS: WGS84 (latitude/longitude)
  )
  points <- get_random_points(shape, n_points = 10)
  expect_true(length(points) == 0)

  points <- get_village_points(shape)
  expect_true(length(points) == 0)
})

test_that("get_village_points filters villages correctly", {
  testthat::skip_if_offline()
  bbox <- c(-85, 29, -82, 31)
  villages <- get_village_points(bbox)
  expect_true("Tallahassee" %in% villages$site)

  # villages <- get_village_points(bbox, max_pop = 500)
  # expect_false("Tallahassee" %in% villages$site)

  bbox <- c(0,0,0,0)
  points <- get_village_points(bbox)
  expect_true(length(points) == 0)

  # test string input
  bbox <- 'hey'
  points <- get_village_points(bbox)
  expect_true(length(points) == 0)
})


