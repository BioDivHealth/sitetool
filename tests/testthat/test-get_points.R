
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
  testthat::skip_on_ci()
  # Basic case: return right number of points
  bbox <- c(-85, 29, -82, 31)
  points <- get_random_points(bbox, n_points = 10)
  expect_true(nrow(points) == 10)

  bbox <- c(-80, 25, -75, 30)
  points <- get_random_points(bbox, road_dist=10, n_points = 10)
  expect_true(nrow(points) == 10)

  bbox <- c(-80, 25, -75, 30)
  points <- get_random_points(bbox, road_dist=10, city_dist=10, n_points = 10)
  expect_true(nrow(points) == 10)

  # Return no points
  bbox <- c(0, 0, 0, 0)
  expect_message(get_random_points(bbox, n_points = 10), "Area must be valid shape object.")

  bbox <- 'aaaa'
  expect_message(get_random_points(bbox, n_points = 10), "Area must be shape object or bounding box.")

  points <- get_random_points(bbox, n_points = 0)
  expect_true(nrow(points) == 0)

  points <- get_random_points(bbox, n_points = -5)
  expect_true(nrow(points) == 0)
})

test_that("get_points works with shapefile", {
  testthat::skip_if_offline()
  testthat::skip_on_ci()
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


test_that("get_land_area works with multipolygon", {
  poly1 <- sf::st_polygon(list(rbind(
    c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
  )))
  poly2 <- sf::st_polygon(list(rbind(
    c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
  )))

  # Combine into MULTIPOLYGON
  multipoly <- sf::st_multipolygon(list(poly1, poly2))
  shape <- sf::st_sfc(multipoly, crs = 4326)

  # Run the function
  result <- get_land_area(shape)

  # Check result is valid and of correct type
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

  # Combine into MULTIPOLYGON
  multipoly <- sf::st_multipolygon(list(poly1, poly2))
  shape <- sf::st_sfc(multipoly, crs = 4326)

  # Run the function
  result <- get_random_points(shape, n_points=10)
  expect_true(nrow(result) == 10)
})

test_that("get_village_points works with multipolygon", {
  testthat::skip_if_offline()
  poly1 <- sf::st_polygon(list(rbind(
    c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
  )))
  poly2 <- sf::st_polygon(list(rbind(
    c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
  )))

  # Combine into MULTIPOLYGON
  multipoly <- sf::st_multipolygon(list(poly1, poly2))
  shape <- sf::st_sfc(multipoly, crs = 4326)

  # Run the function
  villages <- get_village_points(shape)
  expect_true("Reward" %in% villages$site)
})

# test_that("get_points handles multiple-row MULTIPOLYGON sf objects", {
#   # First MULTIPOLYGON (two disjoint squares)
#   poly1a <- sf::st_polygon(list(rbind(
#     c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
#   )))
#   poly1b <- sf::st_polygon(list(rbind(
#     c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
#   )))
#   multipoly1 <- sf::st_multipolygon(list(poly1a, poly1b))
#
#   # Second MULTIPOLYGON (another disjoint set)
#   poly2a <- sf::st_polygon(list(rbind(
#     c(-116, 35), c(-116, 36), c(-115, 36), c(-115, 35), c(-116, 35)
#   )))
#   poly2b <- sf::st_polygon(list(rbind(
#     c(-114, 35), c(-114, 36), c(-113, 36), c(-113, 35), c(-114, 35)
#   )))
#   multipoly2 <- sf::st_multipolygon(list(poly2a, poly2b))
#
#   # Combine into a 2-row sf object
#   shape <- sf::st_sf(
#     id = 1:2,
#     geometry = sf::st_sfc(multipoly1, multipoly2, crs = 4326)
#   )
#
#   # Run function
#   result <- get_random_points(shape, 10)
#   expect_true(nrow(result) == 10)
#
# })

test_that("get_points correctly spreads by distance", {
  testthat::skip_if_offline()

  bbox <- c(-85, 29, -82, 31)
  # Run function
  result <- get_random_points(bbox, n_points=100, min_dist=2000)
  dists <- sf::st_distance(result)

  dist_matrix <- as.matrix(dists)
  dist_matrix[lower.tri(dist_matrix, diag = TRUE)] <- NA

  # Assert that all non-NA distances are >= min_dist (allowing small tolerance)
  expect_true(all(dist_matrix[!is.na(dist_matrix)] >= units::set_units(2000, "m")))

})

test_that("get_points deals with too large min_dist", {
  testthat::skip_if_offline()
  # Basic case: return right number of points
  bbox <- c(-85, 29, -82, 31)
  expect_error(
    get_random_points(bbox, n_points = 10, min_dist=10000000),
    regexp = "Cannot fit.*points.*min_dist.*"
  )
})



test_that("get_village_points handles multiple-row MULTIPOLYGON sf objects", {
  testthat::skip_if_offline()
  testthat::skip_on_ci()
  # First MULTIPOLYGON (two disjoint squares)
  poly1a <- sf::st_polygon(list(rbind(
    c(-120, 35), c(-120, 36), c(-119, 36), c(-119, 35), c(-120, 35)
  )))
  poly1b <- sf::st_polygon(list(rbind(
    c(-118, 35), c(-118, 36), c(-117, 36), c(-117, 35), c(-118, 35)
  )))
  multipoly1 <- sf::st_multipolygon(list(poly1a, poly1b))

  # Second MULTIPOLYGON (another disjoint set)
  poly2a <- sf::st_polygon(list(rbind(
    c(-116, 35), c(-116, 36), c(-115, 36), c(-115, 35), c(-116, 35)
  )))
  poly2b <- sf::st_polygon(list(rbind(
    c(-114, 35), c(-114, 36), c(-113, 36), c(-113, 35), c(-114, 35)
  )))
  multipoly2 <- sf::st_multipolygon(list(poly2a, poly2b))

  # Combine into a 2-row sf object
  shape <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(multipoly1, multipoly2, crs = 4326)
  )

  # Run function
  result <- get_village_points(shape)
  expect_true("Baker" %in% result$site)

})

test_that("get_village_points filters villages correctly", {
  testthat::skip_if_offline()
  bbox <- c(-85, 29, -82, 31)
  villages <- get_village_points(bbox)
  expect_true("Tallahassee" %in% villages$site)

  bbox <- c(0,0,0,0)
  points <- get_village_points(bbox)
  expect_true(length(points) == 0)

  # test string input
  bbox <- 'hey'
  points <- get_village_points(bbox)
  expect_true(length(points) == 0)
})

