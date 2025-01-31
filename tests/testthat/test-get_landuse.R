test_that("get_landuse handles invalid inputs", {
  # Test with non-sf input
  non_sf_input <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_message(get_landuse(non_sf_input), "Area must be shape object or bounding box.")
})

test_that("get_landuse processes valid sf input", {
  # Create a simple sf object for testing
  shape <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      matrix(c(-0.1, 51, -0.1, 51.1, 0.1, 51.1, 0.1, 51, -0.1, 51), ncol = 2, byrow = TRUE)
    )))
  ), crs = 4326)

  result <- get_landuse(shape)
  expect_s4_class(result, "SpatRaster")
})
