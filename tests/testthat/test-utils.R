# Test validate_text_input
test_that("validate_text_input handles valid and invalid inputs correctly", {
  # Valid input
  expect_equal(validate_text_input(123, inapp=F), 123)

  # Valid input
  expect_equal(validate_text_input('123', inapp=F), '123')

  # Non-numeric input
  expect_null(validate_text_input("abc", inapp=F))

  # Exceeds limit
  expect_null(validate_text_input("1000001", limit = 300, inapp=F))
})

# Test check_validity
test_that("check_validity correctly validates areas and bounding boxes", {
  # Valid bounding box
  valid_bbox <- c(-100, -50, 100, 50)
  expect_s3_class(check_validity(valid_bbox), "sfc")

  # Invalid bounding box values
  invalid_bbox <- 'aaa'
  expect_null(check_validity(invalid_bbox))
})

# Test check_bbox
test_that("check_bbox correctly identifies valid bounding boxes", {

  # works with named list
  valid_bbox <- list(xmin = -120, xmax = 120, ymin = -45, ymax = 45)
  invalid_bbox <- list(xmin = -200, xmax = 200, ymin = -95, ymax = 95)

  expect_true(check_bbox(valid_bbox))
  expect_false(check_bbox(invalid_bbox))

  # works with unnamed list
  valid_bbox <- c(-120, -45, 120, 45)
  invalid_bbox <-   c(-200, -95, 200, 95)

  expect_true(check_bbox(valid_bbox))
  expect_false(check_bbox(invalid_bbox))
})

# # Test fix_geometry
# test_that("fix_geometry fixes invalid geometries", {
#   # Invalid geometry
#   invalid_geometry <- sf::st_sfc(sf::st_polygon(list(matrix(c(
#     0, 0,   # Point 1
#     1, 1,   # Point 2
#     1, 0,   # Point 3
#     0, 1,   # Point 4
#     0, 0    # Closing Point (but self-intersects)
#   ), ncol = 2, byrow = TRUE))))
#
#   sf::st_crs(invalid_geometry) <- 4326  # Set coordinate reference system
#
#   fixed_geometry <- fix_geometry(invalid_geometry)
#   expect_s3_class(fixed_geometry, "sfc")
#   expect_true(sf::st_is_valid(fixed_geometry))
# })
