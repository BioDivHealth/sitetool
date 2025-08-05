sample_point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
sample_sf <- sf::st_sf(site = "A", site_id = "A1", input_site = TRUE, geometry = sample_point)
sample_raster <- terra::rast(ncol = 10, nrow = 10, xmin = -1, xmax = 1, ymin = -1, ymax = 1)
terra::values(sample_raster) <- runif(100, 0, 1)

# Test getCroppedArea
test_that("getCroppedArea returns correct buffer", {
  buffered_area <- getCroppedArea(sample_sf$geometry, dist = 1000)
  expect_s3_class(buffered_area, "sf")
  expect_true(nrow(buffered_area) > 0)
})
#
# test_that("createLCDataFrame skips invalid areas", {
#
#   # Create a dummy raster
#   r <- terra::rast(ncol = 10, nrow = 10, vals = sample(1:3, 100, replace = TRUE))
#
#   # Set categories for raster
#   categories <- data.frame(
#     value = 1:3,      # Raster values (e.g., 1 = forest, 2 = water, 3 = urban)
#     cover = c("forest", "water", "urban")  # Corresponding landcover names
#   )
#   levels(r) = categories
#
#   # Create a mock data frame representing village sites
#   in_df <- data.frame(
#     site = c("village1"),
#     site_id = c(1),
#     input_site = c(TRUE)
#   )
#   in_df$geometry <- sf::st_sfc(
#     sf::st_point(c(2000, 2000))  # point outside of raster bounds
#   )
#
#   # Test the function
#   result_invalid_area <- createLCDataFrame(in_df, r, dist = 5, progress = FALSE)
#
#   # Ensure the result is empty since the site is outside of raster bounds
#   expect_equal(nrow(result_invalid_area), 0)
#
# })
#
# test_that("createLCDataFrame works with worldcover", {
#   skip_on_cran()
#   skip_if_offline()
#
#   # Create a small test polygon in known tile
#   shape <- sf::st_as_sf(sf::st_sfc(
#     sf::st_polygon(list(rbind(c(30.0, 0.0), c(30.1, 0.0), c(30.1, 0.1), c(30.0, 0.1), c(30.0, 0.0)))),
#     crs = 4326
#   ))
#
#   # Run function with expected coarse resolution (assuming your function supports it)
#   r <- get_worldcover(shape, tile_limit = 5, inapp = FALSE, coarse_res = 100)
#
#   point <- sf::st_sfc(sf::st_point(c(30.05, 0.05)), crs=4326)
#   sample_sf <- sf::st_sf(site = "A", site_id = "A1", input_site = TRUE, geometry = point)
#
#   out = createLCDataFrame(sample_sf, r, 500)
#
#   expect_true(all(out$cover %in% c('Builtup', 'Grassland', 'Treecover', 'Water', 'Wetland')))
#
# })
#
# # Test continuous dataframe
# test_that("createContDataFrame returns a dataframe", {
#   result <- createContDataFrame(sample_sf, sample_raster, dist = 1000, progress = FALSE)
#   expect_s3_class(result, "data.frame")
#   expect_true("measure" %in% colnames(result))
# })


test_that("summarizeRaster handles categorical raster correctly", {
  library(terra)
  # Create a categorical raster
  r <- terra::rast(nrows = 10, ncols = 10)

  # Assign random values from 1 to 3
  terra::values(r) <- sample(1:3, terra::ncell(r), replace = TRUE)

  # Convert to categorical (factor) raster
  r <- terra::as.factor(r)

  # Set factor levels (NOTE: must use a named list)
  levels(r) <- list(data.frame(ID = 1:3, label = c("forest", "water", "urban")))

  result <- summarizeRaster(r)

  # Check output is a data.frame
  expect_s3_class(result, "data.frame")

  # Check columns exist
  expect_true(all(c("cover", "count", "value") %in% colnames(result)))

  # Check proportions sum to 1
  expect_equal(sum(result$value), 1, tolerance=1e-6)

  # Check counts sum to total cells (non-NA)
  expect_equal(sum(result$count), sum(!is.na(values(r))))

  # Check 'cover' column matches factor levels labels
  expect_true(all(result$cover %in% c('forest', 'water', 'urban')))
})


test_that("summarizeRaster handles continuous raster correctly", {
  # Create continuous raster
  r <- terra::rast(nrows=10, ncols=10)
  terra::values(r) <- runif(terra::ncell(r), min=0, max=100)

  result <- summarizeRaster(r)

  # Check output is a data.frame
  expect_s3_class(result, "data.frame")

  # Check columns exist
  expect_true(all(c("cover", "value") %in% colnames(result)))

  # Check that cover is the raster's name (default NULL, so likely "")
  expect_equal(result$cover, names(r)[1])

  # Check value is numeric and equals the mean of raster values
  expect_equal(result$value, mean(values(r, na.rm=T), na.rm=TRUE))
})


test_that("summarizeRaster returns NULL for empty raster", {
  r <- terra::rast(nrows=10, ncols=10)
  terra::values(r) <- NA

  # For categorical raster with all NA
  r_cat <- suppressWarnings(terra::as.factor(r))
  levels(r_cat) <- data.frame(id=integer(0), label=character(0))

  expect_silent(result <- summarizeRaster(r_cat))
  # Should return a dataframe but likely empty
  expect_null(result)

  # For continuous raster with all NA
  expect_silent(result2 <- summarizeRaster(r))
  expect_true(is.data.frame(result2))
  expect_true(is.na(result2$value))
})
