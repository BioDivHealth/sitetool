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

test_that("createLCDataFrame works as expected", {

  # Create a mock categorical raster (e.g., landcover categories)
  # Assuming a 10x10 raster with 3 landcover classes
  r <- terra::rast(ncol = 10, nrow = 10, vals = sample(1:3, 100, replace = TRUE))

  categories <- data.frame(
    value = 1:3,      # Raster values (e.g., 1 = forest, 2 = water, 3 = urban)
    cover = c("forest", "water", "urban")  # Corresponding landcover names
  )

  # Set categories using the two-column data.frame
  levels(r) = categories

  # Create a mock data frame representing village sites
  in_df <- data.frame(
    site = c("village1", "village2"),
    site_id = c(1, 2),
    input_site = c(TRUE, FALSE)
  )

  in_df$geometry = sf::st_sfc(
    sf::st_point(c(1, 1)),
    sf::st_point(c(6, 6))
  )

  # Distance parameter for the radius
  dist <- 10  # 1 unit of distance for cropping

  # Run the function
  result <- createLCDataFrame(in_df, r, dist)

  # expect four columns
  expect_equal(length(result), 4)

  # Check that necessary columns exist
  expect_true("cover" %in% names(result))
  expect_true("site_id" %in% names(result))
  expect_true("measure" %in% names(result))
  expect_true("value" %in% names(result))

  # Check that the expected columns (mean_patch_area, cover_total_area) have numeric values
  expect_true(all(is.numeric(result$value)))

  # Check if the output has more than one row (i.e., some processing happened)
  expect_gt(nrow(result), 0)

  # Check that the `site_id` grouping works properly and the proportions are calculated
  expect_true(all(result$measure %in% c("cover_total_area", "mean_patch_area", "proportion")))

})
# Test continuous dataframe
test_that("createContDataFrame returns a dataframe", {
  result <- createContDataFrame(sample_sf, sample_raster, dist = 1000, progress = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true("measure" %in% colnames(result))
})
