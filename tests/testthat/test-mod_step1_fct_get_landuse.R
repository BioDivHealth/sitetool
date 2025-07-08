test_that("calculate_3x3_tiles returns correct tiles for simple square", {
  # Create a simple square polygon roughly from (1,1) to (4,4)
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(1,1), c(4,1), c(4,4), c(1,4), c(1,1)
  ))), crs = 4326)
  sf_poly <- sf::st_sf(geometry = poly)

  tiles <- calculate_3x3_tiles(sf_poly)
  print(tiles)
  # Should include tiles covering lon 0-3 and 3-6; lat 0-3 and 3-6 but limited by shape bbox
  expect_true(all(grepl("^N(00|03)", tiles)))

  # Expected longitudes can be 000 or 003
  expect_true(all(grepl("(E|W)(000|003)$", tiles)))

  expect_true(length(tiles) >= 1)
})

test_that("calculate_3x3_tiles returns multiple tiles for larger shape", {
  # Larger bbox to cover multiple 3x3 tiles
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-5,-5), c(10,-5), c(10,10), c(-5,10), c(-5,-5)
  ))), crs = 4326)
  sf_poly <- sf::st_sf(geometry = poly)

  tiles <- calculate_3x3_tiles(sf_poly)

  expect_gt(length(tiles), 1)  # Multiple tiles returned
  expect_true(all(grepl("N", tiles) | grepl("S", tiles)))
  expect_true(all(grepl("E", tiles) | grepl("W", tiles)))
})

test_that("calculate_3x3_tiles handles southern and western hemispheres", {
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-10,-10), c(-5,-10), c(-5,-5), c(-10,-5), c(-10,-10)
  ))), crs = 4326)
  sf_poly <- sf::st_sf(geometry = poly)

  tiles <- calculate_3x3_tiles(sf_poly)

  expect_true(all(grepl("^S", tiles)))  # Latitude prefix S for southern hemisphere
  expect_true(all(grepl("^.\\d{2}W", tiles)))   # Longitude prefix W for western hemisphere
})

test_that("calculate_3x3_tiles errors on non-sf input", {
  expect_warning(calculate_3x3_tiles("not an sf object"), "Input must be an sf object")
})

test_that("calculate_3x3_tiles handles single point shape", {
  point <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)
  sf_point <- sf::st_sf(geometry = point)

  tiles <- calculate_3x3_tiles(sf_point)

  expect_length(tiles, 1)
  expect_true(grepl("N00E000", tiles))
})

test_that("download_rast/crop_rast returns cropped raster", {
  coords <- matrix(
    c(
      30.5, 0.5,
      32.5, 0.5,
      32.5, 2.5,
      30.5, 2.5,
      30.5, 0.5
    ),
    ncol = 2,
    byrow = TRUE
  )

  # Create polygon and sf object
  poly <- sf::st_polygon(list(coords))
  shape <- sf::st_sfc(poly, crs = 4326)  # WGS84 coordinate system
  shape_sf <- sf::st_sf(geometry = shape)

  url = "https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/map/ESA_WorldCover_10m_2021_v200_N00E030_Map.tif"

  result <- download_rast(url)
  expect_s4_class(result, "SpatRaster")

  result <- crop_rast(result, shape_sf)
  expect_true(terra::relate(terra::ext(result), terra::ext(shape_sf), "intersects")[1])

})

# test_that("download_rast returns NULL on failed download", {
#   poly <- sf::st_sfc(sf::st_polygon(list(rbind(
#     c(1,1), c(4,1), c(4,4), c(1,4), c(1,1)
#   ))), crs = 4326)
#
#   expect_warning(download_rast("bad_url.tif", poly), "bad_url.tif: No such file or directory (GDAL error 4)")
# })

test_that("get_worldcover handles invalid inputs", {
  # Test with non-sf input
  non_sf_input <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_warning(get_worldcover(non_sf_input), "Input must be an sf object")
})

test_that("get_worldcover processes valid sf input", {
  # Create a simple sf object for testing
  shape <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      matrix(c(-0.1, 51, -0.1, 51.1, 0.1, 51.1, 0.1, 51, -0.1, 51), ncol = 2, byrow = TRUE)
    )))
  ), crs = 4326)

  result <- get_worldcover(shape)
  expect_s4_class(result, "SpatRaster")
})

test_that("get_worldcover returns coarser raster at expected resolution", {
  skip_on_cran()
  skip_if_offline()

  # Create a small test polygon in known tile
  shape <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(c(30.0, 0.0), c(30.1, 0.0), c(30.1, 0.1), c(30.0, 0.1), c(30.0, 0.0)))),
    crs = 4326
  ))

  # Run function with expected coarse resolution (assuming your function supports it)
  result <- get_worldcover(shape, tile_limit = 5, inapp = FALSE, coarse_res = 100)

  # Check output is not null
  expect_false(is.null(result), info = "Returned raster should not be NULL")

  # Check approximate resolution in degrees (100m ≈ 0.0009 degrees at equator)
  expected_deg <- 0.0009
  tolerance <- 0.0003
  res_vals <- terra::res(result)

  expect_true(
    all(abs(res_vals - expected_deg) < tolerance),
    info = paste("Expected resolution ~", expected_deg, "deg, got:", paste(res_vals, collapse = ", "))
  )
})


test_that("color table is assigned properly", {
  skip_on_cran()
  skip_if_offline()

  # Create a small test polygon in known tile
  shape <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(c(30.0, 0.0), c(30.1, 0.0), c(30.1, 0.1), c(30.0, 0.1), c(30.0, 0.0)))),
    crs = 4326
  ))

  # Run function with expected coarse resolution (assuming your function supports it)
  r <- get_worldcover(shape, tile_limit = 5, inapp = FALSE, coarse_res = 100)

  color_table <- data.frame(
    code = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
    name = c("No Data", "Treecover", "Shrubland", "Grassland", "Cropland", "Builtup",
             "Bare/Sparse", "Snow/Ice", "Water", "Wetland", "Mangroves", "Moss/Lichen"),
    color = c("#000000", "#006400", "#ffbb22", "#ffff4c", "#f096ff", "#fa0000",
              "#b4b4b4", "#f0f0f0", "#0064c8", "#0096a0", "#00cf75", "#fae6a0")
  )
  ct <- terra::coltab(r)[[1]]

  # 1. Check color table is not NULL
  expect_false(is.null(ct))

  # 2. Check all expected codes are present
  expect_true(all(color_table$code %in% ct$values))

  # 3.. Check levels are assigned
  lvls <- terra::levels(r)[[1]]
  expect_true(all(color_table$code %in% lvls$code))
  expect_equal(lvls$name[match(color_table$code, lvls$code)], color_table$name)
})

#
# test_that("get_worldcover returns NULL if tiles exceed limit", {
#   large_poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
#     c(0,0), c(20,0), c(20,20), c(0,20), c(0,0)
#   ))), crs = 4326))
#
#   # Should show notification and return NULL
#   result <- get_worldcover(large_poly, tile_limit = 1)
#   expect_null(result)
# })


# test_that("get_worldcover handles merge error gracefully", {
#   small_poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
#     c(0,0), c(3,0), c(3,3), c(0,3), c(0,0)
#   ))), crs = 4326))
#
#   dummy_rast <- terra::rast(matrix(1:4, 2, 2))
#
#   stub(get_worldcover, "calculate_3x3_tiles", function(shape) c("N00E000", "N03E000"))
#   stub(get_worldcover, "download_rast", function(url, shape) dummy_rast)
#
#   # Mock terra::merge to throw error on second merge
#   merge_call_count <- 0
#   stub(get_worldcover, "terra::merge", function(r1, r2) {
#     merge_call_count <<- merge_call_count + 1
#     if (merge_call_count == 2) stop("merge failed")
#     r1
#  })


test_that("download_elevation returns cropped elevation raster for valid shape", {
  skip_on_cran()
  skip_if_offline()

  poly <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(30, 1), c(31, 1), c(31, 2), c(30, 2), c(30, 1)
    ))),
    crs = 4326
  ))

  result <- download_elevation(poly)

  # Check it's a SpatRaster
  expect_s4_class(result, "SpatRaster")

  ext = terra::ext(result)
  expect_equal(unname(ext[1]), 30)

  # Check it overlaps with input shape
  expect_true(terra::relate(terra::ext(result), terra::ext(poly), "intersects")[1])
})
