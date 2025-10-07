
test_that("Shapefile uploads correctly", {
  testServer(mod_step1_server, {
    skip_on_ci()
    address = test_path("test_sf.geojson")
    skip_if_not(file.exists(address), "Missing test shapefile")
    session$setInputs(shapefile = list(datapath = address))
    session$flushReact()

    # Call {testthat} functions
    expect_true(!is.null(mapvals$sf))
    expect_true("POLYGON" %in% sf::st_geometry_type(mapvals$sf))
  })
})


test_that("Invalid shapefile doesn't upload", {
  testServer(mod_step1_server, {
    skip_on_ci()
    address = test_path("invalid_geom.geojson")
    session$setInputs(shapefile = list(datapath = address))
    session$flushReact()

    expect_true(is.null(mapvals$sf))

    address = test_path("invalid_line.geojson")
    session$setInputs(shapefile = list(datapath = address))
    session$flushReact()

    expect_true(is.null(mapvals$sf))
  })
})
