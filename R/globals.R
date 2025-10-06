globalVariables(unique(c(
  # calculatePatchMetrics:
  "layer",
  # createLCDataFrame:
  "area", "cover", "cover_total_area", "mean_patch_area", "proportion", "site_id", "total_area",
  # generate_plot:
  "color", "group", "input_site", "site", "value",
  # get_cities:
  "cities",
  # get_land_area:
  "lakes_boundaries", "land_boundaries",
  # get_village_points:
  "geometry", "name", "osm_id", "population",
  # mod_step1_server : <anonymous>:
  "product", "raster_cats", "subcover", '.',
  # mod_step3_server : <anonymous>:
  "latitude", "longitude", "measure",
  "geom_type",
  "x_plot",
  "runif",
  "site_type",
  "x_plot_jitter",
  "y_plot_jitter",
  "x_start",
  "x_end",
  "value",
  "cover",
  "x_base",
  "renderLeaflet",
  "leaflet",
  "addTiles",
  "addCircleMarkers",
  "freq",
  "global",
  "type",
  # max raster size (changes for server vs local)
  max_raster_size <- 500 * 1024^2  # 4 MB or 500 MB depending on version
)))
