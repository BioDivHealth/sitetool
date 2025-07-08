calculate_3x3_tiles <- function(shape) {
  if (!inherits(shape, "sf")) {
    warning("Input must be an sf object", call. = FALSE)
    return(NULL)
  }
  # Get bounding box
  bbx <- sf::st_bbox(shape)

  # Calculate tile bounds (each 3x3 degrees)
  min_lon <- floor(bbx["xmin"] / 3) * 3
  max_lon <- ceiling(bbx["xmax"] / 3) * 3 - 3
  min_lat <- floor(bbx["ymin"] / 3) * 3
  max_lat <- ceiling(bbx["ymax"] / 3) * 3 - 3

  # If max < min, it's a point or tiny feature — force it to cover at least one tile
  if (max_lon < min_lon) max_lon <- min_lon
  if (max_lat < min_lat) max_lat <- min_lat

  # Create tile grid
  lon_vals <- seq(min_lon, max_lon, by = 3)
  lat_vals <- seq(min_lat, max_lat, by = 3)

  tiles <- expand.grid(lon = lon_vals, lat = lat_vals)

  # Generate names like S48E036
  tile_names <- apply(tiles, 1, function(row) {
    lat <- row["lat"]
    lon <- row["lon"]

    lat_prefix <- if (lat < 0) "S" else "N"
    lon_prefix <- if (lon < 0) "W" else "E"

    lat_str <- sprintf("%02d", abs(lat))
    lon_str <- sprintf("%03d", abs(lon))

    paste0(lat_prefix, lat_str, lon_prefix, lon_str)
  })

  return(tile_names)
}


format_tile_name <- function(lat, lon, year=2019) {
  lat_prefix <- ifelse(lat >= 0, "N", "S")
  lon_prefix <- ifelse(lon >= 0, "E", "W")
  lat_str <- sprintf("%02d", abs(lat))
  lon_str <- sprintf("%03d", abs(lon))
  paste0("ESACCI-LC-L4-LCCS-Map-100m-P1Y-", year, "-v2.1.1_", lat_prefix, lat_str, lon_prefix, lon_str, ".tif")
}


download_rast <- function(url, inapp=F){
    r <- tryCatch({
      terra::rast(url)
    }, error = function(e) {
      if (inapp) shiny::showNotification(paste("Failed to download tile:", t), type = "error")
      return(NULL)
    })
    return(r)
}


crop_rast <- function(rast, shape, inapp=F){
  cr <- tryCatch({
    terra::crop(rast, shape)
  }, error = function(e) {
    if (inapp) shiny::showNotification(paste("Failed to crop tile:", t), type = "error")
    return(NULL)
  })
  return(cr)
}

#' @title Download ESA WorldCover data
#' @description
#' Downloads 10 m resolution land cover data
#'
#' @param shape sf. sf object containing the area of interest
#' @param inapp boolean. prints messages to shiny app if in app
#' @return a SpatRaster of landuse for the shape area.
#' @export

get_worldcover <- function(shape, tile_limit = 3, inapp = FALSE, coarse_res = 100) {
  tiles <- calculate_3x3_tiles(shape)

  raster_tiles <- NULL
  i <- 1
  n_steps <- length(tiles) + 3

  if (length(tiles) > tile_limit && inapp) {
    shiny::showNotification("Please select a smaller area or upload your own data.", type = "error")
    return(NULL)
  }

  for (t in tiles) {
    if (inapp) incProgress(1/n_steps, detail = paste("Getting tiles:", i))

    url <- paste0(
      "https://esa-worldcover.s3.eu-central-1.amazonaws.com/",
      "v200/2021/map/",
      "ESA_WorldCover_10m_2021_v200_",
      t,
      "_Map.tif"
    )

    ras <- download_rast(url, inapp)
    ras <- crop_rast(ras, shape, inapp)

    if (!is.null(ras)) {
      scale_factor <- round(coarse_res / 10)
      if(scale_factor > 1){
        try({
          # Aggregate using the modal function for categorical data.
          ras_agg <- terra::aggregate(ras, fact = scale_factor, fun = 'modal', na.rm=T)
        }, silent = TRUE)
      }


      if (is.null(raster_tiles)) {
        raster_tiles <- ras_agg
      } else {
        if (inapp) incProgress(1/n_steps, detail = "Merging tiles")
        tryCatch({
         # ras_aligned <- terra::resample(ras_agg, raster_tiles, method = "near")
          raster_tiles <- terra::merge(raster_tiles, ras_agg, method = "near")
        }, error = function(e) {
          if (inapp) showNotification(paste("Merge failed for tile:", t), type = "error")
        })
      }
    }

    i <- i + 1
  }

  if(!is.null(raster_tiles)){
    color_table <- data.frame(
      code = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
      cover = c("No Data", "Treecover", "Shrubland", "Grassland", "Cropland", "Builtup",
               "Bare/Sparse", "Snow/Ice", "Water", "Wetland", "Mangroves", "Moss/Lichen"),
      color = c("#000000", "#006400", "#ffbb22", "#ffff4c", "#f096ff", "#fa0000",
                "#b4b4b4", "#f0f0f0", "#0064c8", "#0096a0", "#00cf75", "#fae6a0")
    )
  #  raster_tiles <- as.factor(raster_tiles)
    levels(raster_tiles) <- color_table%>%dplyr::select(code, cover)
    terra::coltab(raster_tiles) <- color_table%>%dplyr::select(code, color)
  }

  return(raster_tiles)
}


download_elevation <- function(shape, inapp=F) {
  if (!inherits(shape, "sf")) {
    warning("Input must be an sf object", call. = FALSE)
    return(NULL)
  }

  # Download elevation raster for bbox at specified zoom
  tryCatch({
    elev_raster <- geodata::elevation_global(res = 2.5, path=tempdir())
  }, error = function(e){
    if(inapp){showNotification(paste('Failed to download elevation data.'), type='error')}
    return(NULL)
  })

  # Crop elevation raster to shape
  elev_cropped <- crop_rast(elev_raster, shape)

  return(elev_cropped)
}

download_footprint <- function(shape, year=2009, inapp=F) {
  if (!inherits(shape, "sf")) {
    warning("Input must be an sf object", call. = FALSE)
    return(NULL)
  }

  # Download elevation raster for bbox at specified zoom
  tryCatch({
    foot_raster <- geodata::footprint(year=year, path=tempdir())
  }, error = function(e){
    if(inapp){showNotification(paste('Failed to download elevation data.'), type='error')}
    return(NULL)
  })

  # Crop elevation raster to shape
  footprint <- crop_rast(foot_raster, shape)

  return(footprint)
}


