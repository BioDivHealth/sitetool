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


# In-memory cache for remote rasters (per R session)
.remote_raster_cache <- new.env(parent = emptyenv())
REMOTE_RASTER_CACHE_TTL_SEC <- 20 * 60
REMOTE_RASTER_CACHE_MAX_ENTRIES <- 6

normalize_raster_cache_bbox <- function(area, digits = 3) {
  bbox <- if (inherits(area, c("sf", "sfc"))) {
    sf::st_bbox(area)
  } else {
    area
  }

  bbox_vals <- if (!is.null(names(bbox)) && all(c("xmin", "ymin", "xmax", "ymax") %in% names(bbox))) {
    bbox[c("xmin", "ymin", "xmax", "ymax")]
  } else {
    bbox[seq_len(4)]
  }

  bbox_vals <- as.numeric(bbox_vals)
  bbox_vals <- round(bbox_vals, digits = digits)

  paste(formatC(bbox_vals, format = "f", digits = digits), collapse = "|")
}

make_remote_raster_cache_key <- function(product, area, digits = 3) {
  paste(product, normalize_raster_cache_bbox(area, digits = digits), sep = "::")
}

prune_remote_raster_cache <- function(max_entries = REMOTE_RASTER_CACHE_MAX_ENTRIES) {
  keys <- ls(envir = .remote_raster_cache, all.names = TRUE)
  if (length(keys) <= max_entries) {
    return(invisible(NULL))
  }

  access_time <- vapply(keys, function(k) {
    entry <- .remote_raster_cache[[k]]
    if (!is.null(entry$last_access)) {
      as.numeric(entry$last_access)
    } else {
      0
    }
  }, numeric(1))

  n_drop <- length(keys) - max_entries
  keys_to_drop <- names(sort(access_time))[seq_len(n_drop)]
  rm(list = keys_to_drop, envir = .remote_raster_cache)

  invisible(NULL)
}

get_remote_raster_cache <- function(key, ttl_sec = REMOTE_RASTER_CACHE_TTL_SEC) {
  entry <- .remote_raster_cache[[key]]
  if (is.null(entry)) {
    return(NULL)
  }

  if (is.null(entry$raster) || is.null(entry$cached_at) || !inherits(entry$raster, "SpatRaster")) {
    .remote_raster_cache[[key]] <- NULL
    return(NULL)
  }

  age_sec <- as.numeric(difftime(Sys.time(), entry$cached_at, units = "secs"))
  if (!is.finite(age_sec) || age_sec > ttl_sec) {
    .remote_raster_cache[[key]] <- NULL
    return(NULL)
  }

  entry$last_access <- Sys.time()
  .remote_raster_cache[[key]] <- entry

  entry$raster
}

set_remote_raster_cache <- function(key, raster, max_entries = REMOTE_RASTER_CACHE_MAX_ENTRIES) {
  if (!inherits(raster, "SpatRaster")) {
    return(invisible(NULL))
  }

  .remote_raster_cache[[key]] <- list(
    raster = raster,
    cached_at = Sys.time(),
    last_access = Sys.time()
  )

  prune_remote_raster_cache(max_entries = max_entries)
  invisible(NULL)
}


download_rast <- function(url, inapp = FALSE) {
  # GDAL may open https:// GeoTIFFs in streaming mode (slow / can appear to hang)
  # unless we explicitly use /vsicurl/ for random access.
  url_vsi <- url
  if (grepl("^https?://", url, ignore.case = TRUE) && !grepl("^/vsi", url, ignore.case = TRUE)) {
    url_vsi <- paste0("/vsicurl/", url)
  }

  r <- tryCatch({
    terra::rast(url_vsi)
  }, error = function(e) {
    if (inapp) {
      shiny::showNotification(
        paste("Failed to download raster:", basename(url)),
        type = "error"
      )
    }
    return(NULL)
  })

  return(r)
}


crop_rast <- function(rast, shape, inapp = FALSE) {
  cr <- tryCatch({
    terra::crop(rast, shape, snap='out')
  }, error = function(e) {
    if (inapp) {
      shiny::showNotification("Failed to crop raster.", type = "error")
    }
    return(NULL)
  })
  return(cr)
}

#' @title Download ESA WorldCover data
#' @description
#' Downloads 10 m resolution land cover data
#'
#' @param shape sf. sf object containing the area of interest
#' @param tile_limit int. max number of tiles to return.
#' @param coarse_res int. resolution to scale to.
#' @param inapp boolean. prints messages to shiny app if in app
#' @return a SpatRaster of landuse for the shape area.
#' @export

get_worldcover <- function(shape, tile_limit = 3,  coarse_res = 100, inapp=FALSE) {
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
          ras <- terra::aggregate(ras, fact = scale_factor, fun = 'modal', na.rm=T)
        }, silent = TRUE)
      }

      if (is.null(raster_tiles)) {
        raster_tiles <- ras
      } else {
        if (inapp) incProgress(1/n_steps, detail = "Merging tiles")
        tryCatch({
         # ras_aligned <- terra::resample(ras_agg, raster_tiles, method = "near")
          raster_tiles <- terra::merge(raster_tiles, ras, method = "near")
        }, error = function(e) {
          if (inapp) showNotification(paste("Merge failed for tile:", t), type = "error")
        })
      }
    }

    i <- i + 1
  }

  if(!is.null(raster_tiles)){
    color_table <- data.frame(
      code = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
      cover = c("Treecover", "Shrubland", "Grassland", "Cropland", "Builtup",
               "Bare/Sparse", "Snow/Ice", "Water", "Wetland", "Mangroves", "Moss/Lichen"),
      color = c("#006400", "#ffbb22", "#ffff4c", "#f096ff", "#fa0000",
                "#b4b4b4", "#f0f0f0", "#0064c8", "#0096a0", "#00cf75", "#fae6a0")
    )
  #  raster_tiles <- as.factor(raster_tiles)
    levels(raster_tiles) <- color_table%>%dplyr::select(code, cover)
    terra::coltab(raster_tiles) <- color_table%>%dplyr::select(code, color)
  }

  return(raster_tiles)
}

#' Download Elevation Raster
#'
#' Downloads and crops a global elevation raster (2.5 arc-min resolution) to the extent of an sf object.
#'
#' @param shape An `sf` object defining the area of interest.
#' @param inapp Logical. If `TRUE`, shows a notification in a Shiny app upon failure.
#'
#' @return A cropped elevation raster (`SpatRaster`).
#' @export
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


#' Download Human Footprint Raster
#'
#' Downloads and crops a global human footprint raster to the extent of an sf object.
#'
#' @param shape An `sf` object defining the area of interest.
#' @param year Numeric year for the footprint dataset (default = 2009).
#' @param inapp Logical. If `TRUE`, shows a notification in a Shiny app upon failure.
#'
#' @return A cropped footprint raster (`SpatRaster`).
#' @export
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
