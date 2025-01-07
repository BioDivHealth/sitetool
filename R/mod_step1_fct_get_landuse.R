#' @title Download land use data from Copernicus Global Land Service
#' @description
#' Downloads 100 m resolution landcover data from the Copernicus Global Land
#' Service e.g. (https://zenodo.org/records/4723921)
#'
#' @param shape sf. sf object containing the area of interest
#' @param async boolean whether in an interactive format or n
#' @return a SpatRaster of landuse for the shape area.
#' @author Simon Smart <simon.smart@@cantab.net>, modified by N. Imirzian
#' @export

get_landuse <- function(shape, async=FALSE) {
  if (!("sf" %in% class(shape))){
    message <- "Shape must be an sf object"
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

  #determine 20 degree tiles covering entire shape
  bbx <- sf::st_bbox(shape) / 20
  min_x_tile <- floor(bbx["xmin"]) * 20
  max_x_tile <- (ceiling(bbx["xmax"]) * 20) - 20
  min_y_tile <- (floor(bbx["ymin"]) * 20) + 20
  max_y_tile <- (ceiling(bbx["ymax"]) * 20)
  x_all <- seq(from = min_x_tile, to = max_x_tile, 20)
  y_all <- seq(from = min_y_tile, to = max_y_tile, 20)
  tiles <- expand.grid(x_all, y_all)
  colnames(tiles) <- c("x", "y")
  tiles$x_str <- formatC(tiles$x, width = 4, format = "d", flag = "0+")
  tiles$y_str <- formatC(tiles$y, width = 3, format = "d", flag = "0+")
  tiles$y_str <- gsub("-", "S", tiles$y_str)
  tiles$y_str <- gsub("\\+", "N", tiles$y_str)
  tiles$x_str <- gsub("-", "W", tiles$x_str)
  tiles$x_str <- gsub("\\+", "E", tiles$x_str)
  tiles$url <- paste0(tiles$x_str, tiles$y_str,"/", tiles$x_str, tiles$y_str)

  #check whether each tile contains part of the region of interest
  #needed in case of multiple countries / empty tiles
  valid_tiles <- NA

  for (r in 1:nrow(tiles)){
    t <- tiles[r,]

    tile_coords <- matrix(c(
      t$x, t$y - 20,
      t$x + 20, t$y - 20,
      t$x + 20, t$y,
      t$x, t$y,
      t$x, t$y - 20
    ), ncol = 2, byrow = TRUE)

    tile_polygon <- sf::st_polygon(list(tile_coords))
    tile_sf <- sf::st_sfc(tile_polygon, crs = 4326)
    tile_sf <- sf::st_sf(geometry = tile_sf)
    valid_tiles[r] <- sum(sf::st_intersects(shape, tile_sf) |> lengths())
  }

  tiles <- tiles[valid_tiles > 0,]


  tryCatch({
    raster_tiles <- NULL
    i = 1
    n_steps <- length(tiles) + 3

    for (t in tiles$url){
      incProgress(1/n_steps, detail = paste("Getting tiles:", i))
      url = glue::glue('https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2015/{t}_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif')
      ras <- terra::rast(url)
      if (is.null(raster_tiles)){
        raster_tiles <- ras

      } else {
        incProgress(1/n_steps, detail='Merging tiles')
        raster_tiles <- terra::merge(raster_tiles, ras)
      }
      i=i+1
    }
    #tile_name <- paste0(l," land use")
    incProgress(1/n_steps, detail='Croppng raster')
    raster_tiles <- terra::crop(raster_tiles, shape)
    #raster_tiles <- terra::aggregate(raster_tiles, fact = 10, fun = "mean")
    #raster_layers[[tile_name]] <- raster_tiles
    #  names(raster_layers[[tile_name]]) <- tile_name
    # }
  },
  error = function(x){
    message <- paste0("An error occurred while trying to download land use data: ", x)
    NULL},
  warning = function(x){
    message <- paste0("An error occurred while trying to download land use data: ", x)
    NULL}
  )
  return(raster_tiles)
}


