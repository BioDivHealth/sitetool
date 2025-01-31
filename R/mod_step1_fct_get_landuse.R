#' @title Download land use data from Copernicus Global Land Service
#' @description
#' Downloads 100 m resolution landcover data from the Copernicus Global Land
#' Service e.g. (https://zenodo.org/records/4723921)
#'
#' @param shape sf. sf object containing the area of interest
#' @param inapp boolean. prints messages to shiny app if in app
#' @return a SpatRaster of landuse for the shape area.
#' @export

get_landuse <- function(shape, inapp=F) {

  if(is.null(check_validity(shape))){
    return(NULL)
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
    if(sf::st_is_valid(tile_sf)){
      valid_tiles[r] <- sum(sf::st_intersects(shape, tile_sf) |> lengths())
    }
  }

  tiles <- tiles[valid_tiles > 0,]

    raster_tiles <- NULL
    i = 1
    n_steps <- length(tiles) + 3

    if (length(tiles$url) > 1 & inapp) {
      shiny::showNotification("Please select a smaller area or upload your own data.", type = "error")
    }
    else{
        for (t in tiles$url){
          if(inapp){incProgress(1/n_steps, detail = paste("Getting tiles:", i))}
          url = glue::glue('https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2015/{t}_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif')

          tryCatch({
            ras <- terra::rast(url)
            ras <- terra::crop(ras, shape)
          }, error = function(e) {
            showNotification(e$message)
            return(NULL)
          })
          if (is.null(raster_tiles)){
            raster_tiles <- ras

          } else {
            if(inapp){incProgress(1/n_steps, detail='Merging tiles')}
            tryCatch({
              # Attempt to merge the raster tiles
              raster_tiles <- terra::merge(raster_tiles, ras)
            }, error = function(e) {
              return(NULL)
            })

          }
          i=i+1
        }
    }
  return(raster_tiles)
}


