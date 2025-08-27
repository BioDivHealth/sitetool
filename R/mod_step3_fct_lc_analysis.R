
# Helper Functions ------------------------------------------------------------

# Creates a buffer around a point location
getCroppedArea <- function(place, dist = 1000, crs = 4326) {
  # Convert input to sf object
  place_sf <- sf::st_as_sf(place)

  # Get UTM zone from longitude
  lon <- sf::st_coordinates(place_sf)[1, 1]
  utm_zone <- floor((lon + 180) / 6) + 1
  utm_crs <- paste0("EPSG:", 32600 + utm_zone)

  # Project to UTM, buffer in meters, reproject to original CRS
  crop <- place_sf %>%
    sf::st_transform(utm_crs) %>%
    sf::st_buffer(dist) %>%
    sf::st_transform(crs)

  return(crop)
}

calculatePatchMetrics <- function(raster_crop, cell_areas) {
  # Convert raster to patches (connected areas of the same class)

  s = terra::segregate(raster_crop, keep=TRUE, other=NA)
  p = terra::patches(s, directions = 8)

  patch_areas = lapply(p, function(x){

    z = terra::zonal(cell_areas, x, sum)
    data.frame(layer = names(x),
               mean_patch_area = mean(z$area, na.rm=T)
    )
  })
  patch_areas = do.call(rbind, patch_areas)
  patch_areas = patch_areas%>%subset(layer != 'patches')
}


#' Extract raster statistics around site buffers
#'
#' @param in_df A data frame with a geometry column (sf object) and site metadata.
#' @param raster A SpatRaster (either categorical or continuous).
#' @param dist Radius (in CRS units) around each site to extract raster values.
#' @param progress Logical. Show progress if running inside a Shiny app.
#'
#' @return A long-format data frame with summary statistics or patch metrics.
#' @export

siteRasterStats <- function(in_df, raster, dist, progress = FALSE) {

  is_categorical <- terra::is.factor(raster)

  df_list <- lapply(seq_len(nrow(in_df)), function(i) {

    area <- getCroppedArea(in_df$geometry[i], dist)

    raster_crop <- terra::crop(raster, area)

    if (is_categorical) {
      # ----- Categorical: Patch metrics -----
      cell_areas <- terra::cellSize(raster_crop)
      class_tot <- terra::zonal(cell_areas, raster_crop, sum, na.rm = TRUE)

      if (nrow(class_tot) == 0) return(NULL)  # skip empty

      if (nrow(class_tot) < 2) {
        class_area <- data.frame(layer = class_tot$cover, mean_patch_area = class_tot$area)
      } else {
        class_area <- calculatePatchMetrics(raster_crop, cell_areas)
      }

      d_temp <- cbind(class_tot, class_area)
      d_temp$site <- in_df$site[i]
      d_temp$site_id <- in_df$site_id[i]
      d_temp$input_site <- in_df$input_site[i]
      return(d_temp)

    } else {
      # ----- Continuous: Global statistics -----
      d_temp <- terra::global(raster_crop, c('mean', 'sd', 'range'), na.rm = TRUE)
      d_temp$site <- in_df$site[i]
      d_temp$site_id <- as.character(in_df$site_id[i])
      d_temp$input_site <- in_df$input_site[i]
      return(d_temp)
    }

    if (progress) incProgress(1 / nrow(in_df), detail = paste("Site number:", i))
  })

  df <- do.call(rbind, df_list)

  if (is_categorical) {
    if ("layer" %in% colnames(df)) {
      df <- df %>%
        dplyr::select(-layer) %>%
        dplyr::group_by(site_id) %>%
        dplyr::mutate(total_area = sum(area)) %>%
        dplyr::group_by(site, site_id, input_site, cover) %>%
        dplyr::reframe(
          mean_patch_area = mean(mean_patch_area),
          cover_total_area = sum(area),
          proportion = sum(area) / total_area
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct() %>%
        tidyr::pivot_longer(c(cover_total_area, mean_patch_area, proportion),
                            names_to = 'measure', values_to = 'value')
    } else {
      df <- data.frame(site = character(0), site_id = character(0), cover = character(0),
                       measure = character(0), value = numeric(0))
    }

  } else {
    df$cover <- names(raster)
    df <- df %>%
      tidyr::pivot_longer(c(mean, sd, min, max),
                          names_to = 'measure', values_to = 'value')
  }

  return(df)
}



summarizeRaster <- function(r, product) {
  tryCatch({
    is_categorical <- terra::is.factor(raster)
    if(is_categorical){
      df <- terra::freq(r)  # Set useNA = "ifany" to include NAs

      # Compute total number of non-NA cells
      total_cells <- sum(df$count)

      colnames(df)[colnames(df) == "value"] <- "cover"

      # Add a proportion column
      df$value <- df$count / total_cells

    }
    else{
      mean_val = terra::global(r, fun = "mean", na.rm = TRUE)
      df = data.frame(cover = product,
                      value = mean_val$mean)
    }
    return(df)
  }, error = function(e){
    return(NULL)
  })
}

