

reset_map <- function(map, draw){
  if(draw == 'TRUE'){
    map <- map%>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers()%>%
      leaflet::clearControls()%>%
      leaflet.extras::addDrawToolbar(rectangleOptions = TRUE,
                                     polylineOptions = FALSE,
                                     circleOptions = FALSE,
                                     markerOptions = FALSE,
                                     circleMarkerOptions = FALSE,
                                     singleFeature = TRUE,
                                     polygonOptions = FALSE)
  }
  else{
    map <- map%>%
      leaflet.extras::removeDrawToolbar(clearFeatures=TRUE)%>%
      leaflet::clearShapes()%>%
      leaflet::clearMarkers()%>%
      leaflet::clearControls()
  }

}



map_points <- function(map, sites) {
  if (!is.null(sites) && nrow(sites) > 0) {
    coords <- sf::st_coordinates(sites)
    map %>%
      leaflet::addCircleMarkers(
        data = sites,
        lng = coords[, 1],
        lat = coords[, 2],
        popup = paste0(
          "<strong>Site: </strong>", sites$site,
          "<br><strong>LAT: </strong>", round(coords[, 2], 5),
          "<br><strong>LNG: </strong>", round(coords[, 1], 5)
        ),
        color = ifelse(sites$input_site, "red", "blue"),
        radius = 5,
        fillOpacity = 0.8
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        colors = c("red", "blue"),
        labels = c("Selected Sites", "Generated Sites")
      )
  } else {
    map
  }
}


draw_sf <- function(map, sf_obj, draw=F) {
  bbx = sf::st_bbox(sf_obj)

  map <- map %>%
    reset_map(draw=draw)%>%
    leaflet::addPolygons(data = sf_obj, color = "#5365FF", fillOpacity = 0.2) %>%
    leaflet::fitBounds(lng1 = bbx[[1]], lat1 = bbx[[2]],
                       lng2 = bbx[[3]], lat2 = bbx[[4]])
}


add_raster <- function(map, r){
  # Calculate the raster size in bytes3
  raster_size <- terra::ncell(r) * terra::nlyr(r) * 8
  r_plot = r
  # Check if raster size exceeds the maximum allowed size
  if (raster_size > max_raster_size) {
    # Calculate downscale factor to reduce raster size below the limit
    downscale_factor <- ceiling(sqrt(raster_size / max_raster_size))

    # Downscale raster by the calculated factor
    r_plot <- terra::aggregate(r_plot, fact = downscale_factor, fun = 'modal', na.rm = TRUE)

  }

  if (!is.null(terra::coltab(r)[[1]])) {

    map <- map %>%
      leaflet::clearImages() %>%  # Clear any previous raster
      leaflet::addRasterImage(r_plot, opacity = 0.8) %>%
      leaflet::addRasterLegend(r, opacity = 0.8)
  }
  else{
    min_val <- min(terra::values(r_plot), na.rm = TRUE)
    max_val <- max(terra::values(r_plot), na.rm = TRUE)
    colors <- rev(hcl.colors(10, palette = "Terrain"))

    map <- map%>%
      leaflet::addRasterImage(r_plot, colors = leaflet::colorNumeric(
        palette = colors,
        domain = c(min_val, max_val),
      ), opacity = 0.8)

  }
}

add_raster_stack <- function(map, r_stack, max_raster_size = 5e7) {
  stopifnot(inherits(r_stack, "SpatRaster"))

  # Loop through each layer in the stack
  for (i in seq_len(terra::nlyr(r_stack))) {
    r <- r_stack[[i]]
    layer_name <- names(r)

    # Calculate the raster size in bytes
    raster_size <- terra::ncell(r) * 8
    r_plot <- r

    # Downscale if needed
    if (raster_size > max_raster_size) {
      downscale_factor <- ceiling(sqrt(raster_size / max_raster_size))
      r_plot <- terra::aggregate(r_plot, fact = downscale_factor, fun = "modal", na.rm = TRUE)
    }

    # If categorical raster (has coltab)
    if (!is.null(terra::coltab(r)[[1]])) {
      map <- map %>%
        leaflet::addRasterImage(
          r_plot,
          opacity = 0.8,
          group = layer_name
        ) %>%
        leaflet::addRasterLegend(
          r,
          opacity = 0.8,
          group = layer_name
        )
    } else {
      # Continuous raster
      min_val <- min(terra::values(r_plot), na.rm = TRUE)
      max_val <- max(terra::values(r_plot), na.rm = TRUE)
      colors <- rev(hcl.colors(10, palette = "Terrain"))

      pal <- leaflet::colorNumeric(
        palette = colors,
        domain = c(min_val, max_val)
      )

      map <- map %>%
        leaflet::addRasterImage(
          r_plot,
          colors = pal,
          opacity = 0.8,
          group = layer_name
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = terra::values(r),
          opacity = 0.8,
          group = layer_name
        )
    }
  }

  # Add a layer control to toggle
  map <- map %>% leaflet::addLayersControl(
    baseGroups = names(r_stack),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

  return(map)
}

add_rasters_native <- function(map, rasters, max_raster_size = 5e7) {
  # rasters = named list of SpatRaster objects
  for (name in names(rasters)) {
    r <- rasters[[name]]

    # downscale if too large
    raster_size <- terra::ncell(r) * 8
    r_plot <- r
    if (raster_size > max_raster_size) {
      downscale_factor <- ceiling(sqrt(raster_size / max_raster_size))
      r_plot <- terra::aggregate(r_plot, fact = downscale_factor, fun = "modal", na.rm = TRUE)
    }

    # categorical vs continuous
    if (!is.null(terra::coltab(r)[[1]])) {
      map <- map %>%
        leaflet::addRasterImage(r_plot, opacity = 0.8, group = name) %>%
        leaflet::addRasterLegend(r, opacity = 0.8, group = name)
    } else {
      min_val <- min(terra::values(r_plot), na.rm = TRUE)
      max_val <- max(terra::values(r_plot), na.rm = TRUE)
      colors <- rev(hcl.colors(10, "Terrain"))
      pal <- leaflet::colorNumeric(colors, domain = c(min_val, max_val))

      map <- map %>%
        leaflet::addRasterImage(r_plot, colors = pal, opacity = 0.8, group = name) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = terra::values(r),
          opacity = 0.8,
          group = name,
          title = name
        )
    }
  }

  # toggle layers
  map <- map %>%
    leaflet::addLayersControl(
      baseGroups = names(rasters),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  return(map)
}
