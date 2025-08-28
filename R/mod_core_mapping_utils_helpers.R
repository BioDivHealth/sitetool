

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

add_raster_stack <- function(map, rasters, max_raster_size = 5e7) {
  for (name in names(rasters)) {
    r <- rasters[[name]]

    # Downscale if too large
    if (terra::ncell(r) * 8 > max_raster_size) {
      factor <- ceiling(sqrt((terra::ncell(r) * 8) / max_raster_size))
      r <- terra::aggregate(r, fact = factor, fun = "modal", na.rm = TRUE)
    }

    # EVERYTHING IS AN UGLY WORKAROUND :(
    # MUST FIX
    coltab <- terra::coltab(r)[[1]]
    if (!is.null(coltab)) {
      # Construct hex colors from RGB columns
      coltab <- coltab[!(coltab$red == 0 & coltab$green == 0 & coltab$blue == 0), ]
      colors <- rgb(coltab$red, coltab$green, coltab$blue, maxColorValue = 255)
      if (!is.null(terra::levels(r)[[1]]) && "code" %in% names(terra::levels(r)[[1]])) {
        lvl <- terra::levels(r)[[1]]
        labels <- lvl$cover[match(terra::coltab(r)[[1]]$value, lvl$code)]
      } else {
        labels <- as.character(coltab$value)
      }

      pal <- leaflet::colorFactor(palette = colors, domain = labels)

      # Add raster and legend
      map <- map%>%
        leaflet::addRasterImage(r, colors = colors, opacity = 0.8, group=name) %>%
        leaflegend::addLegendFactor(
          pal = pal,
          values = labels,
          group = name,
          title = name,
          position = "bottomleft",
          orientation = "horizontal",
          width = 5,
          height = 5
        )

    } else {
      # Continuous raster
      vals_sample <- terra::values(r, mat = FALSE, na.rm = TRUE)
      vals_sample <- sample(vals_sample, min(10000, length(vals_sample)))  # sample for speed

      min_val <- min(vals_sample)
      max_val <- max(vals_sample)
      if (length(unique(vals_sample)) == 1) {
        min_val <- unique(vals_sample) - 0.5
        max_val <- unique(vals_sample) + 0.5
      }

      pal <- leaflet::colorNumeric(palette = rev(hcl.colors(10, "Terrain")),
                                   domain = c(min_val, max_val))

      map <- map %>%
        leaflet::addRasterImage(r, colors = pal, opacity = 0.8, group = name) %>%
        leaflegend::addLegendNumeric(
          pal = pal,
          values = vals_sample,
          title = name,
          group = name,
          position = "bottomleft",
          orientation = "horizontal",
          width = 150,
          height = 20
        )
    }
  }

  # Layer toggle
  map <- map %>%
    leaflet::addLayersControl(
      baseGroups = names(rasters),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  map
}
