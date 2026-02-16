

reset_map <- function(map, draw){
  if(draw == 'TRUE'){
    map <- map%>%
      leaflet::clearShapes() %>%
      leaflet::clearGroup("DrawnROI") %>%
      leaflet::clearMarkers()%>%
      leaflet::clearControls()%>%
      leaflet::addMeasure() %>%
      leaflet.extras::addDrawToolbar(targetGroup = "DrawnROI",
                                     rectangleOptions = leaflet.extras::drawRectangleOptions(
                                       shapeOptions = leaflet.extras::drawShapeOptions(fill = FALSE, fillOpacity = 0)
                                     ),
                                     polylineOptions = FALSE,
                                     circleOptions = FALSE,
                                     markerOptions = FALSE,
                                     circleMarkerOptions = FALSE,
                                     singleFeature = TRUE,
                                     polygonOptions = FALSE) %>%
      leaflet::addScaleBar(position = "topleft")
  }
  else{
    map <- map%>%
      leaflet.extras::removeDrawToolbar(clearFeatures=TRUE)%>%
      leaflet::clearShapes()%>%
      leaflet::clearGroup("DrawnROI") %>%
      leaflet::clearMarkers()%>%
      leaflet::clearControls()%>%
      leaflet::addScaleBar(position = "topleft") %>%
      leaflet::addMeasure()
  }

}


numeric_raster_summary <- function(r, sample_size = 10000) {
  if (is.null(r) || terra::ncell(r) == 0) {
    return(NULL)
  }

  r_single <- r[[1]]

  stats <- tryCatch(
    terra::global(r_single, c("min", "max"), na.rm = TRUE),
    error = function(e) NULL
  )

  if (is.null(stats) || nrow(stats) == 0) {
    return(NULL)
  }

  min_val <- suppressWarnings(as.numeric(stats$min[1]))
  max_val <- suppressWarnings(as.numeric(stats$max[1]))

  if (!is.finite(min_val) || !is.finite(max_val)) {
    return(NULL)
  }

  n_target <- max(1, min(as.integer(sample_size), terra::ncell(r_single)))

  vals_sample <- tryCatch({
    sampled <- terra::spatSample(
      r_single,
      size = n_target,
      method = "random",
      na.rm = TRUE,
      values = TRUE,
      as.df = TRUE
    )
    as.numeric(sampled[[1]])
  }, error = function(e) numeric(0))

  vals_sample <- vals_sample[is.finite(vals_sample)]

  if (length(vals_sample) == 0) {
    vals_sample <- c(min_val, max_val)
  }

  if (min_val == max_val) {
    vals_sample <- c(vals_sample[1], vals_sample[1])
    min_val <- min_val - 0.5
    max_val <- max_val + 0.5
  }

  list(min_val = min_val, max_val = max_val, vals_sample = vals_sample)
}


sync_draw_toolbar <- function(map, draw = FALSE, clear_features = FALSE) {
  if (isTRUE(clear_features) || !isTRUE(draw)) {
    map <- map %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = clear_features)
  }

  if (isTRUE(draw)) {
    map <- map %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "DrawnROI",
        rectangleOptions = leaflet.extras::drawRectangleOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(fill = FALSE, fillOpacity = 0)
        ),
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        singleFeature = TRUE,
        polygonOptions = FALSE
      )
  }

  map
}



map_points <- function(map, sites) {
  map <- map %>%
    leaflet::clearGroup("Sites") %>%
    leaflet::removeControl(layerId = "sites_legend")

  if (is.null(sites) || nrow(sites) == 0) {
    return(map)
  }

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
      fillOpacity = 0.8,
      group = "Sites",
      layerId = sites$site_id
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      colors = c("red", "blue"),
      labels = c("Selected Sites", "Generated Sites"),
      layerId = "sites_legend"
    )
}


draw_sf <- function(map, sf_obj, draw=F, zoom_box=FALSE, fill_opacity = 0.2) {
  if (is.null(sf_obj)) {
    return(map %>% leaflet::clearGroup("ROI"))
  }

  bbx <- sf::st_bbox(sf_obj)

  map <- map %>%
    leaflet::clearGroup("ROI") %>%
    leaflet::addPolygons(data = sf_obj, color = "#5365FF", fillOpacity = fill_opacity, group = "ROI")

  if (zoom_box) {
    map <- map %>%
      leaflet::fitBounds(
        lng1 = bbx[[1]], lat1 = bbx[[2]],
        lng2 = bbx[[3]], lat2 = bbx[[4]]
      )
  }

  map
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
      leaflet::addRasterImage(
        r_plot,
        opacity = 0.8,
        options = leaflet::tileOptions(zIndex = 10)
      ) %>%
      leaflet::addRasterLegend(r, opacity = 0.8)
  }
  else{
    summary_stats <- numeric_raster_summary(r_plot)
    if (is.null(summary_stats)) {
      return(map)
    }

    min_val <- summary_stats$min_val
    max_val <- summary_stats$max_val
    colors <- rev(hcl.colors(10, palette = "Terrain"))

    map <- map %>%
      leaflet::addRasterImage(
        r_plot,
        colors = leaflet::colorNumeric(
          palette = colors,
          domain = c(min_val, max_val)
        ),
        opacity = 0.8,
        group = group,
        options = leaflet::tileOptions(zIndex = 10)
      )

  }
}

raster_legend_id <- function(name) {
  safe_name <- gsub("[^A-Za-z0-9_]", "_", as.character(name))
  paste0("raster_legend_", safe_name)
}


clear_raster_stack <- function(map, raster_names = character(0)) {
  map <- map %>%
    leaflet::clearImages() %>%
    leaflet::removeLayersControl()

  if (length(raster_names) > 0) {
    for (name in raster_names) {
      map <- map %>%
        leaflet::clearGroup(name) %>%
        leaflet::removeControl(layerId = raster_legend_id(name))
    }
  }

  map
}


add_raster_stack <- function(map, rasters, max_raster_size = 5e7) {
  if (is.null(rasters) || length(rasters) == 0) {
    return(map)
  }

  if (is.null(names(rasters)) || any(!nzchar(names(rasters)))) {
    names(rasters) <- paste0("Raster ", seq_along(rasters))
  }

  for (name in names(rasters)) {
    r <- rasters[[name]]

    # Downscale if too large
    if (terra::ncell(r) * 8 > max_raster_size) {
      factor <- ceiling(sqrt((terra::ncell(r) * 8) / max_raster_size))
      r <- terra::aggregate(r, fact = factor, fun = "modal", na.rm = TRUE)
    }

    coltab <- terra::coltab(r)[[1]]
    if (!is.null(coltab)) {
      # Construct hex colors from RGB columns
      coltab <- coltab[!(coltab$red == 0 & coltab$green == 0 & coltab$blue == 0), ]
      colors <- grDevices::rgb(coltab$red, coltab$green, coltab$blue, maxColorValue = 255)
      values <- if ("value" %in% names(coltab)) coltab$value else coltab[[1]]
      labels <- NULL
      if (!is.null(terra::levels(r)[[1]]) && all(c("code", "cover") %in% names(terra::levels(r)[[1]]))) {
        lvl <- terra::levels(r)[[1]]
        labels <- lvl$cover[match(values, lvl$code)]
      }
      if (is.null(labels) || anyNA(labels)) {
        labels <- as.character(values)
      }

      pal <- leaflet::colorFactor(palette = colors, domain = labels, levels = labels)

      # Add raster and legend
        map <- map %>%
          leaflet::addRasterImage(
            r,
            colors = colors,
            opacity = 0.8,
            group = name,
            options = leaflet::tileOptions(zIndex = 10)
          ) %>%
          leaflegend::addLegendFactor(
            pal = pal,
            values = factor(labels, levels = labels),
            group = name,
            title = name,
            position = "bottomright",
            orientation = "horizontal",
            width = 5,
            height = 5,
            layerId = raster_legend_id(name)
          )

    } else {
      # Continuous raster
      summary_stats <- numeric_raster_summary(r)
      if (is.null(summary_stats)) {
        next
      }

      vals_sample <- summary_stats$vals_sample
      min_val <- summary_stats$min_val
      max_val <- summary_stats$max_val

      pal <- leaflet::colorNumeric(palette = rev(hcl.colors(10, "Terrain")),
                                   domain = c(min_val, max_val))

        map <- map %>%
          leaflet::addRasterImage(
            r,
            colors = pal,
            opacity = 0.8,
            group = name,
            options = leaflet::tileOptions(zIndex = 10)
          ) %>%
          leaflegend::addLegendNumeric(
            pal = pal,
            values = vals_sample,
            title = name,
            group = name,
            position = "bottomleft",
            orientation = "horizontal",
            width = 150,
            height = 20,
            layerId = raster_legend_id(name)
          )
    }
  }

  # Layer toggle
  map <- map %>%
    leaflet::addLayersControl(
      baseGroups = names(rasters),
      options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
    )

  map
}
