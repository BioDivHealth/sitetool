

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
    map <- map %>%
      leaflet::clearMarkers()%>%
      leaflet::clearControls()%>%
      leaflet::addCircleMarkers(data = sites,
                       lng = ~sf::st_coordinates(sites)[, 1],
                       lat = ~sf::st_coordinates(sites)[, 2],
                       popup = ~paste0("Site: ", site,
                                       "<br>LAT: ", sf::st_coordinates(geometry)[,2],
                                       "<br>LNG: ", sf::st_coordinates(geometry)[,1]),
                       color = ~ifelse(input_site, "red", "blue"),  # Change color if needed
                       radius = 5,
                       fillOpacity = 0.8)%>%
      leaflet::addLegend(
        position = "bottomright",
        colors = c("red", "blue"),
        labels = c("Input Sites", "Additional Sites")
      )
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
      ), opacity = 0.8) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = leaflet::colorNumeric(
          palette = colors,
          domain = c(min_val, max_val)
        ),
        values = terra::values(r),
        opacity = 0.8
      )

  }
}
