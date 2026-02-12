#' core_mapping UI Function
#'
#' @description Creates the app map.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_core_mapping_ui <- function(id) {
  ns <- NS(id)
   tagList(
    tags$head(
      tags$style(HTML("
        .leaflet-bottom.leaflet-left {
          bottom: 40px !important; /* shift legends up by 30px */
        }
      "))
    ),
    leaflet::leafletOutput(ns("map"), height = 500),
    tags$div(
      style = "display: flex; gap: 10px;",
      selectInput(
        ns("bmap"), "Background map:",
        choices = c(
          "Open Street Map" = "OpenStreetMap",
          "ESRI Imagery"= "Esri.WorldImagery",
          "Open Topo"   = "OpenTopoMap"
        ),
        selected = "OpenStreetMap"
      )
    )
  )
}


#' core_mapping Server Functions
#'
#' @noRd
mod_core_mapping_server <- function(id, common){
  moduleServer(id, function(input, output, session){
    map_ready <- shiny::reactiveVal(FALSE)
    raster_groups <- shiny::reactiveVal(character(0))
    draw_toolbar_loaded <- shiny::reactiveVal(TRUE)

    map_proxy <- function() {
      leaflet::leafletProxy("map", session = session)
    }

    # Render initial Leaflet map once
    output$map <- leaflet::renderLeaflet({
      initial_bmap <- isolate(input$bmap)
      if (is.null(initial_bmap) || !nzchar(initial_bmap)) {
        initial_bmap <- "OpenStreetMap"
      }

      leaflet::leaflet() %>%
        leaflet::setView(lng = 0, lat = 20, zoom = 2) %>%
        leaflet::addProviderTiles(initial_bmap, group = "BaseMap") %>%
        leaflet::addScaleBar(position = "bottomleft") %>%
        leaflet::addMeasure() %>%
        leaflet.extras::addDrawToolbar(
          rectangleOptions = TRUE,
          polylineOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          singleFeature = TRUE,
          polygonOptions = FALSE
        )
    })

    # Mark map as ready when widget is mounted
    observeEvent(input$map_zoom, {
      map_ready(TRUE)
    }, once = TRUE, ignoreNULL = TRUE)

    # Draw toolbar updates only
    observe({
      req(map_ready())

      draw_enabled <- isTRUE(common$draw)
      toolbar_loaded <- isolate(draw_toolbar_loaded())

      if (!draw_enabled && !toolbar_loaded) {
        return()
      }

      map_proxy() %>%
        sync_draw_toolbar(
          draw = draw_enabled,
          clear_features = !draw_enabled
        )

      draw_toolbar_loaded(draw_enabled)
    })

    # --- (1) Base map changes ---
    observeEvent(input$bmap, {
      req(map_ready(), input$bmap)

      map_proxy() %>%
        leaflet::clearGroup("BaseMap") %>%
        leaflet::addProviderTiles(input$bmap, group = "BaseMap")
    }, ignoreInit = TRUE)

    # --- (2) ROI updates ---
    observe({
      req(map_ready())

      sf_obj <- common$sf
      draw_enabled <- isTRUE(isolate(common$draw))

      if (is.null(sf_obj)) {
        map_proxy() %>% leaflet::clearGroup("ROI")
        return()
      }

      common$raster <- NULL

      proxy <- map_proxy()

      if (draw_enabled) {
        proxy <- proxy %>% leaflet::clearShapes()
      }

      proxy %>%
        draw_sf(sf_obj, zoom_box = TRUE)
    })

    # --- (3) Raster layer updates ---
    observe({
      req(map_ready())

      previous_groups <- isolate(raster_groups())
      raster_stack <- common$raster
      sf_obj <- isolate(common$sf)
      draw_enabled <- isTRUE(isolate(common$draw))

      proxy <- map_proxy() %>%
        clear_raster_stack(previous_groups)

      if (is.null(raster_stack) || length(raster_stack) == 0) {
        if (is.null(sf_obj)) {
          proxy <- proxy %>% leaflet::clearGroup("ROI")
        } else {
          proxy <- proxy %>% draw_sf(sf_obj, zoom_box = FALSE)
        }

        raster_groups(character(0))
        return()
      }

      if (is.null(names(raster_stack)) || any(!nzchar(names(raster_stack)))) {
        names(raster_stack) <- paste0("Raster ", seq_along(raster_stack))
      }

      proxy %>%
        sync_draw_toolbar(draw = draw_enabled, clear_features = TRUE) %>%
        leaflet::clearShapes() %>%
        leaflet::clearGroup("ROI") %>%
        add_raster_stack(raster_stack)

      raster_groups(names(raster_stack))
    })

    # --- (4) Sites added or updated ---
    observe({
      req(map_ready())

      map_proxy() %>%
        map_points(common$sites)
    })

    # --- (5) Bounding box drawn ---
    observeEvent(input$map_draw_new_feature, {
      req(map_ready())

      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE, FALSE)], coords[c(FALSE, TRUE)]), ncol = 2)
      colnames(xy) <- c("longitude", "latitude")

      # Normalize longitude if outside -180 to 180 range
      xy[, 1] <- ((xy[, 1] + 180) %% 360) - 180

      bbox <- c(min(xy[, 1]), min(xy[, 2]), max(xy[, 1]), max(xy[, 2]))

      if (check_bbox(bbox)) {
        bbox_sf <- sf::st_as_sfc(
          sf::st_bbox(
            c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]),
            crs = sf::st_crs(4326)
          )
        ) %>% sf::st_as_sf()
        bbox_sf <- fix_geometry(bbox_sf)

        if (!is.null(bbox_sf)) {
          common$sf <- bbox_sf
          common$bbox_drawn <- TRUE
        } else {
          map_proxy() %>%
            sync_draw_toolbar(draw = isTRUE(common$draw), clear_features = TRUE)
          showNotification("Error with selected area.", type = "error")
        }
      }
    })

    # Add new site on map click
    observeEvent(input$map_click, {
      req(common$add_points)
      click <- input$map_click
      req(click)

      # Create a new site data.frame with coordinates
      new_site <- data.frame(
        site = paste0("SelectedPoint_Lat_",round(click$lat, digits=4), "_Lon_", round(click$lng, digits=4)),
        site_id = paste0("select_", as.integer(Sys.time()), "_Lat_",round(click$lat, digits=4), "_Lon_", round(click$lng, digits=4)),
        input_site = TRUE,
        longitude = click$lng,
        latitude = click$lat,
        stringsAsFactors = FALSE
      )

      # Convert to sf point geometry
      new_site_sf <- sf::st_as_sf(new_site, coords = c("longitude", "latitude"), crs = 4326)

      # Check if inside bbox
      if (!is.null(common$sf)) {
        inside <- any(sf::st_within(new_site_sf, common$sf, sparse = FALSE))

        if (!inside) {
          showNotification("Clicked point is outside of the region of interest.", type = "warning")
          return(NULL) # skip adding point
        }
      }

      # Combine with existing sites
      if (is.null(common$sites) || nrow(common$sites) == 0) {
        common$sites <- new_site_sf
      } else {
        common$sites <- rbind(common$sites, new_site_sf)
      }
    })

    # Toggle site selection when marker clicked
    observeEvent(input$map_marker_click, {
      req(common$add_points)
      click_id <- input$map_marker_click$id
      req(click_id)

      isolate({
        sites_df <- common$sites
        idx <- which(sites_df$site_id == click_id)
        if (length(idx) == 1) {
          sites_df$input_site[idx] <- !sites_df$input_site[idx]
          common$sites <- sites_df
        }
      })
    })

  })
}



## To be copied in the UI
# mod_core_mapping_ui("core_mapping_1")

## To be copied in the server
# mod_core_mapping_server("core_mapping_1")
